{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Data.Maybe
import Scanner (scan, runScanner, toChar)
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Posix.Files
import System.Posix.Signals
import System.Process
import TreeSearch
import TreeView
import TreeViewRaw
import qualified Notmuch
import qualified Notmuch.Message as Notmuch
import qualified Notmuch.SearchResult as Notmuch
import qualified Data.Tree.Zipper as Z
import qualified Data.Tree as Tree
import qualified Data.Text as T


data State = State
    { charge :: IO ()
    , discharge :: IO ()
    , cursor :: Z.TreePos Z.Full TreeView
    , xoffset :: Int
    , yoffset :: Int
    , flashMessage :: String
    , screenWidth :: Int
    , screenHeight :: Int
    }


main :: IO ()
main = do
    setEnv "HOME" =<< getEnv "OLDHOME"

    q@State{..} <- initState
    bracket_ charge discharge $ do
        winchHandler
        run q


initState :: IO State
initState = do

    let query = "tag:inbox AND NOT tag:killed"

    r_ <- either error id <$> Notmuch.search query

    echo0 <- hGetEcho stdin
    buffering0 <- hGetBuffering stdin
    return State
        { charge = do
              _ <- installHandler 28 (Catch winchHandler) Nothing
              hSetEcho stdin False
              hSetBuffering stdin NoBuffering
              -- Save Cursor and use Alternate Screen Buffer
              hPutStr stdout "\ESC[?1049h"
              -- Hide Cursor
              hPutStr stdout "\ESC[?25l"
              -- Move the cursor to the home position
              hPutStr stdout "\ESC[H"
              hFlush stdout
        , discharge = do
              _ <- installHandler 28 Default Nothing
              hSetEcho stdin echo0
              hSetBuffering stdin buffering0
              -- Use Normal Screen Buffer and restore Cursor
              hPutStr stdout "\ESC[?1049l"
              hFlush stdout
        , cursor = Z.fromTree $ fromSearchResults query r_
        , xoffset = 0
        , yoffset = 0
        , flashMessage = "Welcome to much; quit with ^C"
        , screenWidth = 0
        , screenHeight = 0
        }


run :: State -> IO ()
run q = do

    redraw q

    _ <- hLookAhead stdin -- wait for input
    ((_, s), _) <- runScanner scan

    case keymap (map toChar s) of
        Just a ->
            a q >>= run
        Nothing ->
            run q { flashMessage = show $ map toChar s }


redraw :: State -> IO ()
redraw _q@State{..} = do

    --putStrLn $ describe (Z.label cursor)
    --putStr "\ESC[?2J"
    putStr "\ESC[H"
    --mapM_ putStr $ take screenHeight $ repeat "\ESC[2K\n"
    --putStr "\ESC[H"

    -- consumes 1 screenHeight
    putStr $ "\ESC[2K" ++ flashMessage ++ " " ++ show (screenWidth, screenHeight)


    let buf = map (take screenWidth . drop xoffset) $
              take (screenHeight - 1) $
              drop yoffset $
              renderTreeView (Z.label cursor) (Z.toTree cursor)

    mapM_ (putStr . ("\n\ESC[2K"++)) $
        buf
        ++
        take (screenHeight - 1 - length buf) (repeat "~")



winchHandler :: IO ()
winchHandler = do
    -- Report the size of the screen in characters.
    -- Result is CSI 9 ; height ; width t
    putStr "\ESC[19t"


keymap :: String -> Maybe (State -> IO State)

keymap "r" = Just replyToAll
keymap "e" = Just viewSource
keymap "k" = Just moveCursorUp
keymap "j" = Just moveCursorDown
keymap "K" = Just $ moveTreeDown 1
keymap "J" = Just $ moveTreeUp 1
keymap "\ESC[A" = Just moveCursorUp
keymap "\ESC[B" = Just moveCursorDown
keymap "\ESC[a" = Just $ moveTreeDown 1
keymap "\ESC[b" = Just $ moveTreeUp 1
keymap "\ESC[5~" = Just $ \q -> moveTreeDown (screenHeight q `div` 2) q   -- PgUp
keymap "\ESC[6~" = Just $ \q -> moveTreeUp (screenHeight q `div` 2) q -- PgDn
keymap "\n" = Just toggleFold

keymap ('\ESC':'[':'9':';':xs) = Just $ \q@State{..} -> do
    let (h,';':w) = break (==';') (take (length xs - 1) xs) -- ^ drop (assumed) trailing 't'
    return q { screenWidth = read w, screenHeight = read h }
keymap _ = Nothing


moveCursorDown q@State{..} =
    return q { cursor = fromMaybe (Z.root cursor) $ findNext cursor }

moveCursorUp q@State{..} =
    return q { cursor = fromMaybe (Z.root cursor) $ findPrev cursor }

moveTreeUp n q@State{..} =
    return q { yoffset = max 0 (yoffset + n) }

moveTreeDown n q@State{..} =
    return q { yoffset = max 0 (yoffset - n) }


toggleFold q@State{..} = case Z.label cursor of
    TVMessage m -> do
        toggleTag (T.pack "open") m

        let Just sr = findParent isTVSearchResult cursor
            TVSearchResult the_sr = Z.label sr
            Notmuch.ThreadID tid = Notmuch.searchThread the_sr

        t_ <- return . fromMessageForest =<< Notmuch.getThread tid

        let cursor' = Z.modifyTree (\(Tree.Node l _) -> Tree.Node l t_) sr
        return q { cursor = select (==Z.label cursor) cursor' }

    TVSearchResult sr -> do
        let open = not $ null $ Tree.subForest $ Z.tree cursor
        let Notmuch.ThreadID tid = Notmuch.searchThread sr

        t_ <-
            if open
                then return []
                else return . fromMessageForest =<< Notmuch.getThread tid

        let cursor' = Z.modifyTree (\(Tree.Node l _) -> Tree.Node l t_) cursor
        return q { cursor = select (==Z.label cursor) cursor' }

    _ ->
        return q { flashMessage = "nothing happened" }
  where
    select p loc = fromMaybe (error "cannot select") $ findTree p $ Z.root loc

    toggleTag :: T.Text -> Notmuch.Message -> IO ()
    toggleTag tag m = do
        _ <- if tag `elem` Notmuch.messageTags m
            then
                Notmuch.unsetTag tagString (Notmuch.unMessageID $ Notmuch.messageId m)
            else
                Notmuch.setTag tagString (Notmuch.unMessageID $ Notmuch.messageId m)
        return ()
      where
        tagString = T.unpack tag




replyToAll q@State{..} = case getMessage (Z.label cursor) of
    Nothing ->
        return q { flashMessage = "no message" }
    Just m -> do
        editor <- getEnv "EDITOR"
        logname <- getEnv "LOGNAME"
        tmpdir <- getTemporaryDirectory


        let template = logname ++ "_much_draft_.mail"

        let msgId = Notmuch.unMessageID $ Notmuch.messageId m

        withTempFile tmpdir template $ \(path, draftH) -> do
            (_, _, _, procH) <-
                withFile "/dev/null" ReadMode $ \nullH ->
                    createProcess
                        (proc "notmuch" [ "reply" , "id:" ++ msgId ])
                            { std_in = UseHandle nullH
                            , std_out = UseHandle draftH
                            }
            hClose draftH
            waitForProcess procH >>= \case
                ExitFailure code ->
                    putStrLn $ "notmuch exit code = " ++ show code
                ExitSuccess ->
                    finally (system $ editor ++ " " ++ path) charge >>= \case
                        ExitFailure code ->
                            putStrLn $ editor ++ " exit code = " ++ show code
                        ExitSuccess ->
                            return ()
        return q


viewSource q@State{..} = case getMessage (Z.label cursor) of
    Nothing ->
        return q { flashMessage = "no message" }
    Just m -> do
        editor <- getEnv "EDITOR"
        logname <- getEnv "LOGNAME"
        tmpdir <- getTemporaryDirectory

        let template = logname ++ "_much_raw_.mail"

        let msgId = Notmuch.unMessageID $ Notmuch.messageId m

        withTempFile tmpdir template $ \(path, draftH) -> do
            setFileMode path 0o400
            (_, _, _, procH) <-
                withFile "/dev/null" ReadMode $ \nullH ->
                    createProcess
                        (proc "notmuch" [ "show", "--format=raw", "id:" ++ msgId ])
                            { std_in = UseHandle nullH
                            , std_out = UseHandle draftH
                            }
            hClose draftH
            waitForProcess procH >>= \case
                ExitFailure code ->
                    putStrLn $ "notmuch exit code = " ++ show code
                ExitSuccess ->
                    finally (system $ editor ++ " " ++ path) charge >>= \case
                        ExitFailure code ->
                            putStrLn $ editor ++ " exit code = " ++ show code
                        ExitSuccess ->
                            return ()
        return q







withTempFile :: FilePath -> String -> ((FilePath, Handle) -> IO a) -> IO a
withTempFile tmpdir template f = do
    bracket (openTempFile tmpdir template) (removeFile . fst) f
