{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
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
    , headBuffer :: [String]
    , treeBuffer :: [String]
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
        , headBuffer = []
        , treeBuffer = []
        }


run :: State -> IO ()
run q0 = do
    let q = render q0

    redraw q

    _ <- hLookAhead stdin -- wait for input
    ((_, s), _) <- runScanner scan

    case keymap (map toChar s) of
        Just a ->
            a q >>= run
        Nothing ->
            run q { flashMessage = show $ map toChar s }


render :: State -> State
render q@State{..} =
    q { treeBuffer = newTreeBuf
      , headBuffer = newHeadBuf
      }
  where
    newTreeBuf = renderTreeView (Z.label cursor) (Z.toTree cursor)
    newHeadBuf =
        [ show screenWidth ++ "x" ++ show screenHeight
          --show (linearPos cursor + 1) ++ "/" ++ show (length uncut)
          ++ " " ++ show (linearPos cursor - yoffset)
          ++ " " ++ show (topOverrun q)
          ++ " " ++ show (botOverrun q)
          ++ " " ++ flashMessage
        , "--"
        , "--"
        , "--"
        ]



redraw :: State -> IO ()
redraw _q@State{..} = do

    let image =
            map (take screenWidth . drop xoffset) $
            take screenHeight $
            headBuffer ++ drop yoffset treeBuffer
        screen =
            image ++ take (screenHeight - length image) (repeat "~")

    case map (++"\ESC[K") screen of
      (first : rest) ->
          putStr $ "\ESC[H" ++ first ++ concatMap ("\n"++) rest
      _ ->
          return ()



winchHandler :: IO ()
winchHandler = do
    -- Report the size of the screen in characters.
    -- Result is CSI 9 ; height ; width t
    putStr "\ESC[19t"


keymap :: String -> Maybe (State -> IO State)

keymap "r" = Just replyToAll
keymap "e" = Just viewSource
keymap "k" = Just $ moveCursorUp 1
keymap "j" = Just $ moveCursorDown 1
keymap "K" = Just $ moveTreeDown 1
keymap "J" = Just $ moveTreeUp 1
keymap "\ESC[A" = Just $ moveCursorUp 1
keymap "\ESC[B" = Just $ moveCursorDown 1
keymap "\ESC[a" = Just $ moveTreeDown 1
keymap "\ESC[b" = Just $ moveTreeUp 1
keymap "\ESC[5~" = Just $ \q -> moveTreeDown (screenHeight q `div` 2) q   -- PgUp
keymap "\ESC[6~" = Just $ \q -> moveTreeUp (screenHeight q `div` 2) q -- PgDn
keymap "\n" = Just toggleFold

keymap ('\ESC':'[':'9':';':xs) = Just $ \q@State{..} -> do
    let (h,';':w) = break (==';') (take (length xs - 1) xs) -- ^ drop (assumed) trailing 't'
    return q { screenWidth = read w, screenHeight = read h }
keymap _ = Nothing



topOverrun State{..} =
    max 0 (- (linearPos cursor - yoffset))

botOverrun State{..} =
    max 0 (linearPos cursor - yoffset - (screenHeight - (length headBuffer) - 1))


moveCursorDown n q@State{..} =
    let cursor' = findNextN n cursor
        q' = q { cursor = cursor' }
    in case botOverrun q' of
        0 -> return q'
        i -> moveTreeUp i q'


moveCursorUp n q@State{..} =
    let cursor' = findPrevN n cursor
        q' = q { cursor = cursor' }
    in case topOverrun q' of
        0 -> return q'
        i -> moveTreeDown i q'


moveTreeUp n q@State{..} =
    let q' = q { yoffset = min (length treeBuffer - 1) $ max 0 (yoffset + n) }
    in case topOverrun q' of
        0 -> return q'
        i -> moveCursorDown i q'


moveTreeDown n q@State{..} =
    let q' = q { yoffset = min (length treeBuffer - 1) $ max 0 (yoffset - n) }
    in case botOverrun q' of
        0 -> return q'
        i -> moveCursorUp i q'


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
