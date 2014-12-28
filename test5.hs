{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Tree as Tree
import qualified Data.Tree.Zipper as Z
import qualified Notmuch
import qualified Notmuch.Message as Notmuch
import qualified Notmuch.SearchResult as Notmuch
import qualified System.Console.Terminal.Size as Term
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Monoid
import Scanner (getKey)
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Posix.Files
import System.Posix.Signals
import System.Process
import Trammel
import TreeSearch
import TreeView
import TreeViewRaw
import Utils


data Event =
    EFlash (Trammel String) |
    EKey String |
    EReload |
    EResize Int Int
  deriving Show


data State = State
    { cursor :: Z.TreePos Z.Full TreeView
    , xoffset :: Int
    , yoffset :: Int
    , flashMessage :: Trammel String
    , screenWidth :: Int
    , screenHeight :: Int
    , headBuffer :: [Trammel String]
    , treeBuffer :: [Trammel String]
    }


initState :: IO State
initState = do
    let query = "tag:inbox AND NOT tag:killed"

    r_ <- either error id <$> Notmuch.search query

    return State
        { cursor = Z.fromTree $ fromSearchResults query r_
        , xoffset = 0
        , yoffset = 0
        , flashMessage = "Welcome to much; quit with ^C"
        , screenWidth = 0
        , screenHeight = 0
        , headBuffer = []
        , treeBuffer = []
        }


main :: IO ()
main = finally startup cleanup


cleanup :: IO ()
cleanup = do
    hPutStr stdout "\ESC[?25h"
    hPutStr stdout "\ESC[?1049l"


startup :: IO ()
startup = do

    -- load-env hack
    maybe (return ()) (setEnv "HOME") =<< lookupEnv "OLDHOME"

    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout (BlockBuffering $ Just 4096)

    q0@State{..} <- initState

    --hSetEcho stdin False
    --hSetBuffering stdin NoBuffering
    -- Save Cursor and use Alternate Screen Buffer
    hPutStr stdout "\ESC[?1049h"
    -- Hide Cursor
    hPutStr stdout "\ESC[?25l"

    (putEvent, getEvent) <- do
        v <- newEmptyMVar
        return (putMVar v, takeMVar v)

    (shutdown, waitForShutdown) <- do
        v <- newEmptyMVar
        return (putMVar v (), takeMVar v)


    mapM_ (\(s, f) -> installHandler s (Catch f) Nothing)
        [ (sigINT, shutdown)
        , (28, winchHandler putEvent)
        ]

    threadIds <- mapM forkIO
        [ inputHandler putEvent
        , run getEvent q0
        ]

    winchHandler putEvent

    waitForShutdown
    mapM_ killThread threadIds


winchHandler :: (Event -> IO ()) -> IO ()
winchHandler putEvent =
    Term.size >>= \case
        Just (Term.Window {Term.width = w, Term.height = h}) ->
            putEvent $ EResize w h
        Nothing ->
            return ()


inputHandler :: (Event -> IO ()) -> IO ()
inputHandler putEvent = forever $ do
    -- TODO make stdin configurable
    getKey >>= putEvent . EKey


run :: IO Event -> State -> IO ()
run getEvent = rec where
    rec q = rec =<<
        let q' = render q
        in redraw q' >> getEvent >>= processEvent q'


processEvent :: State -> Event -> IO State
processEvent q = \case
    EFlash t ->
        return q { flashMessage = t }
    EKey s ->
        case keymap s of
            Just a ->
                a q
            Nothing ->
                return q { flashMessage = Plain $ show s }
    EResize w h ->
        return q
            { screenWidth = w, screenHeight = h
            , flashMessage = Plain $ "resize " <> show (w,h)
            }
    ev ->
        return q
            { flashMessage = SGR [31,1] $ Plain $ "unhandled event: " <> show ev
            }


render :: State -> State
render q@State{..} =
    q { treeBuffer = newTreeBuf
      , headBuffer = newHeadBuf
      }
  where
    newTreeBuf = renderTreeView (Z.label cursor) (Z.toTree cursor)
    newHeadBuf =
        [ Plain (show screenWidth) <> "x" <> Plain (show screenHeight)
          <> " " <> Plain (show $ linearPos cursor - yoffset)
          <> " " <> Plain (show $ topOverrun q)
          <> " " <> Plain (show $ botOverrun q)
          <> " " <> flashMessage
          <> " " <> Plain (show (xoffset, yoffset))
        ]

render0 :: State -> [Trammel String]
render0 _q@State{..} = do
    let buffer =
            map (trammelTake screenWidth . trammelDrop xoffset) $
            take screenHeight $
            headBuffer ++ drop yoffset treeBuffer
    buffer ++ take (screenHeight - length buffer) (repeat "~")


redraw :: State -> IO ()
redraw q@State{..} = do
    hPutStr stdout $ map (sub '\t' ' ') $ "\ESC[H" ++ (pp $ mintercalate "\n" $ map eraseRight $ render0 q)
    hFlush stdout
  where
    sub x x' c = if c == x then x' else c
    eraseRight s =
        if len s < screenWidth
            then s <> "\ESC[K"
            else s




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
keymap "\ESC[c" = Just $ moveTreeLeft 1   -- S-Right
keymap "\ESC[d" = Just $ moveTreeRight 1  -- S-Left
keymap "\ESC[5~" = Just $ \q -> moveTreeDown (screenHeight q `div` 2) q   -- PgUp
keymap "\ESC[6~" = Just $ \q -> moveTreeUp (screenHeight q `div` 2) q     -- PgDn
keymap "\n" = Just toggleFold
keymap "\DEL" = Just moveToParent -- backspace

keymap _ = Nothing



topOverrun :: State -> Int
topOverrun State{..} =
    max 0 (- (linearPos cursor - yoffset))


botOverrun :: State -> Int
botOverrun State{..} =
    max 0 (linearPos cursor - yoffset - (screenHeight - (length headBuffer) - 1))



moveCursorDown :: Monad m => Int -> State -> m State
moveCursorDown n q@State{..} =
    let cursor' = findNextN n cursor
        q' = q { cursor = cursor' }
    in case botOverrun q' of
        0 -> return q'
        i -> moveTreeUp i q'


moveCursorUp :: Monad m => Int -> State -> m State
moveCursorUp n q@State{..} =
    let cursor' = findPrevN n cursor
        q' = q { cursor = cursor' }
    in case topOverrun q' of
        0 -> return q'
        i -> moveTreeDown i q'


moveTreeUp :: Monad m => Int -> State -> m State
moveTreeUp n q@State{..} =
    let q' = q { yoffset = min (length treeBuffer - 1) $ max 0 (yoffset + n) }
    in case topOverrun q' of
        0 -> return q'
        i -> moveCursorDown i q'


moveTreeDown :: Monad m => Int -> State -> m State
moveTreeDown n q@State{..} =
    let q' = q { yoffset = min (length treeBuffer - 1) $ max 0 (yoffset - n) }
    in case botOverrun q' of
        0 -> return q'
        i -> moveCursorUp i q'


moveTreeLeft :: Monad m => Int -> State -> m State
moveTreeLeft n q@State{..} =
    return q { xoffset = xoffset + n }

moveTreeRight :: Monad m => Int -> State -> m State
moveTreeRight n q@State{..} =
    return q { xoffset = max 0 (xoffset - n) }


moveToParent :: Monad m => State -> m State
moveToParent q@State{..} =
    case Z.parent cursor of
        Nothing -> return q { flashMessage = "cannot go further up" }
        Just cursor' ->
            let q' = q { cursor = cursor' }
            in case topOverrun q' of
                0 -> return q'
                i -> moveTreeDown i q'


toggleFold :: State -> IO State
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




replyToAll :: State -> IO State
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
                    (system $ editor ++ " " ++ path) >>= \case
                        ExitFailure code ->
                            putStrLn $ editor ++ " exit code = " ++ show code
                        ExitSuccess ->
                            return ()
        return q


viewSource :: State -> IO State
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
                    (system $ editor ++ " " ++ path) >>= \case
                        ExitFailure code ->
                            putStrLn $ editor ++ " exit code = " ++ show code
                        ExitSuccess ->
                            return ()
        return q
