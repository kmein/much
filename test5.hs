{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
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
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import Data.Time
import Event
import Scanner (scan)
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Posix.Files
import System.Posix.Signals
import System.Process
import TagUtils
import Trammel
import TreeSearch
import TreeView
import TreeViewRaw
import Utils



data State = State
    { cursor :: Z.TreePos Z.Full TreeView
    , xoffset :: Int
    , yoffset :: Int
    , flashMessage :: Trammel String
    , screenWidth :: Int
    , screenHeight :: Int
    , headBuffer :: [Trammel String]
    , treeBuffer :: [Trammel String]
    , now :: UTCTime
    , decset :: [Int]
    , decrst :: [Int]
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
        , now = UTCTime (fromGregorian 1984 5 23) 49062
        , decset =
            1000 : -- X & Y on button press and release
            1005 : -- UTF-8 mouse mode
            1049 : -- use cleared alternate screen buffer
            []
        , decrst =
            25 : -- hide cursor
            []
        }


main :: IO ()
main =
    bracket initState cleanup startup


cleanup :: State -> IO ()
cleanup q@State{..} = do
    hSetEcho stdin True
    resetTerm q { decset = decrst, decrst = decset }


startup :: State -> IO ()
startup q0 = do

    -- load-env hack
    maybe (return ()) (setEnv "HOME") =<< lookupEnv "OLDHOME"

    -- TODO move this to resetTerm?
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout (BlockBuffering $ Just 4096)

    resetTerm q0

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
        [ forever $ scan stdin >>= putEvent
        , run getEvent q0
        ]

    winchHandler putEvent

    waitForShutdown
    mapM_ killThread threadIds


resetTerm State{..} = do
    hSetEcho stdin False
    hPutStr stdout $ "\ESC[?" ++ intercalate ";" (map show decset) ++ "h"
    hPutStr stdout $ "\ESC[?" ++ intercalate ";" (map show decrst) ++ "l"


winchHandler :: (Event -> IO ()) -> IO ()
winchHandler putEvent =
    Term.size >>= \case
        Just (Term.Window {Term.width = w, Term.height = h}) ->
            putEvent $ EResize w h
        Nothing ->
            return ()


run :: IO Event -> State -> IO ()
run getEvent = rec where
    rec q = rec =<< do
        t <- getCurrentTime
        let q' = render q { now = t }
        redraw q' >> getEvent >>= processEvent q'


-- TODO merge EKey and EMouse?
processEvent :: State -> Event -> IO State
processEvent q = \case
    EFlash t ->
        return q { flashMessage = t }
    EKey s ->
        keymap s q
    EMouse info ->
        mousemap info q
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
    newTreeBuf = renderTreeView now (Z.label cursor) (Z.toTree cursor)
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




keymap :: String -> State -> IO State

keymap "a" = toggleTagAtCursor "inbox"
keymap "s" = toggleTagAtCursor "unread"
keymap "&" = toggleTagAtCursor "killed"
keymap "*" = toggleTagAtCursor "star"
keymap "r" = replyToAll
keymap "e" = viewSource
keymap "t" = editTags
keymap "k" = moveCursorUp 1
keymap "j" = moveCursorDown 1
keymap "K" = moveTreeDown 1
keymap "J" = moveTreeUp 1
keymap "\ESC[A" = moveCursorUp 1
keymap "\ESC[B" = moveCursorDown 1
keymap "\ESC[a" = moveTreeDown 1
keymap "\ESC[b" = moveTreeUp 1
keymap "\ESC[c" = moveTreeLeft 1  -- S-Right
keymap "\ESC[d" = moveTreeRight 1 -- S-Left
keymap "\ESC[5~" = \q -> moveTreeDown (screenHeight q `div` 2) q  -- PgUp
keymap "\ESC[6~" = \q -> moveTreeUp (screenHeight q `div` 2) q    -- PgDn
keymap "\n" = toggleFold
keymap "\ESC[Z" = moveCursorUpToPrevUnread -- S-Tab
keymap "\t" = moveCursorDownToNextUnread
keymap "\DEL" = moveToParent  -- backspace

-- TODO Stuff Vim sends after exit (also there is more...)
keymap "\ESC[2;2R" = \q -> return q { flashMessage = flashMessage q <> " " <> Plain "stupid" }
keymap "\ESC[>85;95;0c" = \q -> return q { flashMessage = flashMessage q <> " " <> Plain "stupid" }

keymap s = \q ->
    return q { flashMessage = Plain $ show s }


mousemap :: MouseInfo -> State -> IO State

mousemap MouseInfo{mouseButton=1,mouseY=y} = defaultMouse1Click y
mousemap MouseInfo{mouseButton=3,mouseY=y} = \q -> defaultMouse1Click y q >>= toggleFold
mousemap MouseInfo{mouseButton=4} = moveTreeDown 3
mousemap MouseInfo{mouseButton=5} = moveTreeUp 3
mousemap MouseInfo{mouseButton=0} = return
mousemap info = \q ->
    return q { flashMessage = SGR [38,5,202] $ Plain $ show info }


defaultMouse1Click y q@State{..} = do
    let linearClickPos =
            let i = (y - length headBuffer + yoffset) - 1 {-zero-based-}
            in if 0 <= i && i < length treeBuffer
                then Just i
                else Nothing
    case linearClickPos of
        Nothing ->
            return q
                { flashMessage = Plain $ "nothing to click"
                }
        Just i ->
            return q
                { cursor = findNextN i $ Z.root cursor
                }



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


moveCursorToUnread
    :: (Num a, Monad m, Eq a)
    => (Z.TreePos Z.Full TreeView -> Maybe (Z.TreePos Z.Full TreeView))
    -> (State -> a)
    -> (a -> State -> m State)
    -> State -> m State
moveCursorToUnread cursorMove getTreeMoveCount treeMove q@State{..} =
    case cursorMove cursor >>= rec of
        Just cursor' ->
            let q' = q { cursor = cursor' }
            in case getTreeMoveCount q' of
                0 -> return q'
                i -> treeMove i q'
        Nothing ->
            return q { flashMessage = "no unread message in sight" }
  where
    rec loc =
        if hasTag "unread" loc
            then Just loc
            else cursorMove loc >>= rec
    hasTag tag loc =
        case Z.label loc of
            TVSearchResult sr ->
                tag `elem` Notmuch.searchTags sr
            TVMessage m ->
                tag `elem` Notmuch.messageTags m
            _ ->
                False

moveCursorUpToPrevUnread :: Monad m => State -> m State
moveCursorUpToPrevUnread =
    moveCursorToUnread findPrev topOverrun moveTreeDown

moveCursorDownToNextUnread :: Monad m => State -> m State
moveCursorDownToNextUnread =
    moveCursorToUnread findNext botOverrun moveTreeUp


toggleFold :: State -> IO State
toggleFold q@State{..} = case Z.label cursor of
    TVMessage _ -> do
        q' <- toggleTagAtCursor "open" q

        let Just sr = findParent isTVSearchResult cursor
            TVSearchResult the_sr = Z.label sr
            Notmuch.ThreadID tid = Notmuch.searchThread the_sr

        t_ <- return . fromMessageForest =<< Notmuch.getThread tid

        let cursor' = Z.modifyTree (\(Tree.Node l _) -> Tree.Node l t_) sr
        return q' { cursor = select (==Z.label cursor) cursor' }

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


toggleTagAtCursor :: Tag -> State -> IO State
toggleTagAtCursor tag q@State{..} = case Z.label cursor of
    TVMessage m -> do
        -- TODO modify search result tags
        -- TODO check Notmuch.{set,unset}Tag result
        if tag `elem` Notmuch.messageTags m
            then do
                Notmuch.unsetTag (T.unpack tag) (Notmuch.unMessageID $ Notmuch.messageId m)
                let cursor' = Z.modifyTree (patchRootLabelTags [DelTag tag]) cursor
                return q { cursor = cursor' }
            else do
                Notmuch.setTag (T.unpack tag) (Notmuch.unMessageID $ Notmuch.messageId m)
                let cursor' = Z.modifyTree (patchRootLabelTags [AddTag tag]) cursor
                return q { cursor = cursor' }

    _ -> return q { flashMessage = "nothing happened" }


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
                    (system (editor ++ " " ++ path) <* resetTerm q) >>= \case
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
                    (system (editor ++ " " ++ path) <* resetTerm q) >>= \case
                        ExitFailure code ->
                            putStrLn $ editor ++ " exit code = " ++ show code
                        ExitSuccess ->
                            return ()
        return q


-- TODO editTags is too convoluted
editTags :: State -> IO State
editTags q@State{..} = case Z.label cursor of
    TVSearchResult sr -> do
        edit
            (Notmuch.searchTags sr)
            ("thread:" <> (Notmuch.unThreadID $ Notmuch.searchThread sr))
            (\tagOps loc ->
                Z.modifyTree (patchTreeTags tagOps) loc
            )

    TVMessage m -> do
        edit
            (Notmuch.messageTags m)
            ("id:" <> (Notmuch.unMessageID $ Notmuch.messageId m)) -- TODO describe war besser
            (\tagOps mloc ->
                -- TODO this needs test cases
                let
                    -- patch message
                    mloc' = Z.modifyTree (patchRootLabelTags tagOps) mloc

                    -- find search result of message
                    srloc = fromMaybe (error "could not find search result of message")
                                      (findParent isTVSearchResult mloc')

                    -- patch search result
                    srloc' = Z.modifyTree (patchRootLabelTags tagOps) srloc

                in
                  -- return message
                  fromMaybe (error "could not find message again")
                            (findTree (==Z.label mloc) srloc')
                )
    _ ->
        return q { flashMessage = "cannot edit tags here" }
  where
    edit tags query patch = do
        editor <- getEnv "EDITOR"
        logname <- getEnv "LOGNAME"
        tmpdir <- getTemporaryDirectory

        let template = logname ++ "_much_.tags"

        withTempFile tmpdir template $ \(path, draftH) -> do
            hPutStr stdout "\ESC[?1049h" -- TODO geht besser
            hPutStr stdout "\ESC[?25l" -- TODO war mal besser
            setFileMode path 0o600

            -- generate draft
            T.hPutStrLn draftH $ T.intercalate " " tags
            hPutStrLn draftH $ "# " <> query

            hClose draftH
            -- TODO factorize editor
            (system (editor ++ " " ++ path) <* resetTerm q) >>= \case
                ExitFailure code -> do
                    return q { flashMessage = Plain $ editor ++ " exit code = " ++ show code }
                ExitSuccess -> do
                    -- TODO parse could fail
                    tags' <- parseTags <$> readFile path

                    case diffTags tags tags' of
                        [] ->
                            return q { flashMessage = Plain "nothing happened" } -- TODO highlight
                        tagOps -> do
                            (_, _, _, procH) <-
                                withFile "/dev/null" ReadWriteMode $ \nullH ->
                                    -- TODO batch tagging(?)
                                    -- TODO proper type for query
                                    createProcess
                                        (proc "notmuch" $ [ "tag" ] ++ tagOpsToArgs tagOps ++ [ "--", query ])
                                            { std_in = UseHandle nullH
                                            , std_out = UseHandle nullH
                                            }
                            waitForProcess procH >>= \case
                                ExitFailure code ->
                                    return q { flashMessage = Plain $ "notmuch exit code = " ++ show code }
                                ExitSuccess ->
                                    return q { cursor = select (==Z.label cursor) (patch tagOps cursor) }

    -- TODO DRY select
    select p loc =
        let root = Z.root loc
        in fromMaybe root $ findTree p root
