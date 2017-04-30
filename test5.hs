{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main, mainWithArgs) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.CaseInsensitive   (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Tree as Tree
import qualified Data.Tree.Zipper as Z
import qualified Network.Mail.Mime as M
import qualified Notmuch
import qualified Notmuch.Message as Notmuch
import qualified Notmuch.SearchResult as Notmuch
import qualified System.Console.Terminal.Size as Term
import Blessings
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Data.Aeson (eitherDecode')
import Data.Foldable (foldrM)
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import Data.Time
import Event
import ParseMail (readMail)
import RenderTreeView (renderTreeView)
import Scanner (scan,Scan(..))
import Screen
import Safe
import System.Directory
import System.Console.Docopt.NoTH (getArgWithDefault, parseArgsOrExit, parseUsageOrExit, shortOption)
import System.Environment
import System.Exit
import System.IO
import System.Posix.Signals
import System.Process
import TagUtils
import Text.Hyphenation
import Text.LineBreak
import TreeSearch
import TreeView
import TreeZipperUtils (modifyFirstParentLabelWhere)
import Utils

import Control.DeepSeq (rnf)

-- | Fork a thread while doing something else, but kill it if there's an
-- exception.
--
-- This is important in the cases above because we want to kill the thread
-- that is holding the Handle lock, because when we clean up the process we
-- try to close that handle, which could otherwise deadlock.
--
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid


data State = State
    { cursor :: Z.TreePos Z.Full TreeView
    , xoffset :: Int
    , yoffset :: Int
    , flashMessage :: Blessings String
    , screenWidth :: Int
    , screenHeight :: Int
    , headBuffer :: [Blessings String]
    , treeBuffer :: [Blessings String]
    , now :: UTCTime
    , signalHandlers :: [(Signal, IO ())]
    }

initState :: String -> IO State
initState query = do
    r_ <- either error id <$> Notmuch.search
                                [ "--offset=0"
                                , "--limit=100"
                                , query
                                ]

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
        , signalHandlers = []
        }


main :: IO ()
main =
    getArgs >>= mainWithArgs


mainWithArgs :: [String] -> IO ()
mainWithArgs args = do
    usage' <- parseUsageOrExit usage
    args' <- parseArgsOrExit usage' args
    let query = getArgWithDefault args' defaultSearch (shortOption 'q')
    withScreen s0 (\_-> initState query >>= runState)
  where
    usage = unlines
      [ "Command-line MUA using notmuch."
      , ""
      , "Usage:"
      , "  much [-q <search-term>]"
      , ""
      , "Options:"
      , "  -q <search-term>, --query=<search-term>"
      , "        Open specific search, defaults to " ++ (show defaultSearch)
      ]
    defaultSearch = "tag:inbox AND NOT tag:killed"

    s0 = Screen False NoBuffering (BlockBuffering $ Just 4096)
            [ 1000 -- X & Y on button press and release
            , 1005 -- UTF-8 mouse mode
            , 1047 -- use alternate screen buffer
            ]
            [   25 -- hide cursor
            ]

runState :: State -> IO ()
runState q0 = do

    -- load-env hack
    maybe (return ()) (setEnv "HOME") =<< lookupEnv "OLDHOME"

    (putEvent, getEvent) <- do
        v <- newEmptyMVar
        return (putMVar v, takeMVar v)

    let q1 = q0 { signalHandlers =
                    [ (sigINT, putEvent EShutdown)
                    , (28, winchHandler putEvent)
                    ] }

    installHandlers (signalHandlers q1)

    threadIds <- mapM forkIO
        [ forever $ scan stdin >>= putEvent . EScan
        ]

    winchHandler putEvent

    run getEvent q1
    mapM_ killThread threadIds


installHandlers :: [(Signal, IO ())] -> IO ()
installHandlers =
    mapM_ (\(s, h) -> installHandler s (Catch h) Nothing)

uninstallHandlers :: [(Signal, IO ())] -> IO ()
uninstallHandlers =
    mapM_ (\(s, _) -> installHandler s Ignore Nothing)

withoutHandlers :: (State -> IO State) -> State -> IO State
withoutHandlers f q@State{..} =
    bracket_ (uninstallHandlers signalHandlers)
             (installHandlers signalHandlers)
             (f q)


winchHandler :: (Event -> IO ()) -> IO ()
winchHandler putEvent =
    Term.size >>= \case
        Just (Term.Window {Term.width = w, Term.height = h}) ->
            putEvent $ EResize w h
        Nothing ->
            return ()


run :: IO Event -> State -> IO ()
run getEvent = rec . Right where
    rec = \case
        Right q -> rec =<< do
            t <- getCurrentTime
            let q' = render q { now = t }
            redraw q' >> getEvent >>= processEvent q'
        Left _q -> return ()


processEvent :: State -> Event -> IO (Either State State)
processEvent q = \case
    EFlash t ->
        return $ Right q { flashMessage = t }
    EScan (ScanKey s) ->
        Right <$> keymap s q
    EScan info@ScanMouse{..} ->
        Right <$> mousemap info q
    EShutdown ->
        return $ Left q
    EResize w h ->
        return $ Right q
            { screenWidth = w, screenHeight = h
            , flashMessage = Plain $ "resize " <> show (w,h)
            }
    ev ->
        return $ Right q
            { flashMessage = SGR [31,1] $ Plain $ "unhandled event: " <> show ev
            }


render :: State -> State
render q@State{..} =
    q { treeBuffer = newTreeBuf
      , headBuffer = newHeadBuf
      }
  where
    newTreeBuf = renderTreeView now cursor (Z.root cursor)
    newHeadBuf =
        [ Plain (show screenWidth) <> "x" <> Plain (show screenHeight)
          <> " " <> Plain (show $ linearPos cursor - yoffset)
          <> " " <> Plain (show $ topOverrun q)
          <> " " <> Plain (show $ botOverrun q)
          <> " " <> flashMessage
          <> " " <> Plain (show (xoffset, yoffset))
        ]

render0 :: State -> [Blessings String]
render0 _q@State{..} = do
    let buffer =
            map (blessingsTake screenWidth . blessingsDrop xoffset) $
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

keymap "A" = attachFilesToDraft
keymap "a" = toggleTagAtCursor "inbox"
keymap "s" = toggleTagAtCursor "unread"
keymap "&" = toggleTagAtCursor "killed"
keymap "*" = toggleTagAtCursor "star"
keymap "r" = replyToAll
keymap "e" = withoutHandlers viewSource
keymap "t" = withoutHandlers editTagsAtCursor
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

-- TODO wrap/unwrap to separate module
keymap "=" = \q@State{..} ->
    let cursor' = case Z.label cursor of
            TVMessageLine a b c s ->
                wrap (TVMessageLine a b c) cursor s
            _ -> cursor
    in return q { cursor = cursor' }
  where

    --unwrap = error "WIP"
        -- 1. get current id (must be TVMessageLine)
        -- 2. find first adjoined TVMessageLine with same id
        -- 3. find last adjoined TVMessageLine with same id
        -- 4. join lines (with space?)

    wrap ctor loc s =
        fromMaybe (error "die hard") $
        Z.nextTree $
        foldr (insert . ctor)
              (Z.delete loc)
              $ hy s

    insert a =
        Z.prevSpace . Z.insert (Tree.Node a [])

    hy s =
        breakStringLn bf s
      where
        shy = '\173'
        hyp = Just german_1996
        bf = BreakFormat 80 8 shy hyp

keymap "\ESCq" = editSearchTerm

-- <F1>
keymap "\ESC[11~" = \q@State{..} ->
    return q { flashMessage = Plain $ show $ treeViewId $ Z.label cursor }

-- <F2>
keymap "\ESC[12~" = \q@State{..} ->
    return q { flashMessage =
                  Plain $
                  show $
                  maybe Nothing (Just . Notmuch.messageFilename) $
                  getMessage $
                  Z.label cursor
              }

-- TODO Stuff Vim sends after exit (also there is more...)
keymap "\ESC[2;2R" = \q -> return q { flashMessage = flashMessage q <> " " <> Plain "stupid" }
keymap "\ESC[>85;95;0c" = \q -> return q { flashMessage = flashMessage q <> " " <> Plain "stupid" }

keymap s = \q ->
    return q { flashMessage = Plain $ show s }


mousemap :: Scan -> State -> IO State

mousemap ScanMouse{mouseButton=1,mouseY=y} = defaultMouse1Click y
mousemap ScanMouse{mouseButton=3,mouseY=y} = \q -> defaultMouse1Click y q >>= toggleFold
mousemap ScanMouse{mouseButton=4} = moveTreeDown 3
mousemap ScanMouse{mouseButton=5} = moveTreeUp 3
mousemap ScanMouse{mouseButton=0} = return
mousemap info = \q ->
    return q { flashMessage = SGR [38,5,202] $ Plain $ show info }


defaultMouse1Click :: Monad m => Int -> State -> m State
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
toggleFold q@State{..} =
    getNewSubForest >>= return . \case
        Left err ->
            q { flashMessage = SGR [31] $ Plain err }
        Right sf ->
            q { cursor = Z.modifyTree (setSubForest sf) cursor }
  where
    getNewSubForest =
        if hasUnloadedSubForest (Z.tree cursor)
            then loadSubForest (Z.label cursor)
            else return $ Right $ unloadSubForest (Z.tree cursor)


toggleTagAtCursor :: Tag -> State -> IO State
toggleTagAtCursor tag q@State{..} = case Z.label cursor of

    TVSearchResult sr -> do
        let tagOp =
                if tag `elem` Notmuch.searchTags sr
                    then DelTag
                    else AddTag
            tagOps = [tagOp tag]
        Notmuch.notmuchTag tagOps sr
        let cursor' = Z.modifyTree (patchTreeTags tagOps) cursor
        return q { cursor = cursor' }

    TVMessage m -> do
        let tagOp =
                if tag `elem` Notmuch.messageTags m
                    then DelTag
                    else AddTag
            tagOps = [tagOp tag]
        Notmuch.notmuchTag tagOps m
        let cursor' =
                -- TODO this needs a nice name
                modifyFirstParentLabelWhere isTVSearchResult f $
                Z.modifyLabel f cursor
            f = patchTags tagOps
        return q { cursor = cursor' }

    _ -> return q { flashMessage = "nothing happened" }


attachFilesToDraft :: State -> IO State
attachFilesToDraft q0 =
    runExceptT (go q0) >>= return . \result ->
        q0 { flashMessage = Plain (show result) }
  where
    go :: State -> ExceptT String IO String
    go State{cursor=cursor,now=now} = do
        msg <- maybe (throwE "not a message") return $
            getMessage (Z.label cursor)

        unless (Notmuch.hasTag "draft" msg) $
            throwE "message has no draft tag"

        paths <- filter (not . null) . lines <$> promptE "add files"
        when (null paths) (throwE "Aborted")

        newTags <-
            lines . LBS8.unpack . fst <$>
            readNotmuchE ["config", "get", "new.tags"] ""

        let tagOps =
                map AddTag ("attachment" : Notmuch.messageTags msg) ++
                map DelTag (map T.pack newTags)

        loadMailE (Notmuch.messageId msg)

            >>= attachFilesE paths
                -- ^ this will catch invalid paths
                -- TODO if it fails, then re-prompt

            >>= return . (addDateHeader now . removeHeader "Date")

            >>= liftE . M.renderMail' . removeHeaders
                [ "Content-Type"
                , "Content-Transfer-Encoding"
                , "MIME-Version"
                ]
            >>= readNotmuchE ("insert" : "--no-hooks" : tagOpsToArgs tagOps)
            >>  liftE (removeFile $ Notmuch.messageFilename msg)
            >>  readNotmuchE ["new", "--no-hooks", "--quiet"] ""

            >>  return "files attached"


replyToAll :: State -> IO State
replyToAll q0 =
    runExceptT (go q0) >>= \result ->
        toggleFold q0 { flashMessage = Plain (show result) }
  where
    go :: State -> ExceptT String IO String
    go State{cursor=cursor,now=now} = do
        msg <- maybe (throwE "not a message") return $
            getMessage (Z.label cursor)

        let msgId = Notmuch.unMessageID $ Notmuch.messageId msg

        newTags <-
            lines . LBS8.unpack . fst <$>
            readNotmuchE ["config", "get", "new.tags"] ""

        let tagOps =
                map AddTag ["draft"] ++
                map DelTag (map T.pack newTags)

        readNotmuchE ["reply", msgId] ""
            >>= return . LBS8.unpack . fst

            -- TODO abort if nothing has been edited
            >>= editMailE

            >>= return . T.pack
            >>= return . readMail
            >>= return . addDateHeader now
            >>= liftE . M.renderMail'
            >>= readNotmuchE ("insert" : "--no-hooks" : tagOpsToArgs tagOps)
            >>  return "draft created"


viewSource :: State -> IO State
viewSource q0 =
    runExceptT (go q0) >>= return . \result ->
        q0 { flashMessage = Plain (show result) }
  where
    go State{cursor=cursor} = do
        msg <- maybe (throwE "not a message") return $
            getMessage (Z.label cursor)

        liftE (readFile $ Notmuch.messageFilename msg) >>= viewMailE


editSearchTerm :: State -> IO State
editSearchTerm q0 =
    runExceptT (go q0) >>= return . \case
        Right q' -> q'
        Left err -> q0 { flashMessage = Plain $ "error: " ++ show err }
  where
    go :: State -> ExceptT String IO State
    go q@State{..} = do

        let parse = filter (/='\n') -- TODO proper parse
            s = fromMaybe "" $ getSearchTerm $ Z.label $ Z.root cursor

        s' <- editStringE s

        result <-
            either throwE return . eitherDecode' . fst =<<
            readNotmuchE ["search", "--format=json", "--format-version=2", s'] ""
            -- ^ TODO duplicates Notmuch.search

        return q { cursor = Z.fromTree $ fromSearchResults (parse s') result }



editTagsAtCursor :: State -> IO State
editTagsAtCursor q0 =
    runExceptT (go q0) >>= return . \case
        Right q' -> q'
        Left err -> q0 { flashMessage = Plain $ "error: " ++ show err }
  where
    go :: State -> ExceptT String IO State
    go q@State{..} = do
        -- TODO does this scream for a type class? :)
        (searchTerm, tags, patch) <- case Z.label cursor of
            TVSearchResult sr -> return
                ( Notmuch.unThreadID $ Notmuch.searchThread sr
                , Notmuch.searchTags sr
                , patchSearchResult
                )
            TVMessage m -> return
                ( Notmuch.unMessageID $ Notmuch.messageId m
                , Notmuch.messageTags m
                , patchMessage
                )
            _ -> throwE "cannot edit tags here"

        tagOps <- editTagsE tags
        when (null tagOps) (throwE "nothing happened")

        _ <- readNotmuchE ("tag" : tagOpsToArgs tagOps ++ ["--", searchTerm]) ""

        return q { cursor = select (==Z.label cursor) (patch tagOps cursor) }

--
-- utilities
--


setSubForest :: Tree.Forest a -> Tree.Tree a -> Tree.Tree a
setSubForest sf t = t { Tree.subForest = sf }


patchMessage
    :: [TagOp] -> Z.TreePos Z.Full TreeView -> Z.TreePos Z.Full TreeView
patchMessage tagOps loc =
    Z.modifyTree (patchTreeTags tagOps) loc


patchSearchResult
     :: [TagOp] -> Z.TreePos Z.Full TreeView -> Z.TreePos Z.Full TreeView
patchSearchResult tagOps loc =
    -- TODO this needs test cases
    let
        -- patch message
        loc' = Z.modifyTree (patchRootLabelTags tagOps) loc

        -- find search result of message
        srloc = fromMaybe (error "could not find search result of message")
                          (findParent isTVSearchResult loc')

        -- patch search result
        srloc' = Z.modifyTree (patchRootLabelTags tagOps) srloc
    in
        -- return message
        fromMaybe (error "could not find message again")
                  (findTree (==Z.label loc) srloc')


-- TODO rename select
select :: (a -> Bool) -> Z.TreePos Z.Full a -> Z.TreePos Z.Full a
select p loc =
    let root = Z.root loc
    in fromMaybe root $ findTree p root


withTempFile' :: FilePath -> ((FilePath, Handle) -> IO a) -> IO a
withTempFile' s f = do
    logname <- getEnv "LOGNAME"
    tmpdir <- getTemporaryDirectory
    withTempFile tmpdir (logname ++ "_much_" ++ s) f


addDateHeader :: UTCTime -> M.Mail -> M.Mail
addDateHeader t m@M.Mail{..} = do
    m { M.mailHeaders =
          ( "Date"
          , T.pack $
            formatTime defaultTimeLocale
                       rfc822DateFormat
                       t
          ) :
          mailHeaders
      }


removeHeader :: CI BS.ByteString -> M.Mail -> M.Mail
removeHeader h m@M.Mail{..} =
    m { M.mailHeaders = filter (\(k, _) -> CI.mk k /= h) mailHeaders }


removeHeaders :: [CI BS.ByteString] -> M.Mail -> M.Mail
removeHeaders hs m@M.Mail{..} =
    m { M.mailHeaders = filter (\(k, _) -> CI.mk k `notElem` hs) mailHeaders }


editMail :: String -> IO (Either ExitCode String)
editMail s =
    withTempFile' "edit.mail" $ \(path, h_tempFile) -> do
        hPutStr h_tempFile s
        hClose h_tempFile
        editor <- getEnv "EDITOR"
        runInteractive editor [path] >>= \case
            ExitSuccess -> Right <$> readFile path
            code -> return (Left code)


editString :: String -> IO (Either ExitCode String)
editString s =
    withTempFile' "edit.string" $ \(path, h_tempFile) -> do
        hPutStr h_tempFile s
        hClose h_tempFile
        editor <- getEnv "EDITOR"
        runInteractive editor [path] >>= \case
            ExitSuccess -> Right <$> readFile path
            code -> return (Left code)


editTags :: [Tag] -> IO (Either ExitCode [TagOp])
editTags tags =
    withTempFile' "edit.tags" $ \(path, h_tempFile) -> do
        T.hPutStrLn h_tempFile $ T.intercalate " " tags
        hClose h_tempFile
        editor <- getEnv "EDITOR"
        runInteractive editor [path] >>= \case
            ExitSuccess -> Right . diffTags tags . parseTags <$> readFile path
                                                   -- ^ TODO parseTags can fail
            code -> return (Left code)


viewMail :: String -> IO (Either ExitCode ())
viewMail s = do
    pager <- getEnv "PAGER"
    (Just h_in, _, _, h_proc) <-
        createProcess (shell pager) { std_in = CreatePipe }
    hPutStr h_in s
    hClose h_in
    waitForProcess h_proc >>= \case
        ExitSuccess -> return (Right ())
        code -> return (Left code)


prompt :: String -> IO (Either ExitCode String)
prompt ps =
    withTempFile' "prompt" $ \(path, h_tempFile) -> do
        mapM_ (hPutStrLn h_tempFile) $ "" : map comment (lines ps)
        hClose h_tempFile
        editor <- getEnv "EDITOR"
        runInteractive editor [path] >>= \case
            ExitSuccess -> Right . removeComments <$> readFile path
            code -> return (Left code)
  where
    comment = ("# "++)
    removeComments =
        unlines .
        filter (maybe True (/='#') . headMay) .
        lines


runInteractive :: FilePath -> [String] -> IO ExitCode
runInteractive cmd args = do
    (_, _, _, h_proc) <- createProcess (proc cmd args)
    waitForProcess h_proc


liftE :: IO a -> ExceptT String IO a
liftE io =
    lift (try io) >>= either (throwE . transformException) return
  where
    transformException :: IOException -> String
    transformException = ("IO: "++) . show


editMailE :: String -> ExceptT String IO String
editMailE ps =
    liftE (editMail ps) >>= \case
        Right r -> return r
        Left code -> throwE $ "edit mail error: " ++ show code


editStringE :: String -> ExceptT String IO String
editStringE s =
    liftE (editString s) >>= \case
        Right r -> return r
        Left code -> throwE $ "edit string error: " ++ show code


editTagsE :: [Tag] -> ExceptT String IO [TagOp]
editTagsE ps =
    liftE (editTags ps) >>= \case
        Right r -> return r
        Left code -> throwE $ "edit tags error: " ++ show code


viewMailE :: String -> ExceptT String IO ()
viewMailE ps =
    liftE (viewMail ps) >>= \case
        Right r -> return r
        Left code -> throwE $ "view mail error: " ++ show code


promptE :: String -> ExceptT String IO String
promptE ps =
    liftE (prompt ps) >>= \case
        Right r -> return r
        Left code -> throwE $ "prompt error: " ++ show code


loadMailE :: Notmuch.MessageID -> ExceptT String IO M.Mail
loadMailE msgId =
    liftE (Notmuch.notmuchShowMail $ Notmuch.unMessageID msgId) >>= \case
        Right m -> return m
        Left err -> throwE $ "load mail error: " ++ show err


readNotmuchE
    :: [String]
    -> LBS8.ByteString
    -> ExceptT String IO (LBS8.ByteString, LBS8.ByteString)
readNotmuchE args input = do
    (exitCode, out, err) <- liftE $ do
        (Just hin, Just hout, Just herr, ph) <-
            createProcess (proc "notmuch" args)
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
        LBS8.hPut hin input
        hClose hin

        out <- LBS8.hGetContents hout
        err <- LBS8.hGetContents herr

        withForkWait (evaluate $ rnf out) $ \waitOut -> do
            withForkWait (evaluate $ rnf err) $ \waitErr -> do

              ---- now write any input
              --unless (null input) $
              --  ignoreSigPipe $ hPutStr inh input
              -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
              --ignoreSigPipe $ hClose inh

              -- wait on the output
              waitOut
              waitErr
              hClose hout
              hClose herr

        -- wait on the process
        exitCode <- waitForProcess ph

        return (exitCode, out, err)

    case exitCode of
        ExitSuccess -> return (out, err)
        ExitFailure code ->
            throwE $ "notmuch " ++ intercalate " " args ++
                     " exit code: " ++ show code ++ "; stderr:\n" ++
                     LBS8.unpack err


attachFilesE :: [FilePath] -> M.Mail -> ExceptT String IO M.Mail
attachFilesE paths =
    liftE . attachFiles paths


attachFiles :: [FilePath] -> M.Mail -> IO M.Mail
attachFiles filenames mail0 =
    foldrM attachFile mail0 filenames


attachFile :: FilePath -> M.Mail -> IO M.Mail
attachFile filePath mail = do
    fileType <- fromMaybe "application/octet-stream" <$> getFileType filePath
    M.addAttachment (T.pack fileType) filePath mail


getFileType :: FilePath -> IO (Maybe String)
getFileType path =
    -- XXX GNU file's exit code is 0 even if path doesn't exist
    doesFileExist path >>= \case
        True -> do
            (_, out, _) <- readProcessWithExitCode "file" ["-Lib", path] ""
            return $ Just (init out)
        False ->
            return Nothing
