{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Much.Action
import Blessings.String
import Control.Concurrent
import Control.DeepSeq (rnf)
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Much.Core
import Data.Aeson (eitherDecode')
import Data.CaseInsensitive (CI)
import Data.Foldable (foldrM)
import Data.List (intercalate)
import Data.Maybe
import Data.Time
import Much.ParseMail (readMail)
import Safe
import Scanner
import Much.State
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import Much.TagUtils
import Text.Hyphenation
import Text.LineBreak
import Much.TreeSearch
import Much.TreeView
import Much.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Tree as Tree
import qualified Data.Tree.Zipper as Z
import qualified Network.Mail.Mime as M
import qualified Notmuch
import qualified Notmuch.Message as Notmuch
import qualified Notmuch.SearchResult as Notmuch

main :: IO ()
main =
    mainWithState emptyState
      { keymap = myKeymap
      , mousemap = myMousemap
      }

myKeymap :: String -> State -> IO State

myKeymap "A" = attachFilesToDraft
myKeymap "a" = toggleTagAtCursor "inbox"
myKeymap "s" = toggleTagAtCursor "unread"
myKeymap "&" = toggleTagAtCursor "killed"
myKeymap "*" = toggleTagAtCursor "star"
myKeymap "r" = replyToAll
myKeymap "e" = withoutHandlers viewSource
myKeymap "t" = withoutHandlers editTagsAtCursor
myKeymap "k" = moveCursorUp 1
myKeymap "j" = moveCursorDown 1
myKeymap "K" = moveTreeDown 1
myKeymap "J" = moveTreeUp 1
myKeymap "\ESC[A" = moveCursorUp 1
myKeymap "\ESC[B" = moveCursorDown 1
myKeymap "\ESC[a" = moveTreeDown 1
myKeymap "\ESC[b" = moveTreeUp 1
myKeymap "\ESC[c" = moveTreeLeft 1  -- S-Right
myKeymap "\ESC[d" = moveTreeRight 1 -- S-Left
myKeymap "\ESC[5~" = \q -> moveTreeDown (screenHeight q `div` 2) q  -- PgUp
myKeymap "\ESC[6~" = \q -> moveTreeUp (screenHeight q `div` 2) q    -- PgDn
myKeymap "\n" = toggleFold
myKeymap "\ESC[Z" = moveCursorUpToPrevUnread -- S-Tab
myKeymap "\t" = moveCursorDownToNextUnread
myKeymap "\DEL" = moveToParent  -- backspace

-- TODO wrap/unwrap to separate module
myKeymap "=" = \q@State{..} ->
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

myKeymap "\ESCq" = editSearchTerm

-- <F1>
myKeymap "\ESC[11~" = \q@State{..} ->
    return q { flashMessage = Plain $ show $ treeViewId $ Z.label cursor }

-- <F2>
myKeymap "\ESC[12~" = \q@State{..} ->
    return q { flashMessage =
                  Plain $
                  show $
                  maybe Nothing (Just . Notmuch.messageFilename) $
                  getMessage $
                  Z.label cursor
              }

-- TODO Stuff Vim sends after exit (also there is more...)
myKeymap "\ESC[2;2R" = \q -> return q { flashMessage = flashMessage q <> " " <> Plain "stupid" }
myKeymap "\ESC[>85;95;0c" = \q -> return q { flashMessage = flashMessage q <> " " <> Plain "stupid" }

myKeymap s = displayKey s


myMousemap :: Scan -> State -> IO State
myMousemap ScanMouse{mouseButton=1,mouseY=y} = defaultMouse1Click y
myMousemap ScanMouse{mouseButton=3,mouseY=y} = \q -> defaultMouse1Click y q >>= toggleFold
myMousemap ScanMouse{mouseButton=4} = moveTreeDown 3
myMousemap ScanMouse{mouseButton=5} = moveTreeUp 3
myMousemap ScanMouse{mouseButton=0} = return
myMousemap info = displayMouse info


withoutHandlers :: (State -> IO State) -> State -> IO State
withoutHandlers f q@State{..} =
    bracket_ (uninstallHandlers signalHandlers)
             (installHandlers signalHandlers)
             (f q)


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
