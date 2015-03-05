{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main, mainWithArgs) where

import qualified Data.ByteString as BS
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
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Foldable (foldrM)
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import Data.Time
import Event
import ParseMail (readMail)
import RenderTreeView (renderTreeView)
import Scanner (scan)
import Safe
import System.Directory
import System.Console.Docopt (getArgWithDefault, optionsWithUsage, shortOption)
import System.Environment
import System.Exit
import System.IO
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import System.Posix.Files
import System.Posix.Signals
import System.Process
import TagUtils
import Trammel
import TreeSearch
import TreeView
import TreeZipperUtils (modifyFirstParentLabelWhere)
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

initState :: String -> IO State
initState query = do
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
    getArgs >>= mainWithArgs


mainWithArgs :: [String] -> IO ()
mainWithArgs args = do
    args' <- optionsWithUsage usage args
    let query = getArgWithDefault args' defaultSearch (shortOption 'q')
    bracket (initState query) cleanup startup
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


resetTerm :: State -> IO ()
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
    newTreeBuf = renderTreeView now cursor (Z.root cursor)
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

keymap "A" = attachFilesToDraft
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

keymap "\ESCq" = \q@State{..} ->
    let parse = filter (/='\n') -- TODO proper parse
        draft = fromMaybe "" $ getSearchTerm $ Z.label $ Z.root cursor
    in editString q draft >>= \case
        Left err -> return q { flashMessage = Plain err }
        Right s' -> Notmuch.search s' >>= \case
            Left err ->
                return q { flashMessage = Plain err }
            Right result ->
                return q { cursor = Z.fromTree $ fromSearchResults (parse s') result }

keymap "\ESC[11~" = \q@State{..} ->
    return q { flashMessage = Plain $ show $ treeViewId $ Z.label cursor }

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


mousemap :: MouseInfo -> State -> IO State

mousemap MouseInfo{mouseButton=1,mouseY=y} = defaultMouse1Click y
mousemap MouseInfo{mouseButton=3,mouseY=y} = \q -> defaultMouse1Click y q >>= toggleFold
mousemap MouseInfo{mouseButton=4} = moveTreeDown 3
mousemap MouseInfo{mouseButton=5} = moveTreeUp 3
mousemap MouseInfo{mouseButton=0} = return
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


setSubForest :: Tree.Forest a -> Tree.Tree a -> Tree.Tree a
setSubForest sf t = t { Tree.subForest = sf }


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
attachFilesToDraft q@State{..} = case getMessage (Z.label cursor) of
    Nothing ->
        return q { flashMessage = "no message" }
    Just m -> do
        let msgId = Notmuch.unMessageID $ Notmuch.messageId m
        filenames <- either (const []) lines <$> prompt "add files"
        Notmuch.notmuchShowMail msgId >>= \case
            Left err ->
                return q { flashMessage = Plain $ "Error: " ++ show err }
            Right mail ->
                    return mail
                >>= return . removeHeaders -- TODO mk unforgettable
                    [ "Content-Type"
                    , "Content-Transfer-Encoding"
                    , "MIME-Version"
                    ]
                >>= attachFiles filenames
                >>= return . removeHeader "Date"
                >>= addDateHeader
                >>= M.renderMail'
                >>= Notmuch.notmuchWithInput
                    [ "insert"
                    , "--no-hooks"
                    -- TODO dont hardcode which tags (and use TagOps)
                    , "+draft"
                    , "+attachment"
                    , "-inbox"
                    , "-unread"
                    ]
                >>= \case
                    (ExitFailure code, _, _) ->
                        return q { flashMessage =
                            Plain $ "notmuch insert exit code = " ++ show code
                        }
                    _ ->
                        return q { flashMessage = "draft created" }


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


replyToAll :: State -> IO State
replyToAll q@State{..} = case getMessage (Z.label cursor) of
    Nothing ->
        return q { flashMessage = "no message" }
    Just m -> do
        let msgId = Notmuch.unMessageID $ Notmuch.messageId m
        withTempFile' "draft.mail" $ \(path, draftH) -> do
            (_, _, _, procH) <-
                withFile "/dev/null" ReadMode $ \nullH ->
                    createProcess
                        (proc "notmuch" [ "reply" , msgId ])
                            { std_in = UseHandle nullH
                            , std_out = UseHandle draftH
                            }
            hClose draftH
            waitForProcess procH >>= \case
                ExitFailure code ->
                    return q { flashMessage =
                                  Plain $ "notmuch exit code = " ++ show code
                             }
                ExitSuccess ->
                    runEditor' path q >>= \case
                        ExitFailure code ->
                            return q { flashMessage = Plain $ "editor exit code = " ++ show code }
                        ExitSuccess -> do
                            -- TODO check if path has been written to,
                            --      else abort
                            draft <-
                                M.renderMail' =<<
                                addDateHeader =<<
                                return . readMail =<<
                                T.readFile path
                            -- TODO use TagOps
                            Notmuch.notmuchWithInput
                                [ "insert"
                                , "--no-hooks"
                                , "+draft"
                                -- TODO dont hardcode which tags to delete
                                , "-inbox"
                                , "-unread"
                                ]
                                draft >>= \case
                                    (ExitFailure code, _, _) ->
                                        return q { flashMessage =
                                            Plain $ "notmuch insert exit code = " ++ show code
                                        }
                                    _ ->
                                        toggleFold q {
                                            flashMessage = "draft created"
                                        }


viewSource :: State -> IO State
viewSource q@State{..} = case getMessage (Z.label cursor) of
    Nothing ->
        return q { flashMessage = "no message" }
    Just m -> do
        let msgId = Notmuch.unMessageID $ Notmuch.messageId m
        withTempFile' "raw.mail" $ \(path, draftH) -> do
            setFileMode path 0o400
            (_, _, _, procH) <-
                withFile "/dev/null" ReadMode $ \nullH ->
                    createProcess
                        (proc "notmuch" [ "show", "--format=raw", msgId ])
                            { std_in = UseHandle nullH
                            , std_out = UseHandle draftH
                            }
            hClose draftH
            waitForProcess procH >>= \case
                ExitFailure code ->
                    return q { flashMessage =
                                  Plain $ "notmuch exit code = " ++ show code
                             }
                ExitSuccess ->
                    runEditor path q


-- TODO editTags is too convoluted
editTags :: State -> IO State
editTags q@State{..} = case Z.label cursor of
    TVSearchResult sr -> do
        edit
            (Notmuch.searchTags sr)
            (Notmuch.unThreadID $ Notmuch.searchThread sr)
            (\tagOps loc ->
                Z.modifyTree (patchTreeTags tagOps) loc
            )

    TVMessage m -> do
        edit
            (Notmuch.messageTags m)
            (Notmuch.unMessageID $ Notmuch.messageId m) -- TODO describe war besser
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
        withTempFile' ".tags" $ \(path, draftH) -> do
            hPutStr stdout "\ESC[?1049h" -- TODO geht besser
            hPutStr stdout "\ESC[?25l" -- TODO war mal besser
            setFileMode path 0o600

            -- generate draft
            T.hPutStrLn draftH $ T.intercalate " " tags
            hPutStrLn draftH $ "# " <> query

            hClose draftH

            runEditor' path q >>= \case
                ExitFailure code -> do
                    return q { flashMessage = Plain $ "editor exit code = " ++ show code }
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


editString :: State -> String -> IO (Either String String)
editString q s =
    withTempFile' ".string" $ \(path, h) -> do
        hPutStr stdout "\ESC[?1049h" -- TODO geht besser
        hPutStr stdout "\ESC[?25l" -- TODO war mal besser
        setFileMode path 0o600

        hPutStr h s

        hClose h

        runEditor' path q >>= \case
            ExitFailure code ->
                return . Left $ "error exit code = " <> show code
            ExitSuccess ->
                Right <$> readFile path


runEditor :: FilePath -> State -> IO State
runEditor path q@State{..} =
    runEditor' path q >>= \case
        ExitFailure code ->
            return q { flashMessage =
                          Plain $ "editor exit code = " ++ show code
                     }
        ExitSuccess ->
            return q


runEditor' :: [Char] -> State -> IO ExitCode
runEditor' path q@State{..} = do
    editor <- getEnv "EDITOR"
    system (editor ++ " " ++ path) <* resetTerm q


withTempFile' :: FilePath -> ((FilePath, Handle) -> IO a) -> IO a
withTempFile' s f = do
    logname <- getEnv "LOGNAME"
    tmpdir <- getTemporaryDirectory
    withTempFile tmpdir (logname ++ "_much_" ++ s) f


addDateHeader :: M.Mail -> IO M.Mail
addDateHeader m@M.Mail{..} = do
    t <- getCurrentTime
    return m
        { M.mailHeaders =
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
