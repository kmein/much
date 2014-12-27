{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}


import Data.Default
import Graphics.Vty

--import Data.List

import Control.Applicative
--import Language.Haskell.TH.Ppr (bytesToString)
--import Data.Aeson
--import Data.List.Split
--import Data.Attoparsec.ByteString hiding (string)
import Data.Maybe
import Data.Monoid
--import Data.String
--import Data.Traversable
import Data.Tree
import qualified Data.Tree.Zipper as Z
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
--import qualified Data.ByteString.Char8 as BS8
--import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
--import qualified Data.Text.Encoding as T
--import qualified Data.Text.IO as T
--import Data.Version (Version(..), parseVersion)
--import System.Process
--import System.IO
--import qualified Data.Map as M

import System.Environment
import Notmuch
import Notmuch.Message
import Notmuch.SearchResult
--import Safe

import Control.Exception

import TreeView

import TreeSearch
--import Editor (edit)
import System.Process
import System.Environment
import qualified Data.ByteString.Lazy as LBS
import System.IO
import System.Directory
import Control.Exception (bracket)
import Control.Exception
import System.IO
import Control.DeepSeq (rnf)


data State = State
    { vty :: Vty
    , cursor :: Z.TreePos Z.Full TreeView
    , xoffset :: Int
    , yoffset :: Int
    , message :: String
    }


toggleTag :: T.Text -> Message -> IO ()
toggleTag tag m = do
    _ <- if tag `elem` messageTags m
        then
            unsetTag tagString (unMessageID $ messageId m)
        else
            setTag tagString (unMessageID $ messageId m)
    return ()
  where
    tagString = T.unpack tag


tagMessage :: T.Text -> Message -> IO LBS.ByteString
tagMessage tag m =
  setTag (T.unpack tag) (unMessageID $ messageId m)


untagMessage :: T.Text -> Message -> IO LBS.ByteString
untagMessage tag m =
  unsetTag (T.unpack tag) (unMessageID $ messageId m)


main :: IO ()
main =
    main' "tag:inbox AND NOT tag:killed"

main' :: String -> IO ()
main' query = do
    setEnv "HOME" "/home/tv"
    bracket (mkVty def) finit run
  where

    finit vty = do
        shutdown vty

    run vty0 = do
        --XXX show a single thread
        --t_ <- getThread "0000000000000862"
        --let v = fromMessageTree t_
        --let c = findMessage (MessageID "87egtmvj0n.fsf@write-only.cryp.to") v
        --rec vty 0 c v

        r_ <- either error id <$> search query
        rec State
            { vty = vty0
            , cursor = Z.fromTree $ fromSearchResults query r_
            , xoffset = 0
            , yoffset = 0
            , message = "Welcome to much; quit with ^C"
            }

    rec :: State -> IO ()
    rec q0@State{..} = do
        let
            img =
                --string def (show i) <->
                --string def (maybe "Nothing" describe (focusPrev v cursor)) <->
                --string def (maybe "Nothing" describe cursor) <->
                --string def (maybe "Nothing" describe (focusNext v cursor)) <->
                --string def (maybe "Nothing" describe (focusPrev v cursor)) <->
                --string def (describe $ Z.label cursor) <->
                --string def (maybe "Nothing" describe (focusNext v cursor)) <->
                treeImage (Just $ Z.label cursor) (Z.toTree cursor)
                --renderTree q
            pic = picForImage $
                    (string def message) <->
                    translate xoffset yoffset img
            --v = Z.root cursor
        update vty pic
        nextEvent vty >>= \e -> case e of
            EvKey KEsc [] ->
                rec q

            EvKey (KChar 'c') [MCtrl] ->
                error "^C"

            EvKey (KChar 'k') [] ->
                rec q { cursor = fromMaybe (Z.root cursor) $ findPrev cursor }
            EvKey (KChar 'j') [] ->
                rec q { cursor = fromMaybe (Z.root cursor) $ findNext cursor }
            EvKey KEnter [] ->
                onEnter cursor

            EvKey (KChar 'H') [] -> rec q { xoffset = xoffset - 3 }
            EvKey (KChar 'L') [] -> rec q { xoffset = xoffset + 3 }
            EvKey (KChar 'J') [] -> rec q { yoffset = yoffset - 3 }
            EvKey (KChar 'K') [] -> rec q { yoffset = yoffset + 3 }

            EvKey (KChar 'r') [] ->
                case getMessage (Z.label cursor) of
                    Just m -> do
                        replyToAll m q >>= rec
                    Nothing ->
                        rec q { message = "no message" }
                --reply ToAll q >>= \case
                --    Left s -> rec q { message = s }
                --    Right () -> rec q

            EvResize _w _h ->
                rec q

            _ -> do
                rec q { message = "unbound key: " ++ show e }
      where

        q = q0 { message = "" }

        onEnter c_ = case Z.label c_ of
            TVMessage m -> do
                toggleTag "open" m

                let loc = cursor
                    Just sr = findParent isTVSearchResult loc
                    --Just sr0 = Z.firstChild sr -- TODO can there be only one (thread per sr)?
                    TVSearchResult the_sr = Z.label sr
                    ThreadID tid = searchThread the_sr

                t_ <- return . fromMessageForest =<< getThread tid

                let loc' = Z.modifyTree (\(Node l _) -> Node l t_) sr
                rec q { cursor = select (==Z.label cursor) loc' }

            TVSearchResult sr -> do
                --let Just loc = findTree (==c_) $ Z.fromTree v
                let loc = cursor

                let open = not $ null $ subForest $ Z.tree loc
                let ThreadID tid = searchThread sr

                t_ <-
                    if open
                        then return []
                        else return . fromMessageForest =<< getThread tid

                let loc' = Z.modifyTree (\(Node l _) -> Node l t_) loc
                rec q { cursor = select (==Z.label cursor) loc' }

            _ ->
                -- TODO make some noise
                rec q


select :: (a -> Bool) -> Z.TreePos Z.Full a -> Z.TreePos Z.Full a
select p loc = fromMaybe (error "cannot select") $ findTree p $ Z.root loc


treeImage :: Maybe TreeView -> Tree TreeView -> Image
treeImage c (Node n ns) =
    treeViewImage (c == Just n) n <->
    translateX 2 (vertCat $ map (treeImage c) ns)


--reply :: ReplyTo -> State -> IO (Either String ())
--reply replyTo _q@State{..} =
--    case getMessage (Z.label cursor) of
--        Just Message{..} -> do
--            x <- notmuchReply replyTo ("id:" <> unMessageID messageId)
--            edit x
--            return $ Right ()
--        Nothing ->
--            return $ Left "no message"

--edit :: LBS.ByteString -> IO ()
--edit draft = do
--    editor <- getEnv "EDITOR"
--    logname <- getEnv "LOGNAME"
--    tmpdir <- getTemporaryDirectory
--
--    let template = logname ++ "_much_draft_XXX.email"
--
--    bracket (openTempFile tmpdir template) cleanup $ \(path, h) -> do
--        LBS.hPut h draft
--        hClose h
--        --hFlush h
--        system (editor ++ " " ++ path)
--        return ()
--  where
--    cleanup (path, h) = do
--        removeFile path
--        hClose h

replyToAll Message{..} q = do
    editor <- getEnv "EDITOR"
    logname <- getEnv "LOGNAME"
    tmpdir <- getTemporaryDirectory

    let template = logname ++ "_much_draft_XXX.email"

    bracket (openTempFile tmpdir template) cleanup $ \(path, draftH) -> do
        (_, _, _, procH) <-
            withFile "/dev/null" ReadMode $ \devnull ->
                createProcess
                    (proc "notmuch" [ "reply" , "id:" <> unMessageID messageId ])
                        { std_in = UseHandle devnull
                        , std_out = UseHandle draftH
                        }
        hClose draftH
        waitForProcess procH
        code <- system (editor ++ " " ++ path)
        return q { message = show code }
  where
    cleanup = removeFile . fst


replyToAll2 = do
    editor <- getEnv "EDITOR"
    logname <- getEnv "LOGNAME"
    tmpdir <- getTemporaryDirectory

    let template = logname ++ "_much_draft_XXX.email"

    let msgId = "20141227121335.701B43F@mx2.informatik.uni-stuttgart.de"

    bracket (openTempFile tmpdir template) cleanup $ \(path, draftH) -> do
        (_, _, _, procH) <-
            withFile "/dev/null" ReadMode $ \devnull ->
                createProcess
                    (proc "notmuch" [ "reply" , "id:" <> msgId ])
                        { std_in = UseHandle devnull
                        , std_out = UseHandle draftH
                        }
        hClose draftH
        waitForProcess procH
        code <- system (editor ++ " " ++ path)
        print code
        --return q { message = show code }
  where
    cleanup = removeFile . fst



--    (_, Just hout, _, ph) <- createProcess (proc "notmuch" args)
--                                          { std_out = CreatePipe }
--    output <- LBS.hGetContents hout
--
--
--    withForkWait (evaluate $ rnf output) $ \waitOut -> do
--
--      ---- now write any input
--      --unless (null input) $
--      --  ignoreSigPipe $ hPutStr inh input
--      -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
--      --ignoreSigPipe $ hClose inh
--
--      -- wait on the output
--      waitOut
--      hClose hout
--
--    -- wait on the process
--    _ex <- waitForProcess ph
--    --return (ex, output)
--
--    --case ex of
--    --    ExitSuccess   -> return output
--    --    ExitFailure r -> processFailedException "readProcess" cmd args r
--
--    return output
