{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Notmuch where

import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent
import Control.DeepSeq (rnf)
import Control.Exception
import Data.Aeson
import Data.Monoid
import Data.Tree
import Notmuch.Message
import Notmuch.SearchResult
import System.IO
import System.Process


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




notmuch :: [String] -> IO LBS.ByteString
notmuch args = do
    (_, Just hout, _, ph) <- createProcess (proc "notmuch" args)
                                          { std_out = CreatePipe }
    output <- LBS.hGetContents hout


    withForkWait (evaluate $ rnf output) $ \waitOut -> do

      ---- now write any input
      --unless (null input) $
      --  ignoreSigPipe $ hPutStr inh input
      -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
      --ignoreSigPipe $ hClose inh

      -- wait on the output
      waitOut
      hClose hout

    -- wait on the process
    _ex <- waitForProcess ph
    --return (ex, output)

    --case ex of
    --    ExitSuccess   -> return output
    --    ExitFailure r -> processFailedException "readProcess" cmd args r

    return output



--notmuch' args = do
--    (_, Just hout, _, _) <- createProcess (proc "notmuch" args)
--                                          { std_out = CreatePipe }
--    BS.hGetContents hout


search :: String -> IO (Either String [SearchResult])
search term =
    notmuch [ "search", "--format=json", "--format-version=2", term ]
        >>= return . eitherDecode'


data ReplyTo = ToAll | ToSender
instance Show ReplyTo where
    show ToAll = "all"
    show ToSender = "sender"

--notmuchReply :: String -> IO (Either String [SearchResult])
notmuchReply :: ReplyTo -> String -> IO LBS.ByteString
notmuchReply replyTo term =
    notmuch
        [ "reply"
        , "--reply-to=" ++ show replyTo
        , term
        ]
 --       >>= return . eitherDecode'


notmuchShow :: String -> IO (Forest Message)
notmuchShow term = do
    c' <- notmuch [ "show", "--format=json", "--format-version=2"
                   , term ]
    -- TODO why head?
    return $ threadForest $ head $
        either error id (eitherDecode' c')


setTag :: String -> String -> IO LBS.ByteString
setTag tag i = do
    notmuch [ "tag", "+" <> tag , "id:" <> i ]


unsetTag :: String -> String -> IO LBS.ByteString
unsetTag tag i = do
    notmuch [ "tag", "-" <> tag , "id:" <> i ]
