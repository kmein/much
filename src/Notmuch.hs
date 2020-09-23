{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Notmuch where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Network.Mail.Mime as M
import Control.Concurrent
import Control.DeepSeq (rnf)
import Control.Exception
import Data.Aeson.Extends
import Data.Tree
import Notmuch.Class
import Notmuch.Message
import Notmuch.SearchResult
import Much.ParseMail (readMail)
import System.Exit
import System.IO
import System.Process
import Much.TagUtils


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


notmuch' :: [String] -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
notmuch' args = do
    (_, Just hout, Just herr, ph) <-
        createProcess (proc "notmuch" args)
            { std_out = CreatePipe
            , std_err = CreatePipe
            }
    out <- LBS.hGetContents hout
    err <- LBS.hGetContents herr

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


notmuchWithInput
    :: [String]
    -> LBS.ByteString
    -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
notmuchWithInput args input = do
    (Just hin, Just hout, Just herr, ph) <-
        createProcess (proc "notmuch" args)
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
    LBS.hPut hin input
    hClose hin

    out <- LBS.hGetContents hout
    err <- LBS.hGetContents herr

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


search :: [String] -> IO (Either String [SearchResult])
search args =
    eitherDecodeLenient' <$>
        notmuch ("search" : "--format=json" : "--format-version=2" : args)


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
 --       >>= return . eitherDecodeLenient'


notmuchShow :: String -> IO (Forest Message)
notmuchShow term = do
    c' <- notmuch [ "show", "--format=json", "--format-version=2"
                   , term ]
    -- TODO why head?
    return $ threadForest $ head $
        either error id (eitherDecodeLenient' c')


notmuchShowPart :: String -> Int -> IO (Either String MessagePart)
notmuchShowPart term partId = do
    -- TODO handle partId == 0 and partId > N
    (exitCode, out, err) <-
        notmuch' [ "show", "--format=json", "--format-version=2"
                 , "--part=" <> show partId
                 , term ]
    return $ case exitCode of
        ExitSuccess -> eitherDecodeLenient' out
        _ -> Left $ show exitCode <> ": " <> LBS8.unpack err


notmuchShowMail :: String -> IO (Either String M.Mail)
notmuchShowMail term =
    notmuch' [ "show", "--format=raw", "--format-version=2", term ]
    >>= return . \case
      (ExitSuccess, out, _) ->
          case LT.decodeUtf8' out of
              Right x -> Right (readMail $ LT.toStrict x)
              Left ex -> Left $ "meh" ++ show ex
      (exitCode, _, err) ->
          Left $ "notmuch failed with exit code " ++ show exitCode ++
                 ": " ++ LBS8.unpack err


notmuchTag :: HasNotmuchId a => [TagOp] -> a -> IO ()
notmuchTag tagOps x =
    notmuch ("tag" : tagOpsToArgs tagOps ++ [notmuchId x]) >> return ()
