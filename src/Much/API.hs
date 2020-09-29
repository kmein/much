{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Much.API (module Much.API) where

import Control.Concurrent
import Control.Exception (catch, finally, throwIO)
import Control.Monad.IO.Class
import Data.Function ((&))
import Data.Proxy (Proxy)
import Much.API.Config as Much.API
import Much.Event
import Much.State
import Much.TreeView
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp
import Notmuch.Class
import Notmuch.Message
import Servant
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (removeLink)
import qualified Data.Tree.Zipper as Z


type API =
    "current" :> (
        "part" :> Get '[PlainText] String
      :<|>
        "query" :> Get '[PlainText] String
    )

api :: Proxy API
api = Proxy

main :: Config -> (Event -> IO ()) -> IO ()
main Config{socketPath} putEvent = do
    sock <- socket AF_UNIX Stream defaultProtocol
    removeIfExists socketPath
    bind sock $ SockAddrUnix socketPath
    listen sock maxListenQueue
    let settings = defaultSettings
          & setPort 0
    runSettingsSocket settings sock app `finally` closeSocket sock
  where
    app :: Application
    app = serve api server

    server :: Server API
    server =
        servePart
      :<|>
        serveQuery

    servePart :: Handler String
    servePart = do
      q <- liftIO getState
      case searchPart (Z.label (cursor q)) of
        Just i -> return (show i <> "\n")
        Nothing -> throwError err404

    serveQuery :: Handler String
    serveQuery = do
      q <- liftIO getState
      return $ (searchQuery $ Z.label $ cursor q) <> "\n"

    getState :: IO State
    getState = do
      v <- newEmptyMVar
      putEvent $ EStateGet $ putMVar v
      takeMVar v

    searchPart :: TreeView -> Maybe Int
    searchPart = \case
        TVMessagePart _ i -> Just (partID i)
        _ -> Nothing

    searchQuery :: TreeView -> String
    searchQuery = \case
        TVMessage m                 -> notmuchId m
        TVMessageHeaderField m _    -> notmuchId m
        TVMessagePart m _           -> notmuchId m
        TVMessageQuoteLine m _ _ _  -> notmuchId m
        TVMessageLine m _ _ _       -> notmuchId m
        TVSearch s                  -> s
        TVSearchResult r            -> notmuchId r


removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeLink fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e


closeSocket :: Socket -> IO ()
closeSocket sock = do
  name <- getSocketName sock
  close sock
  case name of
    SockAddrUnix path -> removeIfExists path
    _ -> return ()
