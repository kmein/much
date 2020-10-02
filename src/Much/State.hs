{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Much.State where

import Blessings
import Data.Aeson
import Data.Default
import Data.Time
import GHC.Generics
import Much.TreeView (TreeView(TVSearch))
import Scanner
import System.Posix.Signals
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Tree as Tree
import qualified Data.Tree.Zipper as Z
import qualified Much.API.Config
import qualified Notmuch.Message as Notmuch

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
    , query :: String
    , keymap :: String -> State -> IO State
    , mousemap :: Scan -> State -> IO State
    , tagSymbols :: M.Map T.Text T.Text
    , colorConfig :: ColorConfig (Blessings String -> Blessings String)
    , apiConfig :: Much.API.Config.Config
    , attachmentOverwrite :: Bool
    , attachmentDirectory :: FilePath
    , attachmentFileName :: Notmuch.Message -> Notmuch.MessagePart -> FilePath
    }

instance Show (State -> IO ()) where
    show = const "λ"

data ColorConfig a = ColorConfig
    { alt :: a
    , search :: a
    , focus :: a
    , quote :: a
    , boring :: a
    , prefix :: a
    , date :: a
    , tags :: a
    , unreadSearch :: a
    , unreadMessage :: a
    , boringMessage :: a
    , tagMap :: M.Map T.Text a
    , unprintableFocus :: a
    , unprintableNormal :: a
    } deriving (Generic, Show)

instance Default (ColorConfig (Blessings String -> Blessings String)) where
  def = ColorConfig
    { tagMap = M.fromList
        [ ("killed", SGR [38,5,088])
        , ("star", SGR [38,5,226])
        , ("draft", SGR [38,5,202])
        ]
    , alt = SGR [38,5,182]
    , search = SGR [38,5,162]
    , focus = SGR [38,5,160]
    , unprintableFocus = SGR [38,5,204]
    , unprintableNormal = SGR [35]
    , quote = SGR [38,5,242]
    , boring = SGR [38,5,240]
    , prefix = SGR [38,5,235]
    , date = SGR [38,5,071]
    , tags = SGR [38,5,036]
    , boringMessage = SGR [38,5,023]
    , unreadMessage = SGR [38,5,117]
    , unreadSearch = SGR [38,5,250]
    }

instance FromJSON a => FromJSON (ColorConfig a)

instance Default State where
  def = State
      { cursor = Z.fromTree (Tree.Node (TVSearch "<emptyState>") [])
      , xoffset = 0
      , yoffset = 0
      , flashMessage = "Welcome to much; quit with ^C"
      , screenWidth = 0
      , screenHeight = 0
      , headBuffer = []
      , treeBuffer = []
      , now = UTCTime (fromGregorian 1984 5 23) 49062
      , signalHandlers = []
      , query = "tag:inbox AND NOT tag:killed"
      , keymap = const return
      , mousemap = const return
      , colorConfig = def
      , tagSymbols = M.empty
      , apiConfig = def
      , attachmentOverwrite = False
      , attachmentDirectory = "/tmp"
      , attachmentFileName = \message part ->
          case Notmuch.partContentFilename part of
            Just partFileName -> T.unpack partFileName
            Nothing ->
              "much_"
              <> formatTime defaultTimeLocale "%s" (Notmuch.messageTime message)
              <> "_"
              <> show (Notmuch.partID part)
              <> extension (Notmuch.partContentType part)
      }

extension :: CI.CI T.Text -> String
extension "text/html" = ".html"
extension "text/plain" = ".txt"
extension _ = ""
