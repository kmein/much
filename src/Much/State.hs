{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Much.State where

import Blessings
import Data.Aeson
import Data.Default
import Data.Functor.Identity
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
    , aliases :: M.Map T.Text T.Text
    , colorConfig :: ColorConfig Identity
    , apiConfig :: Much.API.Config.Config
    , attachmentOverwrite :: Bool
    , attachmentDirectory :: FilePath
    , attachmentFileName :: Notmuch.Message -> Notmuch.MessagePart -> FilePath
    }

instance Show (State -> IO ()) where
    show = const "λ"

data ColorConfig f = ColorConfig
    { alt :: f Pm
    , search :: f Pm
    , focus :: f Pm
    , quote :: f Pm
    , boring :: f Pm
    , prefix :: f Pm
    , date :: f Pm
    , tags :: f Pm
    , unreadSearch :: f Pm
    , unreadMessage :: f Pm
    , boringMessage :: f Pm
    , tagMap :: f (M.Map T.Text (f Pm))
    , unprintableFocus :: f Pm
    , unprintableNormal :: f Pm
    } deriving (Generic)

instance Applicative f => Default (ColorConfig f) where
  def = ColorConfig
    { tagMap = pure $ M.fromList
        [ ("killed", pure [38,5,088])
        , ("star", pure [38,5,226])
        , ("draft", pure [38,5,202])
        ]
    , alt = pure [38,5,182]
    , search = pure [38,5,162]
    , focus = pure [38,5,160]
    , unprintableFocus = pure [38,5,204]
    , unprintableNormal = pure [35]
    , quote = pure [38,5,242]
    , boring = pure [38,5,240]
    , prefix = pure [38,5,235]
    , date = pure [38,5,071]
    , tags = pure [38,5,036]
    , boringMessage = pure [38,5,023]
    , unreadMessage = pure [38,5,117]
    , unreadSearch = pure [38,5,250]
    }

instance FromJSON (ColorConfig Maybe)

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
      , aliases = M.empty
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
