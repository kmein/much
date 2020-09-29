{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Much.State where

import Blessings.String (Blessings)
import Data.Aeson
import Data.Time
import GHC.Generics
import Much.TreeView (TreeView)
import Scanner
import System.Posix.Signals
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Tree.Zipper as Z

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
    } deriving (Generic, Show)

instance FromJSON a => FromJSON (ColorConfig a)
