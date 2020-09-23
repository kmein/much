module State where

import Blessings.String (Blessings)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import qualified Data.Tree.Zipper as Z
import Scanner
import System.Posix.Signals
import TreeView (TreeView)

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
    , keymap :: String -> State -> IO State
    , mousemap :: Scan -> State -> IO State
    , tagSymbols :: [(T.Text, T.Text)]
    , colorConfig :: ColorConfig
    }

data ColorConfig = ColorConfig
    { alt :: Blessings String -> Blessings String
    , search :: Blessings String -> Blessings String
    , focus :: Blessings String -> Blessings String
    , quote :: Blessings String -> Blessings String
    , boring :: Blessings String -> Blessings String
    , prefix :: Blessings String -> Blessings String
    , date :: Blessings String -> Blessings String
    , tags :: Blessings String -> Blessings String
    , unreadSearch :: Blessings String -> Blessings String
    , unreadMessage :: Blessings String -> Blessings String
    , boringMessage :: Blessings String -> Blessings String
    , tagMap :: [(T.Text, Blessings String -> Blessings String)]
    }
