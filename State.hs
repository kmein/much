module State where

import Blessings.String (Blessings)
import Data.Time
import qualified Data.Tree.Zipper as Z
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
    }
