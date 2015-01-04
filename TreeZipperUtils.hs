module TreeZipperUtils where

import Data.Tree
import Data.Tree.Zipper

-- Return loc (as parent-like structure) and  parents.
path :: TreePos Full a -> [(Forest a, a, Forest a)]
path loc = toParent loc : parents loc

-- Return parent stack compatible form of loc.
toParent :: TreePos Full a -> (Forest a, a, Forest a)
toParent loc = (before loc, label loc, after loc)
