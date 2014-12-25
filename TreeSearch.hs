module TreeSearch where

import Data.Tree.Zipper
import Data.Maybe

-- findTree :: PosType t => (a -> Bool) -> TreePos t a -> Maybe (TreePos t a)
findTree :: (a -> Bool) -> TreePos Full a -> Maybe (TreePos Full a)
findTree p loc = if p (label loc)
    then Just loc
    else depthFirst loc >>= findTree p
        
depthFirst :: TreePos Full a -> Maybe (TreePos Full a)
depthFirst loc = case firstChild loc of
    Just x -> Just x
    Nothing -> case next loc of
        Just x -> Just x
        Nothing -> case parent loc of
            Just x -> next x
            Nothing -> Nothing
