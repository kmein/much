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
        Nothing -> parentWithNext loc
  where
    parentWithNext x =
        case parent x of
            Nothing -> Nothing
            Just x' -> case next x' of
                Just x' -> Just x'
                Nothing -> parentWithNext x'


findNext :: TreePos Full a -> Maybe (TreePos Full a)
findNext = depthFirst


findPrev :: TreePos Full a -> Maybe (TreePos Full a)
findPrev loc =
    case prev loc of
        Just x -> trans_lastChild x
        Nothing -> case parent loc of
            Just x -> Just x
            Nothing -> Nothing
  where
    trans_lastChild x =
        case lastChild x of
            Nothing -> Just x
            Just x' -> trans_lastChild x'


findParent :: (a -> Bool) -> TreePos Full a -> Maybe (TreePos Full a)
findParent p loc =
    if p (label loc)
        then Just loc
        else parent loc >>= findParent p
