module TreeSearch where

import Data.Tree.Zipper


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
                Just x'' -> Just x''
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



findNextN :: Eq a => Int -> TreePos Full a -> TreePos Full a
findNextN n loc
    | n <= 0 = loc
    | otherwise =
        maybe loc (findNextN $ n - 1) (skipSame findNext loc)


findPrevN :: Eq a => Int -> TreePos Full a -> TreePos Full a
findPrevN n loc
    | n <= 0 = loc
    | otherwise =
        maybe loc (findPrevN $ n - 1) (skipSame findPrev loc)



findParent :: (a -> Bool) -> TreePos Full a -> Maybe (TreePos Full a)
findParent p loc =
    if p (label loc)
        then Just loc
        else parent loc >>= findParent p


linearPos :: TreePos Full a -> Int
linearPos =
    rec 0
  where
    rec i loc = case findPrev loc of
        Just loc' -> rec (i + 1) loc'
        Nothing -> i



skipSame
    :: Eq a =>
      (TreePos Full a -> Maybe (TreePos Full a)) ->
      TreePos Full a ->
      Maybe (TreePos Full a)
skipSame next loc =
    case next loc of
        Nothing -> Nothing
        Just loc' ->
            if label loc' == label loc
                then skipSame next loc'
                else Just loc'
