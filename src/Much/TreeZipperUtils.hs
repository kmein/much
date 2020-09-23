module Much.TreeZipperUtils where

import Data.Maybe
import Data.Tree
import Data.Tree.Zipper

-- Return loc (as parent-like structure) and  parents.
path :: TreePos Full a -> [(Forest a, a, Forest a)]
path loc = toParent loc : parents loc

-- Return parent stack compatible form of loc.
toParent :: TreePos Full a -> (Forest a, a, Forest a)
toParent loc = (before loc, label loc, after loc)


modifyFirstParentLabelWhere
    :: (a -> Bool)
    -> (a -> a)
    -> TreePos Full a
    -> TreePos Full a
modifyFirstParentLabelWhere p f loc0 =
    case parent loc0 of
        Nothing -> loc0
        Just loc0' -> go (byChildIndex loc0) loc0'
  where

    go rewind loc =
        if p (label loc)
            then
                rewind (modifyLabel f loc)
            else
                case parent loc of
                    Nothing -> rewind loc
                    Just loc' ->
                        go (rewind . byChildIndex loc) loc'

    -- generator for a rewind step
    byChildIndex :: TreePos Full a -> (TreePos Full a -> TreePos Full a)
    byChildIndex loc =
        -- The use of fromJust is safe here because we're only modifying
        -- labels and not the tree structure and thus the index is valid.
        fromJust . childAt (childIndex loc)


-- XXX This could be named more general, like countPrevSiblings?
-- XXX Can we kill the recursion?
childIndex :: TreePos Full a -> Int
childIndex =
    go 0
  where
    go index =
        maybe index (go $ index + 1) . prev
