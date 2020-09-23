{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Much.Action where

import Blessings.String
import Scanner
import Much.State
import Much.TagUtils
import Much.TreeSearch
import Much.TreeView
import Much.TreeZipperUtils
import qualified Data.Tree as Tree
import qualified Data.Tree.Zipper as Z
import qualified Notmuch
import qualified Notmuch.Message as Notmuch
import qualified Notmuch.SearchResult as Notmuch

displayKey :: String -> State -> IO State
displayKey s q = return q { flashMessage = Plain $ show s }


displayMouse :: Scan -> State -> IO State
displayMouse info q =
    return q { flashMessage = SGR [38,5,202] $ Plain $ show info }

defaultMouse1Click :: Monad m => Int -> State -> m State
defaultMouse1Click y q@State{..} = do
    let linearClickPos =
            let i = (y - length headBuffer + yoffset) - 1 {-zero-based-}
            in if 0 <= i && i < length treeBuffer
                then Just i
                else Nothing
    case linearClickPos of
        Nothing ->
            return q
                { flashMessage = Plain "nothing to click"
                }
        Just i ->
            return q
                { cursor = findNextN i $ Z.root cursor
                }


moveCursorDown :: Monad m => Int -> State -> m State
moveCursorDown n q@State{..} =
    let cursor' = findNextN n cursor
        q' = q { cursor = cursor' }
    in case botOverrun q' of
        0 -> return q'
        i -> moveTreeUp i q'


moveCursorUp :: Monad m => Int -> State -> m State
moveCursorUp n q@State{..} =
    let cursor' = findPrevN n cursor
        q' = q { cursor = cursor' }
    in case topOverrun q' of
        0 -> return q'
        i -> moveTreeDown i q'


moveTreeUp :: Monad m => Int -> State -> m State
moveTreeUp n q@State{..} =
    let q' = q { yoffset = min (length treeBuffer - 1) $ max 0 (yoffset + n) }
    in case topOverrun q' of
        0 -> return q'
        i -> moveCursorDown i q'


moveTreeDown :: Monad m => Int -> State -> m State
moveTreeDown n q@State{..} =
    let q' = q { yoffset = min (length treeBuffer - 1) $ max 0 (yoffset - n) }
    in case botOverrun q' of
        0 -> return q'
        i -> moveCursorUp i q'


moveTreeLeft :: Monad m => Int -> State -> m State
moveTreeLeft n q@State{..} =
    return q { xoffset = xoffset + n }

moveTreeRight :: Monad m => Int -> State -> m State
moveTreeRight n q@State{..} =
    return q { xoffset = max 0 (xoffset - n) }


moveToParent :: Monad m => State -> m State
moveToParent q@State{..} =
    case Z.parent cursor of
        Nothing -> return q { flashMessage = "cannot go further up" }
        Just cursor' ->
            let q' = q { cursor = cursor' }
            in case topOverrun q' of
                0 -> return q'
                i -> moveTreeDown i q'


moveCursorToUnread
    :: (Num a, Monad m, Eq a)
    => (Z.TreePos Z.Full TreeView -> Maybe (Z.TreePos Z.Full TreeView))
    -> (State -> a)
    -> (a -> State -> m State)
    -> State -> m State
moveCursorToUnread cursorMove getTreeMoveCount treeMove q@State{..} =
    case cursorMove cursor >>= rec of
        Just cursor' ->
            let q' = q { cursor = cursor' }
            in case getTreeMoveCount q' of
                0 -> return q'
                i -> treeMove i q'
        Nothing ->
            return q { flashMessage = "no unread message in sight" }
  where
    rec loc =
        if hasTag "unread" loc
            then Just loc
            else cursorMove loc >>= rec
    hasTag tag loc =
        case Z.label loc of
            TVSearchResult sr ->
                tag `elem` Notmuch.searchTags sr
            TVMessage m ->
                tag `elem` Notmuch.messageTags m
            _ ->
                False

moveCursorUpToPrevUnread :: Monad m => State -> m State
moveCursorUpToPrevUnread =
    moveCursorToUnread findPrev topOverrun moveTreeDown

moveCursorDownToNextUnread :: Monad m => State -> m State
moveCursorDownToNextUnread =
    moveCursorToUnread findNext botOverrun moveTreeUp


openFold :: State -> IO State
openFold q@State{..} =
    handle <$> loadSubForest (Z.label cursor)
  where
    handle = \case
        Left err ->
            q { flashMessage = SGR [31] $ Plain err }
        Right sf ->
            q { cursor = Z.modifyTree (setSubForest sf) cursor }

closeFold :: State -> IO State
closeFold q@State{..} =
    let sf = unloadSubForest (Z.tree cursor)
     in return q { cursor = Z.modifyTree (setSubForest sf) cursor }

toggleFold :: State -> IO State
toggleFold q@State{..} =
    if hasUnloadedSubForest (Z.tree cursor)
        then openFold q
        else closeFold q


toggleTagAtCursor :: Tag -> State -> IO State
toggleTagAtCursor tag q@State{..} = case Z.label cursor of

    TVSearchResult sr -> do
        let tagOp =
                if tag `elem` Notmuch.searchTags sr
                    then DelTag
                    else AddTag
            tagOps = [tagOp tag]
        Notmuch.notmuchTag tagOps sr
        let cursor' = Z.modifyTree (patchTreeTags tagOps) cursor
        return q { cursor = cursor' }

    TVMessage m -> do
        let tagOp =
                if tag `elem` Notmuch.messageTags m
                    then DelTag
                    else AddTag
            tagOps = [tagOp tag]
        Notmuch.notmuchTag tagOps m
        let cursor' =
                -- TODO this needs a nice name
                modifyFirstParentLabelWhere isTVSearchResult f $
                Z.modifyLabel f cursor
            f = patchTags tagOps
        return q { cursor = cursor' }

    _ -> return q { flashMessage = "nothing happened" }


topOverrun :: State -> Int
topOverrun State{..} =
    max 0 (- (linearPos cursor - yoffset))


botOverrun :: State -> Int
botOverrun State{..} =
    max 0 (linearPos cursor - yoffset - (screenHeight - length headBuffer - 1))


setSubForest :: Tree.Forest a -> Tree.Tree a -> Tree.Tree a
setSubForest sf t = t { Tree.subForest = sf }
