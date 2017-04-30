{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TagUtils where

import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Char
import Data.List.Split (wordsBy)
import Data.Tree
import Notmuch.Message
import Notmuch.SearchResult
import TreeView.Types


type Tag = T.Text


data TagOp = AddTag Tag | DelTag Tag


parseTags :: String -> [Tag]
parseTags =
    mconcat . map (map T.pack . wordsBy isSpace . takeWhile (/='#')) . lines


diffTags :: [Tag] -> [Tag] -> [TagOp]
diffTags old new =
    let oldTags = Set.fromList old
        newTags = Set.fromList new
    in (map DelTag $ Set.toList $ oldTags `Set.difference` newTags) ++
       (map AddTag $ Set.toList $ newTags `Set.difference` oldTags)


patchRootLabelTags :: [TagOp] -> Tree TreeView -> Tree TreeView
patchRootLabelTags tagOps x =
    x { rootLabel = patchTags tagOps $ rootLabel x }


patchTreeTags :: [TagOp] -> Tree TreeView -> Tree TreeView
patchTreeTags tagOps =
    fmap (patchTags tagOps)


tagOpsToArgs :: [TagOp] -> [String]
tagOpsToArgs = map $ \case
    AddTag t -> '+' : T.unpack t
    DelTag t -> '-' : T.unpack t


patchTags :: [TagOp] -> TreeView -> TreeView
patchTags tagOps = \case
    TVSearchResult sr ->
        TVSearchResult sr { searchTags = foldr applyTagOp (searchTags sr) tagOps }
    TVMessage m ->
        TVMessage m { messageTags = foldr applyTagOp (messageTags m) tagOps }
    x -> x -- nop


applyTagOp :: TagOp -> [Tag] -> [Tag]
applyTagOp = \case
    AddTag t -> (t:)
    DelTag t -> filter (/=t)
