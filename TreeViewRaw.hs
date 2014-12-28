{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TreeViewRaw (renderTreeView) where

import qualified Notmuch.Message as Notmuch
import qualified Notmuch.SearchResult as Notmuch
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Monoid
import Data.Tree
import Trammel
import TreeView
import Utils (padl)


renderTreeView :: TreeView -> Tree TreeView -> [Trammel String]
renderTreeView cur _loc@(Node label children) =
    [ colorize $ renderTreeView1 hasFocus label ] ++
    concatMap (map ("  "<>) . renderTreeView cur) children
  where
    hasFocus = cur == label
    colorize s =
        if hasFocus
            then SGR [31] s
            else s


renderTreeView1 :: Bool -> TreeView -> Trammel String
renderTreeView1 hasFocus = \case

    TVSearch s ->
        Plain s

    TVSearchResult sr ->
        let c = case (hasFocus, "unread" `elem` Notmuch.searchTags sr) of
                    (False, False) -> SGR [38,5,240]
                    (False,  True) -> SGR [38,5,250]
                    (True,  False) -> SGR [38,5,088]
                    (True,   True) -> SGR [38,5,160]
        in c $
        Plain (
            (padl 11 ' ' $ T.unpack $ Notmuch.searchDateRel sr)
            ++ " (" ++ (show $ Notmuch.searchMatched sr) ++ ")  "
            ++ (T.unpack $ Notmuch.searchSubject sr)
            ++ " "
            )
        <>
        mconcat (L.intersperse " " (map (SGR [38,5,036] . Plain . T.unpack) $ Notmuch.searchTags sr))

    TVMessage m ->
        let c = case (hasFocus, "unread" `elem` Notmuch.messageTags m) of
                    (False, False) -> SGR [38,5,240]
                    (False,  True) -> SGR [38,5,250]
                    (True,  False) -> SGR [38,5,088]
                    (True,   True) -> SGR [38,5,160]
        in c $
        Plain (
            (Notmuch.unMessageID $ Notmuch.messageId m)
            ++ " "
            ++ T.unpack (T.intercalate (T.pack ",") $ Notmuch.messageTags m)
            )

    TVMessageHeaderField m fieldName -> Plain $
        let k = T.unpack $ CI.original fieldName
            v = maybe "nothing"
                      T.unpack
                      (M.lookup fieldName $ Notmuch.messageHeaders m)
        in k ++ ": " ++ v

    TVMessagePart _ p -> Plain $
        "part#"
        ++ (show $ Notmuch.partID p)
        ++ " "
        ++ (T.unpack $ CI.original $ Notmuch.partContentType p)

    TVMessageLine _ _ _ s ->
        Plain s
