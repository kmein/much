{-# LANGUAGE LambdaCase #-}

module Much.TreeView.Types where

import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import Notmuch.Message
import Notmuch.SearchResult


type LineNr = Int


data TreeView
    = TVMessage Message
    | TVMessageHeaderField Message (CI.CI T.Text)
    | TVMessagePart Message MessagePart
    | TVMessageQuoteLine Message MessagePart LineNr String
    | TVMessageRawLine Message MessagePart LineNr String
    | TVMessageLine Message MessagePart LineNr String
    | TVSearch String
    | TVSearchResult SearchResult
  deriving (Show)


instance Eq TreeView where
    x1 == x2 = treeViewId x1 == treeViewId x2


data TreeViewId
    = TVIDMessage T.Text
    | TVIDMessageHeaderField T.Text T.Text
    | TVIDMessagePart T.Text Int
    | TVIDMessageLine T.Text Int Int
    | TVIDSearch T.Text
    | TVIDSearchResult T.Text
  deriving (Eq,Show)


treeViewId :: TreeView -> TreeViewId
treeViewId = \case
    TVMessage m ->
        TVIDMessage (fromMessage m)

    TVMessageHeaderField m mhf ->
        TVIDMessageHeaderField (fromMessage m) (CI.foldedCase mhf)

    TVMessagePart m mp ->
        TVIDMessagePart (fromMessage m) (partID mp)

    TVMessageLine m mp lineNr _ ->
        TVIDMessageLine (fromMessage m) (partID mp) lineNr

    TVMessageQuoteLine m mp lineNr _ ->
        TVIDMessageLine (fromMessage m) (partID mp) lineNr

    TVMessageRawLine m mp lineNr _ ->
        TVIDMessageLine (fromMessage m) (partID mp) lineNr

    TVSearch s ->
        TVIDSearch (T.pack s)

    TVSearchResult sr ->
        TVIDSearchResult (T.pack $ unThreadID $ searchThread sr)

  where
    fromMessage = T.pack . unMessageID . messageId
