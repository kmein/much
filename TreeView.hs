{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module TreeView
    ( TreeView (..)
    , getMessage
    , isTVMessage
    , isTVSearchResult
    , fromSearchResults
    , fromMessageForest
    , fromMessageTree
    , treeViewId
    ) where


import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import Data.Monoid
import Data.Tree
import Notmuch.Message
import Notmuch.SearchResult


type LineNr = Int


data TreeView
    = TVMessage Message
    | TVMessageHeaderField Message (CI.CI T.Text)
    | TVMessagePart Message MessagePart
    | TVMessageQuoteLine Message MessagePart LineNr String
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

    TVSearch s ->
        TVIDSearch (T.pack s)

    TVSearchResult sr ->
        TVIDSearch (T.pack $ unThreadID $ searchThread sr)

  where
    fromMessage = T.pack . unMessageID . messageId


getMessage :: TreeView -> Maybe Message
getMessage = \case
    TVMessage m -> Just m
    TVMessageHeaderField m _ -> Just m
    TVMessagePart m _ -> Just m
    TVMessageQuoteLine m _ _ _ -> Just m
    TVMessageLine m _ _ _ -> Just m
    _ -> Nothing


isTVMessage :: TreeView -> Bool
isTVMessage = \case
    TVMessage _ -> True
    _ -> False


isTVSearchResult :: TreeView -> Bool
isTVSearchResult (TVSearchResult _) = True
isTVSearchResult _ = False


fromSearchResults :: String -> [SearchResult] -> Tree TreeView
fromSearchResults query =
    Node (TVSearch query) . map (\r -> Node (TVSearchResult r) [])


fromMessageForest :: Forest Message -> Forest TreeView
fromMessageForest = map fromMessageTree


fromMessageTree :: Tree Message -> Tree TreeView
fromMessageTree (Node m ms) =
    Node m' ms'
  where

    m' :: TreeView
    m' = TVMessage m

    ms' :: Forest TreeView
    ms' = if isOpen m
              then xconvHead m <> xconvBody m <> map fromMessageTree ms
              else map fromMessageTree ms


xconvHead :: Message -> Forest TreeView
xconvHead m =
    map conv [ "From", "To" ]
    -- TODO add Subject if it differs from thread subject
  where
    conv k =
      Node (TVMessageHeaderField m k) []


xconvBody :: Message -> Forest TreeView
xconvBody m = map (xconvPart m) (messageBody m)


xconvPart :: Message -> MessagePart -> Tree TreeView
xconvPart m p =
    Node (TVMessagePart m p) contents
  where
    contents = case partContent p of
        ContentText t ->
            map (xconvLine m p) $ zip [0..] (T.lines t)
        ContentMultipart parts ->
            map (xconvPart m) parts
        ContentMsgRFC822 _ ->
            []


xconvLine
  :: Message -> MessagePart -> (LineNr, T.Text) -> Tree TreeView
xconvLine m p (i, s) =
    Node (ctor m p i $ T.unpack s) []
  where
    ctor =
        if isQuoteLine s
            then TVMessageQuoteLine
            else TVMessageLine


isQuoteLine :: T.Text -> Bool
isQuoteLine s0 = do
    let s = T.stripStart s0

    -- /^\s*>/
    not (T.null s) && T.head s == '>'
