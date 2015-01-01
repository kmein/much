{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module TreeView
    ( TreeView (..)
    , getMessage
    , isTVSearchResult
    , fromSearchResults
    , fromMessageForest
    , fromMessageTree
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
    TVMessage m1 == TVMessage m2 =
        m1 == m2

    TVMessageHeaderField m1 mhf1 == TVMessageHeaderField m2 mhf2 =
        m1 == m2 && mhf1 == mhf2

    TVMessagePart m1 mp1 == TVMessagePart m2 mp2 =
        m1 == m2 && mp1 == mp2

    TVMessageLine m1 mp1 ln1 _s1 == TVMessageLine m2 mp2 ln2 _s2 =
        m1 == m2 && mp1 == mp2 && ln1 == ln2

    TVMessageQuoteLine m1 mp1 ln1 _s1 == TVMessageQuoteLine m2 mp2 ln2 _s2 =
        m1 == m2 && mp1 == mp2 && ln1 == ln2

    TVSearch s1 == TVSearch s2 =
        s1 == s2

    TVSearchResult s1 == TVSearchResult s2 =
        s1 == s2

    _ == _ = False


getMessage :: TreeView -> Maybe Message
getMessage = \case
    TVMessage m -> Just m
    TVMessageHeaderField m _ -> Just m
    TVMessagePart m _ -> Just m
    TVMessageQuoteLine m _ _ _ -> Just m
    TVMessageLine m _ _ _ -> Just m
    _ -> Nothing


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
xconvBody m = mconcat $ map (xconvPart m) (messageBody m)

xconvPart :: Message -> MessagePart -> Forest TreeView
xconvPart m p = xconvPartContent m p $ partContent p

xconvPartContent
  :: Message -> MessagePart -> MessageContent -> Forest TreeView
xconvPartContent m p = \case
    ContentText t ->
        map (xconvLine m p) $ zip [0..] (T.lines t)
    ContentMultipart parts ->
        map (xconvPart2 m) parts
        -- [Node (TVMessageLine m p 0 "ContentMultipart") []]
    ContentMsgRFC822 _ ->
        [Node (TVMessageLine m p 0 "ContentMsgRFC822") []]


xconvPart2 :: Message -> MessagePart -> Tree TreeView
xconvPart2 m p =
    Node (TVMessagePart m p) $ xconvPartContent m p (partContent p)


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
