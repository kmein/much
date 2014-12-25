{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module ThreadView where

import Data.Default
import Graphics.Vty

import Data.List

--import Data.Aeson
--import Data.List.Split
--import Data.Attoparsec.ByteString hiding (string)
--import Data.Maybe
import Data.Monoid
--import Data.String
--import Data.Traversable
import Data.Tree
--import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as LBS
--import qualified Data.ByteString.Char8 as BS8
--import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
--import qualified Data.Text.Encoding as T
--import qualified Data.Text.IO as T
--import Data.Version (Version(..), parseVersion)
--import System.Process
--import System.IO
--import qualified Data.Map as M

import Notmuch.Message
import Notmuch.SearchResult
import Safe


type LineNr = Int


data ThreadView
    = TVMessage Message
    | TVMessagePart Message MessagePart
    | TVMessageLine Message MessagePart LineNr String
    | TVSearch String
    | TVSearchResult SearchResult
  deriving (Show)

instance Eq ThreadView where
    TVMessage m1 == TVMessage m2 =
        m1 == m2

    TVMessagePart m1 mp1 == TVMessagePart m2 mp2 =
        m1 == m2 && mp1 == mp2

    TVMessageLine m1 mp1 ln1 _s1 == TVMessageLine m2 mp2 ln2 _s2 =
        m1 == m2 && mp1 == mp2 && ln1 == ln2

    TVSearch s1 == TVSearch s2 =
        s1 == s2

    TVSearchResult s1 == TVSearchResult s2 =
        s1 == s2

    _ == _ = False


isTVSearchResult :: ThreadView -> Bool
isTVSearchResult (TVSearchResult _) = True
isTVSearchResult _ = False


describe :: ThreadView -> String
describe (TVMessage m) = "TVMessage " <> unMessageID (messageId m)
describe (TVMessagePart m p) = "TVMessagePart " <> (unMessageID $ messageId m) <> " " <> show (partID p)
describe (TVMessageLine _ _ _ s) = "TVMessageLine " <> show s
describe (TVSearch s) = "TVSearch " <> show s
describe (TVSearchResult sr) = "TVSearchResult " <> show (searchTotal sr)


findMessage :: MessageID -> Tree ThreadView -> Maybe ThreadView
findMessage i =
    find p . flatten
  where
    p (TVMessage m) = i == messageId m
    p _ = False

findTV :: ThreadView -> Tree ThreadView -> Maybe ThreadView
findTV x =
    find (==x) . flatten


fromSearchResults :: String -> [SearchResult] -> Tree ThreadView
fromSearchResults query =
    Node (TVSearch query) . map (\r -> Node (TVSearchResult r) [])


fromMessageTree :: Tree Message -> Tree ThreadView
fromMessageTree (Node m ms) =
    Node m' ms'
  where

    m' :: ThreadView
    m' = TVMessage m

    ms' :: Forest ThreadView
    ms' = if isOpen m
              then xconvBody m <> map fromMessageTree ms
              else map fromMessageTree ms

xconvBody :: Message -> Forest ThreadView
xconvBody m = mconcat $ map (xconvPart m) (messageBody m)

xconvPart :: Message -> MessagePart -> Forest ThreadView
xconvPart m p = xconvPartContent m p $ partContent p

xconvPartContent
  :: Message -> MessagePart -> MessageContent -> Forest ThreadView
xconvPartContent m p = \case
    ContentText t ->
        map (xconvLine m p) $ zip [0..] (T.lines t)
    ContentMultipart parts ->
        map (xconvPart2 m) parts
        -- [Node (TVMessageLine m p 0 "ContentMultipart") []]
    ContentMsgRFC822 _ ->
        [Node (TVMessageLine m p 0 "ContentMsgRFC822") []]


xconvPart2 :: Message -> MessagePart -> Tree ThreadView
xconvPart2 m p =
    Node (TVMessagePart m p) $ xconvPartContent m p (partContent p)


xconvLine
  :: Message -> MessagePart -> (LineNr, T.Text) -> Tree ThreadView
xconvLine m p (i, s) =
    Node (TVMessageLine m p i $ T.unpack s) []



threadViewImage :: Bool -> ThreadView -> Image
threadViewImage hasFocus = \case
    TVMessage m ->
        let col = if isOpen m then om else cm
        in
        string col (unMessageID $ messageId m)
        <|>
        translateX 1 (
          horizCat $
              intersperse (string col ", ") $
              map (text' tagColor) $
              messageTags m
        )

    TVMessagePart _ p ->
        string mp "TVMessagePart"
          <|> translateX 1 (string mp $ show $ partID p)
          <|> translateX 1 (string mp $ show $ partContentType p)

    TVMessageLine _ _ _ s ->
        string ml s

    TVSearch s ->
        string sColor s

    TVSearchResult sr -> do
        let ThreadID tid = searchThread sr
        --string srColor tid
        -- <|>
        --translateX 1
        (string srColor $ padl 11 ' ' $ T.unpack $ searchDateRel sr)
        <|>
        string srColor " ("
        <|>
        (string srColor $ show $ searchMatched sr)
        <|>
        string srColor ")"
        <|>
        string srColor "  "
        -- <|>
        -- (string srColor $ show $ searchTime sr)
        <|>
        (string srColor $ T.unpack $ searchSubject sr)
  where
    --c1 = if hasFocus then c1_focus else c1_nofocus
    --c1_nofocus = withForeColor def $ Color240 $ -16 + 238
    --c1_focus = withForeColor def $ Color240 $ -16 + 244
    --c2 = withForeColor def $ Color240 $ -16 + 106
    --c3 = withForeColor def $ Color240 $ -16 + 199

    tagColor = if hasFocus then tagColor_y else tagColor_n
    tagColor_y = withForeColor def $ color 230
    tagColor_n = withForeColor def $ color 200

    cm = if hasFocus then cm_y else cm_n
    cm_y = withForeColor def $ color 46
    cm_n = withForeColor def $ color 22

    om = if hasFocus then om_y else om_n
    om_y = withForeColor def $ color 82
    om_n = withForeColor def $ color 58

    ml = if hasFocus then ml_y else ml_n
    ml_y = withForeColor def $ color 226
    ml_n = withForeColor def $ color 202

    --ph = if hasFocus then ph_y else ph_n
    --ph_y = withForeColor def $ color 241
    --ph_n = withForeColor def $ color 235

    mp = if hasFocus then mp_y else mp_n
    mp_y = withForeColor def $ color 199
    mp_n = withForeColor def $ color 162

    sColor = if hasFocus then sColor_y else sColor_n
    sColor_y = withForeColor def $ color 196
    sColor_n = withForeColor def $ color  88

    srColor = if hasFocus then srColor_y else srColor_n
    srColor_y = withForeColor def $ color 197
    srColor_n = withForeColor def $ color  89

    color i = Color240 $ -16 + i



padl n c s =
    if length s < n
        then padl n c (c:s)
        else s
