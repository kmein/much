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

--import Notmuch.SearchResult
import Notmuch.Message
--import Notmuch
import Safe


type LineNr = Int


data ThreadView
    = TVMessage Message
    | TVMessageLine Message MessagePart LineNr String
    | TVMessagePart Message MessagePart
  deriving (Show)

instance Eq ThreadView where
    TVMessageLine m1 mp1 ln1 _s1 == TVMessageLine m2 mp2 ln2 _s2 =
        messageId m1 == messageId m2 && mp1 == mp2 && ln1 == ln2

    TVMessagePart m1 mp1 == TVMessagePart m2 mp2 =
        messageId m1 == messageId m2 && mp1 == mp2

    TVMessage m1 == TVMessage m2 =
        messageId m1 == messageId m2

    _ == _ = False


describe :: ThreadView -> String
describe (TVMessage m) = "TVMessage" <> unMessageID (messageId m)
describe (TVMessageLine _ _ _ s) = "TVMessageLine " <> show s
describe (TVMessagePart m p) = "TVMessagePart " <> (unMessageID $ messageId m) <> " " <> show (partID p)


--focusPrev t_cur t = do
--    i <- findIndex ((==t_cur) . messageId) msgs
--    m' <- msgs `atMay` (i - 1)
--    return $ messageId m'
--  where
--    msgs = flatten t
--
--focusNext t_cur t = do
--    i <- findIndex ((==t_cur) . messageId) msgs
--    m' <- msgs `atMay` (i + 1)
--    return $ messageId m'
--  where
--    msgs = flatten t

focusPrev :: Tree ThreadView -> Maybe ThreadView -> Maybe ThreadView
focusPrev v Nothing = lastMay (flatten v)
focusPrev v (Just cur) = do
    i <- elemIndex cur items
    maybe (lastMay items) Just $ atMay items (i - 1)
  where
    items = flatten v

focusNext :: Tree ThreadView -> Maybe ThreadView -> Maybe ThreadView
focusNext v Nothing = headMay (flatten v)
focusNext v (Just cur) = do
    i <- elemIndex cur items
    maybe (headMay items) Just $ atMay items (i + 1)
  where
    items = flatten v


findMessage :: MessageID -> Tree ThreadView -> Maybe ThreadView
findMessage i =
    find p . flatten
  where
    p (TVMessage m) = i == messageId m
    p _ = False

findTV :: ThreadView -> Tree ThreadView -> Maybe ThreadView
findTV x =
    find (==x) . flatten


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
    Node (TVMessagePart m p) []


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

    TVMessageLine _ _ _ s ->
        string ml s

    TVMessagePart _ p ->
        string def "TVMessagePart"
          <|> translateX 1 (string def $ show $ partContentType p)
          <-> translateX 2 (partImage p)

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

    color i = Color240 $ -16 + i



partImage :: MessagePart -> Image
partImage p = case partContentType p of
    "text/plain" ->
        partContentImage $ partContent p
        --string def (show $ partContent p)
    "multipart/alternative" ->
        partContentImage $ partContent p
    "multipart/signed" ->
        partContentImage $ partContent p
    _ ->
        mempty



partTextLineImage :: String -> Image
partTextLineImage s =
    string def s


--messageImage hasFocus m@Message{..} =
--    string c1 (unMessageID messageId)
--    <|>
--    translateX 1 (
--      text' c2 (fromJust $ M.lookup "From" messageHeaders)
--    )
--    <|>
--    translateX 1 (
--      horizCat $ intersperse (string c1 ", ") $ map (text' c3) messageTags
--    )
--    <->
--    translateX 4
--        (if "open" `elem` messageTags
--            then messageBodyImage m
--            else mempty)
--
--  where
--    c1 = if hasFocus then c1_focus else c1_nofocus
--    c1_nofocus = withForeColor def $ Color240 $ -16 + 238
--    c1_focus = withForeColor def $ Color240 $ -16 + 244
--    c2 = withForeColor def $ Color240 $ -16 + 106
--    c3 = withForeColor def $ Color240 $ -16 + 199
--
--
--
--
--messageBodyImage = vertCat . map messagePartImage . messageBody
--
--messagePartImage = partContentImage . partContent
--

partContentImage (ContentText t) =
    vertCat $ map (text' def) $ T.lines t

partContentImage (ContentMultipart parts) =
    --string def "ContentMultipart"
    vertCat $ map partImage parts


partContentImage (ContentMsgRFC822 _) = string def "ContentMsgRFC822"
