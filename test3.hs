{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}


import Data.Default
import Graphics.Vty

--import Data.List

--import Language.Haskell.TH.Ppr (bytesToString)
--import Data.Aeson
--import Data.List.Split
--import Data.Attoparsec.ByteString hiding (string)
--import Data.Maybe
--import Data.Monoid
--import Data.String
--import Data.Traversable
import Data.Tree
import qualified Data.Tree.Zipper as Z
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
--import qualified Data.ByteString.Char8 as BS8
--import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
--import qualified Data.Text.Encoding as T
--import qualified Data.Text.IO as T
--import Data.Version (Version(..), parseVersion)
--import System.Process
--import System.IO
--import qualified Data.Map as M

import Notmuch
import Notmuch.Message
import Notmuch.SearchResult
--import Safe

import Control.Exception

import ThreadView

import TreeSearch







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
--
--focusMessage t_cur t = do
--    i <- findIndex ((==t_cur) . messageId) msgs
--    msgs `atMay` i
--  where
--    msgs = flatten t


toggleTag :: T.Text -> Message -> IO ()
toggleTag tag m = do
    _ <- if tag `elem` messageTags m
        then
            unsetTag tagString (unMessageID $ messageId m)
        else
            setTag tagString (unMessageID $ messageId m)
    return ()
  where
    tagString = T.unpack tag


--toggleTag tag t_cur t =
--    case focusMessage t_cur t of
--        Nothing -> return ()
--        Just m -> do
--            if tag `elem` messageTags m
--                then
--                    unsetTag tagString (unMessageID $ messageId m)
--                else
--                    setTag tagString (unMessageID $ messageId m)
--            return ()
--  where
--    tagString = T.unpack tag

tagMessage :: T.Text -> Message -> IO LBS.ByteString
tagMessage tag m =
  setTag (T.unpack tag) (unMessageID $ messageId m)


untagMessage :: T.Text -> Message -> IO LBS.ByteString
untagMessage tag m =
  unsetTag (T.unpack tag) (unMessageID $ messageId m)



main :: IO ()
main =
    bracket (mkVty def) finit run
  where

    finit vty = do
        shutdown vty

    run vty = do
        t_ <- getThread tid
        let v = fromMessageTree t_
        let c = findMessage (MessageID cid) v
        rec vty 0 c v

    tid = "0000000000000862"
    cid = "87egtmvj0n.fsf@write-only.cryp.to"

    --rec vty t_cur t = do
    rec :: Vty -> Int -> Maybe ThreadView -> Tree ThreadView -> IO ()
    rec vty i c v = do
        let --img = threadImage t_cur (fromMessageTree t)
            img =
                (
                  string def (show i)
                  <|>
                  translateX 1
                    (
                      string def (maybe "Nothing" describe c)
                    )
                )
                <->
                threadImage c v
            pic = picForImage img
        update vty pic
        nextEvent vty >>= \e -> case e of
            EvKey KUp [] ->
                --case focusPrev t t_cur of
                --case focusPrev v c of
                --    Just t_prev ->
                --        --rec vty t_prev t
                --        rec vty (i + 1) t_prev v
                --    Nothing ->
                --        --rec vty t_cur t
                --        rec vty (i + 1) c v
                rec vty (i + 1) (focusPrev v c) v
            EvKey KDown [] ->
                --case focusNext t t_cur of
                --case focusNext v c of
                --    Just t_next ->
                --        --rec vty t_next t
                --        rec vty (i + 1) t_next v
                --    Nothing ->
                --        --rec vty t_cur t
                --        rec vty (i + 1) c v
                rec vty (i + 1) (focusNext v c) v
            EvKey KEnter [] ->
                case c of
                    Nothing -> error "no cursor"
                    Just c_ -> do
                        --toggleTag "open" t_cur t
                        toggleTag "open" c_
                        t'_ <- getThread tid
                        let v' = fromMessageTree t'_
                        let c' = findTV c_ v'
                        if c' == Nothing
                            then error $ "couldn't find" ++ show (c_, v')
                            else return ()
                        --rec vty t_cur t'
                        rec vty (i + 1) c' v'

            EvResize _w _h ->
                --rec vty t_cur t
                rec vty (i + 1) c v

            _ -> do
                print $ "Last event was: " ++ show e




threadImage :: Maybe ThreadView -> Tree ThreadView -> Image
--threadImage t_cur (Node n ns) =
threadImage c (Node n ns) =
    --messageImage hasFocus n
    --threadViewImage (hasFocus n) n
    threadViewImage (c == Just n) n
    <->
    translateX 2 (vertCat $ map (threadImage c) ns)
  --where
  --  --hasFocus = t_cur == messageId n
  --  hasFocus :: ThreadView -> Bool
  --  hasFocus (TVMessage m) = c == m
  --  hasFocus _ = False


--showTree' :: Tree Message -> [String]
--showTree' (Node n ns) =
--    -- (if length ns == 0
--    --     then indentMessageInit $ drawMessage n
--    --     else indentMessageLast $ drawMessage n)
--    drawMessage n
--    ++
--    concat (indentChildren (map showTree' ns))


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




--messageBodyImage = vertCat . map messagePartImage . messageBody
--
--messagePartImage = partContentImage . partContent
--
--partContentImage (ContentText t) =
--    vertCat $ map (text' def) $ T.lines t
--
--partContentImage (ContentMultipart _) = string def "ContentMultipart"
--partContentImage (ContentMsgRFC822 _) = string def "ContentMsgRFC822"







--    ----unMessageID messageId
--    --show messageTime
--    --  -- <> " " <> T.unpack messageDateRel
--    --  <> "\n" <> show (fromJust $ M.lookup "From" messageHeaders)
--    --  <> "\n" <> show (fromJust $ M.lookup "Subject" messageHeaders)
--    [ gray (unMessageID messageId)
--      <> " " <> T.unpack (fromJust $ M.lookup "From" messageHeaders)
--      <> " " <> gray (show messageDateRel)
--      <> " " <> T.unpack (T.intercalate ", " $ map magenta messageTags)
--    -- , T.unpack $ fromJust $ M.lookup "Subject" messageHeaders
--    ]
--    ++
--    (if "open" `elem` messageTags
--        then concat (map (map green . lines . drawMessagePart) messageBody)
--        else [])
--    --map drawMessagePart messageBody
