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
import Data.Maybe
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

import TreeView

import TreeSearch







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
        --XXX show a single thread
        --t_ <- getThread "0000000000000862"
        --let v = fromMessageTree t_
        --let c = findMessage (MessageID "87egtmvj0n.fsf@write-only.cryp.to") v
        --rec vty 0 c v

        let query = "tag:inbox AND NOT tag:killed"
        Right r_ <- search query
        let v = fromSearchResults query r_
        rec vty 0 (Z.fromTree v)

    --rec vty t_cur t = do
    --rec :: Vty -> Int -> Z.TreePos Z.Full TreeView -> Tree TreeView -> IO ()
    rec :: Vty -> Int -> Z.TreePos Z.Full TreeView -> IO ()
    rec vty i c = do
        let
            img =
                --string def (show i) <->
                --string def (maybe "Nothing" describe (focusPrev v c)) <->
                --string def (maybe "Nothing" describe c) <->
                --string def (maybe "Nothing" describe (focusNext v c)) <->
                --string def (maybe "Nothing" describe (focusPrev v c)) <->
                --string def (describe $ Z.label c) <->
                --string def (maybe "Nothing" describe (focusNext v c)) <->
                treeImage (Just $ Z.label c) (Z.toTree c)
            pic = picForImage img
            v = Z.root c
        update vty pic
        nextEvent vty >>= \e -> case e of
            EvKey KUp [] ->
                rec vty (i + 1) (fromMaybe (Z.root c) $ findPrev c)
            EvKey KDown [] ->
                rec vty (i + 1) (fromMaybe (Z.root c) $ findNext c)
            EvKey KEnter [] ->
                onEnter c

            EvResize _w _h ->
                rec vty (i + 1) c

            _ -> do
                print $ "Last event was: " ++ show e
      where
        onEnter c_ = case Z.label c_ of
            TVMessage m -> do
                toggleTag "open" m

                let loc = c
                    Just sr = findParent isTVSearchResult loc
                    Just sr0 = Z.firstChild sr -- TODO can there be only one (thread per sr)?
                    TVSearchResult the_sr = Z.label sr
                    ThreadID tid = searchThread the_sr

                t_ <- return . (:[]) . fromMessageTree =<< getThread tid

                rec vty 0 $ fromMaybe (error "couldn't reselect")
                          $ findTree (==Z.label c)
                          $ Z.modifyTree (\(Node l _) -> Node l t_) sr

            TVSearchResult sr -> do
                --let Just loc = findTree (==c_) $ Z.fromTree v
                let loc = c

                let open = not $ null $ subForest $ Z.tree loc
                let ThreadID tid = searchThread sr

                t_ <-
                    if open
                        then return []
                        else return . (:[]) . fromMessageTree =<< getThread tid

                rec vty 0 $ Z.modifyTree (\(Node l _) -> Node l t_) loc


treeImage :: Maybe TreeView -> Tree TreeView -> Image
--treeImage t_cur (Node n ns) =
treeImage c (Node n ns) =
    --messageImage hasFocus n
    --treeViewImage (hasFocus n) n
    treeViewImage (c == Just n) n
    <->
    translateX 2 (vertCat $ map (treeImage c) ns)
  --where
  --  --hasFocus = t_cur == messageId n
  --  hasFocus :: TreeView -> Bool
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
