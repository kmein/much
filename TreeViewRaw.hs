{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TreeViewRaw where

import Data.Monoid
import TreeView
import Data.Tree
import Trammel
import qualified Notmuch
import qualified Notmuch.Message as Notmuch
import qualified Notmuch.SearchResult as Notmuch
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T


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

    TVSearchResult sr -> Plain $
        (padl 11 ' ' $ T.unpack $ Notmuch.searchDateRel sr)
        ++ "("
        ++ (show $ Notmuch.searchMatched sr)
        ++ ")  "
        ++ (T.unpack $ Notmuch.searchSubject sr)
        -- ++ " "
        -- ++ (let Notmuch.ThreadID tid = Notmuch.searchThread sr in tid)

    TVMessage m -> Plain $
        (Notmuch.unMessageID $ Notmuch.messageId m)
        ++ " "
        ++ T.unpack (T.intercalate (T.pack ",") $ Notmuch.messageTags m)

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
    -- | TVMessageLine Message MessagePart LineNr String


    --TVMessage m ->
    --    let col = if isOpen m then om else cm
    --    in
    --    string col (unMessageID $ messageId m)
    --    <|>
    --    translateX 1 (
    --      horizCat $
    --          intersperse (string col ", ") $
    --          map (text' tagColor) $
    --          messageTags m
    --    )
    s ->
      Plain $ describe s
--        let col = if isOpen m then om else cm
--        in
--        string col (unMessageID $ messageId m)
--        <|>
--        translateX 1 (
--          horizCat $
--              intersperse (string col ", ") $
--              map (text' tagColor) $
--              messageTags m
--        )


--hPutTreeView h hasFocus = \case
--    TVMessage m ->
--        putStr
--    _ ->



--
--treeViewImage :: Bool -> TreeView -> Image
--treeViewImage hasFocus = \case
--    TVMessage m ->
--        let col = if isOpen m then om else cm
--        in
--        string col (unMessageID $ messageId m)
--        <|>
--        translateX 1 (
--          horizCat $
--              intersperse (string col ", ") $
--              map (text' tagColor) $
--              messageTags m
--        )
--
--    TVMessageHeaderField m fieldName ->
--        let k = string mhf $ T.unpack $ CI.original fieldName
--            v = maybe (string mhf_empty "nothing")
--                      (string mhf . T.unpack)
--                      (M.lookup fieldName $ messageHeaders m)
--        in k <|> string mhf ": " <|> v
--
--    TVMessagePart _ p ->
--        string mp "TVMessagePart"
--          <|> translateX 1 (string mp $ show $ partID p)
--          <|> translateX 1 (string mp $ show $ partContentType p)
--
--    TVMessageLine _ _ _ s ->
--        string ml s
--
--    TVSearch s ->
--        string sColor s
--
--    TVSearchResult sr -> do
--        --let ThreadID tid = searchThread sr
--        --string srColor tid
--        -- <|>
--        --translateX 1
--        (string srColor $ padl 11 ' ' $ T.unpack $ searchDateRel sr)
--        <|>
--        string srColor " ("
--        <|>
--        (string srColor $ show $ searchMatched sr)
--        <|>
--        string srColor ")"
--        <|>
--        string srColor "  "
--        -- <|>
--        -- (string srColor $ show $ searchTime sr)
--        <|>
--        (string srColor $ T.unpack $ searchSubject sr)
--        <|>
--        --(string srColor $ T.unpack $ searchThread sr)
--        (translateX 1 $ let ThreadID tid = searchThread sr in string srColor tid)
--        --string srColor tid
--  where
--    --c1 = if hasFocus then c1_focus else c1_nofocus
--    --c1_nofocus = withForeColor def $ Color240 $ -16 + 238
--    --c1_focus = withForeColor def $ Color240 $ -16 + 244
--    --c2 = withForeColor def $ Color240 $ -16 + 106
--    --c3 = withForeColor def $ Color240 $ -16 + 199
--
--    tagColor = if hasFocus then tagColor_y else tagColor_n
--    tagColor_y = withForeColor def $ color 230
--    tagColor_n = withForeColor def $ color 200
--
--    cm = if hasFocus then cm_y else cm_n
--    cm_y = withForeColor def $ color 46
--    cm_n = withForeColor def $ color 22
--
--    om = if hasFocus then om_y else om_n
--    om_y = withForeColor def $ color 82
--    om_n = withForeColor def $ color 58
--
--    ml = if hasFocus then ml_y else ml_n
--    ml_y = withForeColor def $ color 226
--    ml_n = withForeColor def $ color 202
--
--    mhf = if hasFocus then mhf_y else mhf_n
--    mhf_y = withForeColor def $ color 248
--    mhf_n = withForeColor def $ color 244
--
--    mhf_empty = if hasFocus then mhf_empty_y else mhf_empty_n
--    mhf_empty_y = withForeColor def $ color 88
--    mhf_empty_n = withForeColor def $ color 52
--
--    --ph = if hasFocus then ph_y else ph_n
--    --ph_y = withForeColor def $ color 241
--    --ph_n = withForeColor def $ color 235
--
--    mp = if hasFocus then mp_y else mp_n
--    mp_y = withForeColor def $ color 199
--    mp_n = withForeColor def $ color 162
--
--    sColor = if hasFocus then sColor_y else sColor_n
--    sColor_y = withForeColor def $ color 196
--    sColor_n = withForeColor def $ color  88
--
--    srColor = if hasFocus then srColor_y else srColor_n
--    srColor_y = withForeColor def $ color 197
--    srColor_n = withForeColor def $ color  89
--
--    color i = Color240 $ -16 + i
