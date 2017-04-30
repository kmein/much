{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module RenderTreeView (renderTreeView) where

import qualified Notmuch.Message as Notmuch
import qualified Notmuch.SearchResult as Notmuch
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Tree.Zipper as Z
import qualified TreeZipperUtils as Z
import Blessings
import Data.Char
import Data.Monoid
import Data.Time
import Data.Time.Format.Human
import Data.Tree
import TagUtils (Tag)
import TreeView


-- TODO make configurable
humanTimeLocale :: HumanTimeLocale
humanTimeLocale = defaultHumanTimeLocale
    { justNow       = "now"
    , secondsAgo    = \f -> (++ "s" ++ dir f)
    , oneMinuteAgo  = \f -> "1m" ++ dir f
    , minutesAgo    = \f -> (++ "m" ++ dir f)
    , oneHourAgo    = \f -> "1h" ++ dir f
    , aboutHoursAgo = \f -> (++ "h" ++ dir f)
    , at            = \_ -> ("" ++)
    , daysAgo       = \f -> (++ "d" ++ dir f)
    , weekAgo       = \f -> (++ "w" ++ dir f)
    , weeksAgo      = \f -> (++ "w" ++ dir f)
    , onYear        = ("" ++)
    , dayOfWeekFmt  = "%a %H:%M"
    , thisYearFmt   = "%b %e"
    , prevYearFmt   = "%b %e, %Y"
    }
  where dir True  = " from now"
        dir False = " ago"


renderTreeView
    :: UTCTime
    -> Z.TreePos Z.Full TreeView
    -> Z.TreePos Z.Full TreeView
    -> [Blessings String]
renderTreeView now cur =
    renderNode
  where
    isFocus = (Z.label cur==) . Z.label

    renderNode loc =
        renderRootLabel loc :
        maybeRenderSubForest (Z.firstChild loc)

    renderRootLabel loc =
        renderPrefix loc <>
        renderTreeView1 now (isFocus loc) (Z.label loc)

    renderSubForest loc =
        renderNode loc ++
        maybeRenderSubForest (Z.next loc)

    maybeRenderSubForest =
        maybe mempty renderSubForest


renderPrefix :: Z.TreePos Z.Full TreeView -> Blessings String
renderPrefix =
    mconcat . reverse . map prefix . zip [(1 :: Int)..] . Z.path
  where
    prefix (i, (_lhs, x, rhs)) = case x of
        TVSearch _ -> ""
        TVSearchResult _ -> spacePrefix
        TVMessage _ ->
            case i of
                1 ->
                    if null rhs
                        then endPrefix
                        else teePrefix
                _ ->
                    if null rhs
                        then spacePrefix
                        else pipePrefix
        _ ->
            if null $ filter isTVMessage $ map rootLabel rhs
                then spacePrefix
                else pipePrefix


spacePrefix
    , teePrefix
    , pipePrefix
    , endPrefix
    :: Blessings String
spacePrefix = prefixSGR "  "
teePrefix   = prefixSGR "├╴"
pipePrefix  = prefixSGR "│ "
endPrefix   = prefixSGR "└╴"


-- TODO locale-style: headerKey = \s -> SGR [..] (s <> ": ")

searchSGR
    , altSGR
    , focusSGR
    , quoteSGR
    , boringSGR
    , prefixSGR
    , dateSGR
    , tagsSGR
    , boringMessageSGR
    , unreadMessageSGR
    , unreadSearchSGR
    , killedTagSGR
    , starTagSGR
    , draftTagSGR
    :: Blessings String -> Blessings String

altSGR = SGR [38,5,182]
searchSGR = SGR [38,5,162]
focusSGR = SGR [38,5,160]
quoteSGR = SGR [38,5,242]
boringSGR = SGR [38,5,240]
prefixSGR = SGR [38,5,235]
dateSGR = SGR [38,5,071]
tagsSGR = SGR [38,5,036]
killedTagSGR = SGR [38,5,088]
starTagSGR = SGR [38,5,226]
draftTagSGR = SGR [38,5,202]

boringMessageSGR = SGR [38,5,023]
unreadMessageSGR = SGR [38,5,117]
unreadSearchSGR = SGR [38,5,250]


renderTreeView1 :: UTCTime -> Bool -> TreeView -> Blessings String
renderTreeView1 now hasFocus x = case x of

    TVSearch s ->
        let c = if hasFocus then focusSGR else searchSGR
        in c $ Plain s

    TVSearchResult sr ->
        let c = if hasFocus then focusSGR else
                    if isUnread
                        then unreadSearchSGR
                        else boringSGR
            c_authors =
                if hasFocus then focusSGR else
                    if isUnread
                        then altSGR
                        else boringSGR

            isUnread = "unread" `elem` Notmuch.searchTags sr

            authors = Plain $ T.unpack $ Notmuch.searchAuthors sr
            date = dateSGR $ renderDate now x
            subject = Plain $ T.unpack $ Notmuch.searchSubject sr
            tags = tagsSGR $ renderTags (Notmuch.searchTags sr)
            title = if subject /= "" then subject else c_authors authors
        in
          c $ title <> " " <> date <> " " <> tags

    TVMessage m ->
        let fromSGR =
                if hasFocus then focusSGR else
                    if "unread" `elem` Notmuch.messageTags m
                        then unreadMessageSGR
                        else boringMessageSGR
            from = fromSGR $ renderFrom (M.lookup "from" $ Notmuch.messageHeaders m)
            date = dateSGR $ renderDate now x
            tags = tagsSGR $ renderTags (Notmuch.messageTags m) -- TODO filter common tags
        in from <> " " <> date <> " " <> tags

    TVMessageHeaderField m fieldName ->
        let c = if hasFocus then focusSGR else boringSGR
            k = Plain $ T.unpack $ CI.original fieldName
            v = maybe "nothing"
                      (Plain . T.unpack)
                      (M.lookup fieldName $ Notmuch.messageHeaders m)
        in c $ k <> ": " <> v

    TVMessagePart _ p ->
        let c = if hasFocus then focusSGR else boringSGR
            i = Plain $ show $ Notmuch.partID p
            t = Plain $ T.unpack $ CI.original $ Notmuch.partContentType p
            filename = maybe "" (Plain . (" "<>) . show) $ Notmuch.partContentFilename p
            charset = maybe "" (Plain . (" "<>) . show) $ Notmuch.partContentCharset p
            size = Plain $ show $ Notmuch.contentSize (Notmuch.partContent p)
        in c $ "part#" <> i <> " " <> t <> filename <> charset <> " " <> size

    TVMessageQuoteLine _ _ _ s ->
        if hasFocus
            then focusSGR $ Plain s
            else quoteSGR $ Plain s

    TVMessageLine _ _ _ s ->
        if hasFocus
            then focusSGR $ Plain s
            else Plain s



renderDate :: UTCTime -> TreeView -> Blessings String
renderDate now = \case
    TVSearchResult sr -> f humanTimeLocale (Notmuch.searchTime sr)
    TVMessage m -> f humanTimeLocale (Notmuch.messageTime m)
    _ -> SGR [35,1] "timeless"
  where
    f timeLocale time =
        Plain $ humanReadableTimeI18N' timeLocale now time


renderFrom :: Maybe T.Text -> Blessings String
renderFrom = \case
    Just fromLine -> Plain $ dropAddress $ T.unpack fromLine
    Nothing -> SGR [35,1] "Anonymous"


renderTags :: [Tag] -> Blessings String
renderTags =
    -- TODO sort somewhere else
    mconcat . L.intersperse " " . map renderTag . L.sort


renderTag :: Tag -> Blessings String
renderTag tag = case tag of
    "draft" -> draftTagSGR plain
    "killed" -> killedTagSGR plain
    "star" -> starTagSGR plain
    _ -> plain
  where
    plain = Plain $ T.unpack tag


dropAddress :: String -> String
dropAddress xs =
    case L.findIndices (=='<') xs of
        [] -> xs
        is -> L.dropWhileEnd isSpace $ take (last is) xs
