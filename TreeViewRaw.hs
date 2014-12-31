{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TreeViewRaw (renderTreeView) where

import qualified Notmuch.Message as Notmuch
import qualified Notmuch.SearchResult as Notmuch
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Char
import Data.Monoid
import Data.Time
import Data.Time.Format.Human
import Data.Tree
import TagUtils (Tag)
import Trammel
import TreeView


-- TODO make configurable
humanTimeLocale :: HumanTimeLocale
humanTimeLocale = defaultHumanTimeLocale
    { justNow       = "now"
    , secondsAgo    = (++ "s ago")
    , oneMinuteAgo  = "1m ago"
    , minutesAgo    = (++ "m ago")
    , oneHourAgo    = "1h ago"
    , aboutHoursAgo = (++ "h ago")
    , at            = \_ -> ("" ++)
    , daysAgo       = (++ "d ago")
    , weekAgo       = (++ "w ago")
    , weeksAgo      = (++ "w ago")
    , onYear        = ("" ++)
    , dayOfWeekFmt  = "%a %H:%M"
    , thisYearFmt   = "%b %e"
    , prevYearFmt   = "%b %e, %Y"
    }


renderTreeView :: UTCTime -> TreeView -> Tree TreeView -> [Trammel String]
renderTreeView now cur _loc@(Node label children) =
    [ renderTreeView1 now hasFocus label ] ++
    concatMap (map ("  "<>) . renderTreeView now cur) children
  where
    hasFocus = cur == label


-- TODO locale-style: headerKey = \s -> SGR [..] (s <> ": ")

searchSGR
    , focusSGR
    , boringSGR
    , dateSGR
    , tagsSGR
    , unreadMessageSGR
    , unreadSearchSGR
    , killedTagSGR
    :: Trammel String -> Trammel String
searchSGR = SGR [38,5,162]
focusSGR = SGR [38,5,160]
boringSGR = SGR [38,5,240]
dateSGR = SGR [38,5,071]
tagsSGR = SGR [38,5,036]
killedTagSGR = SGR [38,5,088]

unreadMessageSGR = SGR [38,5,117]
unreadSearchSGR = SGR [38,5,250]


renderTreeView1 :: UTCTime -> Bool -> TreeView -> Trammel String
renderTreeView1 now hasFocus x = case x of

    TVSearch s ->
        let c = if hasFocus then focusSGR else searchSGR
        in c $ Plain s

    TVSearchResult sr ->
        let c = if hasFocus then focusSGR else
                    if "unread" `elem` Notmuch.searchTags sr
                        then unreadSearchSGR
                        else boringSGR
            date = dateSGR $ renderDate now x
            tags = tagsSGR $ renderTags (Notmuch.searchTags sr)
            subj = Plain $ T.unpack $ Notmuch.searchSubject sr
        in c $ subj <> " " <> date <> " " <> tags

    TVMessage m ->
        let c = if hasFocus then focusSGR else
                    if "unread" `elem` Notmuch.messageTags m
                        then unreadMessageSGR
                        else boringSGR
            from = renderFrom (M.lookup "from" $ Notmuch.messageHeaders m)
            date = dateSGR $ renderDate now x
            tags = tagsSGR $ renderTags (Notmuch.messageTags m) -- TODO filter common tags
        in c $ from <> " " <> date <> " " <> tags

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
        in c $ "part#" <> i <> " " <> t

    TVMessageLine _ _ _ s ->
        if hasFocus
            then focusSGR $ Plain s
            else Plain s



renderDate :: UTCTime -> TreeView -> Trammel String
renderDate now = \case
    TVSearchResult sr -> f humanTimeLocale (Notmuch.searchTime sr)
    TVMessage m -> f humanTimeLocale (Notmuch.messageTime m)
    _ -> SGR [35,1] "timeless"
  where
    f timeLocale time =
        Plain $ humanReadableTimeI18N' timeLocale now time


renderFrom :: Maybe T.Text -> Trammel String
renderFrom = \case
    Just fromLine -> Plain $ dropAddress $ T.unpack fromLine
    Nothing -> SGR [35,1] "Anonymous"


renderTags :: [Tag] -> Trammel String
renderTags =
    -- TODO sort somewhere else
    mconcat . L.intersperse " " . map renderTag . L.sort


renderTag :: Tag -> Trammel String
renderTag tag = case tag of
    "killed" -> killedTagSGR plain
    _ -> plain
  where
    plain = Plain $ T.unpack tag


dropAddress :: String -> String
dropAddress xs =
    case L.findIndices (=='<') xs of
        [] -> xs
        is -> L.dropWhileEnd isSpace $ take (last is) xs
