{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Much.RenderTreeView (renderTreeView) where

import qualified Notmuch.Message as Notmuch
import qualified Notmuch.SearchResult as Notmuch
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Tree.Zipper as Z
import qualified Much.TreeZipperUtils as Z
import Blessings
import Control.Arrow
import Data.Char
import Data.Function
import Data.Functor.Identity
import Data.Maybe
import Data.Time
import Data.Time.Format.Human
import Data.Tree
import Much.State
import Much.TagUtils (Tag)
import Much.TreeView

color :: (t -> Identity Pm) -> t -> Blessings a -> Blessings a
color key config = SGR $ runIdentity $ key config

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
    :: State
    -> Z.TreePos Z.Full TreeView
    -> [Blessings String]
renderTreeView q@State{..} =
    renderNode
  where
    isFocus = (Z.label cursor==) . Z.label

    renderNode loc =
        renderRootLabel loc :
        maybeRenderSubForest (Z.firstChild loc)

    renderRootLabel loc =
        renderPrefix q loc <>
        renderTreeView1 q (isFocus loc) (Z.label loc)

    renderSubForest loc =
        renderNode loc ++
        maybeRenderSubForest (Z.next loc)

    maybeRenderSubForest =
        maybe mempty renderSubForest


renderPrefix :: State -> Z.TreePos Z.Full TreeView -> Blessings String
renderPrefix state =
    mconcat . reverse . zipWith (curry prefix) [(1 :: Int)..] . Z.path
  where
    prefix (i, (_lhs, x, rhs)) = case x of
        TVSearch _ -> ""
        TVSearchResult _ -> spacePrefix state
        TVMessage _ ->
            case i of
                1 ->
                    if null rhs
                        then endPrefix state
                        else teePrefix state
                _ ->
                    if null rhs
                        then spacePrefix state
                        else pipePrefix state
        _ ->
            if not $ any (isTVMessage . rootLabel) rhs
                then spacePrefix state
                else pipePrefix state


spacePrefix
    , teePrefix
    , pipePrefix
    , endPrefix
    :: State -> Blessings String
spacePrefix q = color prefix (colorConfig q) "  "
teePrefix q = color prefix (colorConfig q) "├╴"
pipePrefix q = color prefix (colorConfig q) "│ "
endPrefix q = color prefix (colorConfig q) "└╴"


-- TODO locale-style: headerKey = \s -> SGR [..] (s <> ": ")


renderTreeView1 :: State -> Bool -> TreeView -> Blessings String
renderTreeView1 q@State{..} hasFocus x = case x of

    TVSearch s ->
        let c = if hasFocus then color focus colorConfig else color search colorConfig
        in c $ Plain s

    TVSearchResult sr ->
        let c
                | hasFocus = color focus colorConfig
                | isUnread = color unreadSearch colorConfig
                | otherwise = color boring colorConfig
            c_authors
                | hasFocus = color focus colorConfig
                | isUnread = color alt colorConfig
                | otherwise = color boring colorConfig

            isUnread = "unread" `elem` Notmuch.searchTags sr

            authors = Plain $ T.unpack $ Notmuch.searchAuthors sr
            date = color Much.State.date colorConfig $ renderDate now x
            subject = Plain $ T.unpack $ Notmuch.searchSubject sr
            tags = color Much.State.tags colorConfig $ renderTags q (Notmuch.searchTags sr)
            title = if subject /= "" then subject else c_authors authors
        in
          c $ title <> " " <> date <> " " <> tags

    TVMessage m ->
        let fromSGR
                | hasFocus = color focus colorConfig
                | "unread" `elem` Notmuch.messageTags m = color unreadMessage colorConfig
                | otherwise = color boringMessage colorConfig
            from = fromSGR $ renderFrom (M.lookup "from" $ Notmuch.messageHeaders m)
            date = color Much.State.date colorConfig $ renderDate now x
            tags = color Much.State.tags colorConfig $ renderTags q (Notmuch.messageTags m) -- TODO filter common tags
        in from <> " " <> date <> " " <> tags

    TVMessageHeaderField m fieldName ->
        let c = if hasFocus then color focus colorConfig else color boring colorConfig
            k = Plain $ T.unpack $ CI.original fieldName
            v = maybe "nothing"
                      (Plain . T.unpack)
                      (M.lookup fieldName $ Notmuch.messageHeaders m)
        in c $ k <> ": " <> v

    TVMessagePart _ p ->
        let c = if hasFocus then color focus colorConfig else color boring colorConfig
            i = Plain $ show $ Notmuch.partID p
            t = Plain $ T.unpack $ CI.original $ Notmuch.partContentType p
            filename = maybe "" (Plain . (" "<>) . show) $ Notmuch.partContentFilename p
            charset = maybe "" (Plain . (" "<>) . show) $ Notmuch.partContentCharset p
            size = Plain $ show $ Notmuch.contentSize (Notmuch.partContent p)
        in c $ "part#" <> i <> " " <> t <> filename <> charset <> " " <> size

    TVMessageQuoteLine _ _ _ s ->
        if hasFocus
            then color focus colorConfig $ Plain s
            else color quote colorConfig $ Plain s

    TVMessageRawLine _ _ _ s ->
        mconcat . map (uncurry renderClassifiedString) $ classifiedGroupBy isPrint s
      where
        renderClassifiedString :: Bool -> String -> Blessings String
        renderClassifiedString = \case
            True -> printableColor . Plain
            False -> unprintableColor . Plain . showLitChar'

        (printableColor, unprintableColor) =
            if hasFocus
              then (color focus colorConfig, color unprintableFocus colorConfig)
              else (color quote colorConfig, color unprintableNormal colorConfig)

        showLitChar' :: String -> String
        showLitChar' = (>>= f)
          where f '\ESC' = "^["
                f c = showLitChar c ""

        classifiedGroupBy :: Eq b => (a -> b) -> [a] -> [(b, [a])]
        classifiedGroupBy f =
            map (f . head &&& id) . L.groupBy ((==) `on` f)

    TVMessageLine _ _ _ s ->
        if hasFocus
            then color focus colorConfig $ Plain s
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


renderTags :: State -> [Tag] -> Blessings String
renderTags state =
    -- TODO sort somewhere else
    mconcat . L.intersperse " " . map (renderTag state) . L.sort


renderTag :: State -> Tag -> Blessings String
renderTag state tag = case M.lookup tag $ runIdentity $ tagMap $ colorConfig state of
    Just visual -> SGR (runIdentity visual) plain
    Nothing -> plain
  where
    plain = Plain $ T.unpack $ fromMaybe tag $ M.lookup tag (tagSymbols state)


dropAddress :: String -> String
dropAddress xs =
    case L.elemIndices '<' xs of
        [] -> xs
        is -> L.dropWhileEnd isSpace $ take (last is) xs
