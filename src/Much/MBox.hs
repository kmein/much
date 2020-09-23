{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Much.MBox
    (
      -- TODO don't re-export MBox but use our own Message type
      module Export
    , getMessageId
    , toForest
    ) where

import qualified Data.MBox as Export

import           Control.Applicative
import qualified Data.CaseInsensitive as CI
import qualified Data.List as List
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.MBox
import           Data.Ord
import           Data.Set   (Set)
import qualified Data.Set as Set
import           Data.Text.Lazy   (Text)
import           Data.Time
import           Data.Tree   (Tree, Forest)
import qualified Data.Tree as Tree
import qualified MappedSets
import qualified Data.Text.Lazy as Text
import           Safe
import           System.Locale
import qualified Text.ParserCombinators.Parsec.Rfc2822 as P
import qualified Text.ParserCombinators.Parsec as P


type Ident = Text


data IdentFields = IdentFields
    { messageId :: Ident
    , inReplyTo :: [Ident]
    , references :: [Ident]
    }
  deriving Show


toForest :: MBox -> Forest Message
toForest mbox =
    map (sortTree . fmap (\i -> fromMaybe (error "meh") $ Map.lookup i msgs)) $
    concatMap (Tree.subForest . mkSubTree) (Set.toList $ roots refs)
  where

    mkSubTree rootLabel =
        Tree.Node rootLabel $
            map mkSubTree (maybe [] Set.toList $ Map.lookup rootLabel backRefs)

    refs = mboxRefs mbox
    backRefs = MappedSets.invert refs
    msgs = unpackMBox mbox


-- TODO finde a new home for roots
roots :: Ord a => Map a (Set a) -> Set a
roots refs =
    Set.unions $ Map.elems $ Map.filter p refs
  where
    messageIDs = Set.fromList $ Map.keys refs
    p = Set.null . Set.intersection messageIDs


-- TODO finde a new home for sortTree
sortTree :: Tree Message -> Tree Message
sortTree t =
    Tree.Node (Tree.rootLabel t) $
        map sortTree $
        List.sortOn (getMessageDate . Tree.rootLabel) $
        Tree.subForest t


getMessageDate :: Message -> Maybe UTCTime
getMessageDate msg =
    parseTime defaultTimeLocale rfc822DateFormat =<<
    Text.unpack . snd <$>
        (lastMay $
         filter ((==CI.mk "Date") . CI.mk . Text.unpack . fst) $
         headers msg)


unpackMBox :: MBox -> Map Ident Message
unpackMBox =
    Map.fromList .
    map (\msg -> (getMessageId $ headers msg, msg))


getIdentFields :: Message -> IdentFields
getIdentFields m =
    IdentFields
        { messageId = getMessageId hdrs
        , inReplyTo = getInReplyTo hdrs
        , references = getReferences hdrs
        }
  where
    hdrs = headers m


-- TODO generate default Message-ID if not present
getMessageId :: [Header] -> Ident
getMessageId =
    head .
    headerMessageIds "Message-ID"


getInReplyTo :: [Header] -> [Ident]
getInReplyTo =
    headerMessageIds "In-Reply-To"


getReferences :: [Header] -> [Ident]
getReferences =
    headerMessageIds "References"


headerMessageIds :: P.SourceName -> [Header] -> [Ident]
headerMessageIds headerName =
    concatMap (
        either ((:[]) . Text.pack . show) id .
        parseMsgIds headerName .
        snd
    ) .
    filter ((==CI.mk headerName) . CI.mk . Text.unpack . fst)


parseMsgIds :: P.SourceName -> Text -> Either P.ParseError [Ident]
parseMsgIds srcName =
    fmap (map (Text.init . Text.tail . Text.pack)) .
    P.parse obs_in_reply_to_parser srcName .
    Text.unpack
  where
    --obs_in_reply_to_parser :: CharParser a [String]
    obs_in_reply_to_parser =
        --filter (not . null) <$> P.many (P.phrase >> return [] <|> P.msg_id)
        P.many1 P.msg_id


messageRefs :: IdentFields -> [Ident]
messageRefs IdentFields{..} =
    if null inReplyTo
        then maybe [""] (:[]) (lastMay references)
        else inReplyTo


mboxRefs :: MBox -> Map Ident (Set Ident)
mboxRefs =
    MappedSets.mk .
    map (\m ->
          let x = getIdentFields m
          in (messageId x, messageRefs x))
