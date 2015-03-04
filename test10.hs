{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

--module Main (main) where

import           Control.Applicative
--import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.CaseInsensitive as CI
import qualified Data.List as List
import           Data.Maybe
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict as Map
import           Data.MBox
import           Data.Ord
import           Data.Set   (Set)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as LT
import           Data.Time
import           Data.Tree   (Tree, Forest)
import qualified Data.Tree as Tree
import qualified MappedSets
import           Safe
import           System.Environment
import           System.Locale
import           System.Process
import qualified Text.ParserCombinators.Parsec.Rfc2822 as P
import qualified Text.ParserCombinators.Parsec as P


type Ident = LT.Text

data IdentFields = IdentFields
    { messageId :: Ident
    , inReplyTo :: [Ident]
    , references :: [Ident]
    }
  deriving Show


notmuchShowMBox :: String -> IO MBox
notmuchShowMBox searchTerm =
    parseMBox . LT.pack <$> readProcess
        "notmuch"
        [ "show"
        , "--format=mbox"
        , "--entire-thread=true"
        , searchTerm
        ]
        ""


getMessageDate :: Message -> Maybe UTCTime
getMessageDate msg =
    parseTime defaultTimeLocale rfc822DateFormat =<<
    LT.unpack . snd <$>
        (lastMay $
         filter ((==CI.mk "Date") . CI.mk . LT.unpack . fst) $
         headers msg)


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
        either ((:[]) . LT.pack . show) id .
        parseMsgIds headerName .
        snd
    ) .
    filter ((==CI.mk headerName) . CI.mk . LT.unpack . fst)


parseMsgIds :: P.SourceName -> LT.Text -> Either P.ParseError [LT.Text]
parseMsgIds srcName =
    either Left (Right . map (LT.init . LT.tail . LT.pack)) .
    P.parse obs_in_reply_to_parser srcName .
    LT.unpack
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


roots :: Ord a => Map a (Set a) -> Set a
roots refs =
    Set.unions $ Map.elems $ Map.filter p refs
  where
    messageIDs = Set.fromList $ Map.keys refs
    p = Set.null . Set.intersection messageIDs


sortTree :: Tree Message -> Tree Message
sortTree t =
    Tree.Node (Tree.rootLabel t) $
        map sortTree $
        List.sortBy (comparing $ getMessageDate . Tree.rootLabel) $
        Tree.subForest t


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



unpackMBox :: MBox -> Map Ident Message
unpackMBox =
    Map.fromList .
    map (\msg -> (getMessageId $ headers msg, msg))


renderMessage :: Message -> String
renderMessage msg =
    LT.unpack (getMessageId $ headers msg)
    ++ " "
    ++ drop (length ("From " :: String)) (LT.unpack $ fromLine msg)


main :: IO ()
main = do
    -- load-env hack
    maybe (return ()) (setEnv "HOME") =<< lookupEnv "OLDHOME"

    notmuchShowMBox "tree1" >>=
        putStrLn . Tree.drawTree .
        Tree.Node "subject:tree-test" .
        map (fmap renderMessage) .
        toForest
