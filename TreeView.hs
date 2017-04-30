{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


module TreeView
    ( module Export
    , getMessage
    , getSearchTerm
    , isTVMessage
    , isTVSearchResult
    , fromSearchResults
    , fromMessageForest
    , fromMessageTree
    , loadSubForest
    , unloadSubForest
    , hasUnloadedSubForest
    ) where


import qualified Data.Text as T
import Control.Applicative
import Data.Monoid
import Data.Tree
import Notmuch
import Notmuch.Message
import Notmuch.SearchResult
import TreeView.Types as Export


getMessage :: TreeView -> Maybe Message
getMessage = \case
    TVMessage m -> Just m
    TVMessageHeaderField m _ -> Just m
    TVMessagePart m _ -> Just m
    TVMessageQuoteLine m _ _ _ -> Just m
    TVMessageLine m _ _ _ -> Just m
    _ -> Nothing


getSearchTerm :: TreeView -> Maybe String
getSearchTerm = \case
    TVSearch term -> Just term
    _ -> Nothing


isTVMessage :: TreeView -> Bool
isTVMessage = \case
    TVMessage _ -> True
    _ -> False


isTVMessagePart :: TreeView -> Bool
isTVMessagePart = \case
    TVMessagePart _ _ -> True
    _ -> False


isTVSearchResult :: TreeView -> Bool
isTVSearchResult (TVSearchResult _) = True
isTVSearchResult _ = False


fromSearchResults :: String -> [SearchResult] -> Tree TreeView
fromSearchResults query =
    Node (TVSearch query) . map (\r -> Node (TVSearchResult r) [])


fromMessageForest :: Forest Message -> Forest TreeView
fromMessageForest = map fromMessageTree


fromMessageTree :: Tree Message -> Tree TreeView
fromMessageTree (Node m ms) =
    Node (TVMessage m)
         (xconvHead m <> xconvBody m <> map fromMessageTree ms)


xconvHead :: Message -> Forest TreeView
xconvHead m =
    map conv [ "From", "To" ]
    -- TODO add Subject if it differs from thread subject
  where
    conv k =
      Node (TVMessageHeaderField m k) []


xconvBody :: Message -> Forest TreeView
xconvBody m = map (xconvPart m) (messageBody m)


xconvPart :: Message -> MessagePart -> Tree TreeView
xconvPart m p =
    Node (TVMessagePart m p) contents
  where
    contents = case partContent p of
        ContentText t ->
            map (xconvLine m p) $ zip [0..] (T.lines t)
        ContentMultipart parts ->
            map (xconvPart m) parts
        ContentMsgRFC822 _ ->
            []


xconvLine
  :: Message -> MessagePart -> (LineNr, T.Text) -> Tree TreeView
xconvLine m p (i, s) =
    Node (ctor m p i $ T.unpack s) []
  where
    ctor =
        if isQuoteLine s
            then TVMessageQuoteLine
            else TVMessageLine


isQuoteLine :: T.Text -> Bool
isQuoteLine s0 = do
    let s = T.stripStart s0

    -- /^\s*>/
    not (T.null s) && T.head s == '>'


--
-- Loading / Unloading
--


loadSubForest :: TreeView -> IO (Either String (Forest TreeView))
loadSubForest = \case
    TVMessage m ->
        Right
            . unloadPartsWithFilename
            . map unloadReadSubForests
            . concatMap subForest
            . fromMessageForest
            . findFirsts messageMatch
            <$> notmuchShow (termFromMessage m)

    TVMessagePart m mp ->
        -- TODO parse --format=raw
        notmuchShowPart (termFromMessage m) (partID mp) >>= return . \case
            Left e -> Left $ show e
            Right mp' ->
                Right
                    . unloadPartsWithFilename
                    . subForest
                    $ xconvPart m mp'

    TVSearchResult sr -> do
        Right
            . unloadPartsWithFilename
            . map unloadReadSubForests
            . fromMessageForest
            <$> notmuchShow (termFromSearchResult sr)

    TVSearch s -> do
        Right
            . subForest
            . fromSearchResults s
            . either error id
            <$> Notmuch.search s

    _ ->
        return $ Right []

  where
    termFromMessage = unMessageID . messageId
    termFromSearchResult = unThreadID . searchThread


unloadSubForest :: Tree TreeView -> Forest TreeView
unloadSubForest t = case rootLabel t of
    TVMessage _ ->
        filter (isTVMessage . rootLabel) $ subForest t
    TVMessagePart _ _ ->
        filter (isTVMessagePart . rootLabel) $ subForest t
    _ ->
        []


hasUnloadedSubForest :: Tree TreeView -> Bool
hasUnloadedSubForest t = case rootLabel t of
    TVMessage _ ->
        null $ filter (not . isTVMessage . rootLabel) $ subForest t
    TVMessagePart _ _ ->
        null $ filter (not . isTVMessagePart . rootLabel) $ subForest t
    _ ->
        null $ subForest t


unloadReadSubForests :: Tree TreeView -> Tree TreeView
unloadReadSubForests t = case rootLabel t of
    TVMessage m | "unread" `notElem` messageTags m ->
        t { subForest =
                map unloadReadSubForests $
                filter (isTVMessage . rootLabel) $
                subForest t
          }
    _ ->
        t { subForest =
                map unloadReadSubForests $
                subForest t
          }


unloadPartsWithFilename :: Forest TreeView -> Forest TreeView
unloadPartsWithFilename =
    map rewriteTree
  where
    f x@Node{..} = case rootLabel of
      TVMessagePart _ mp ->
          case partContentFilename mp of
              Nothing -> x
              Just _ ->
                  x { subForest = [] }
      _ -> x

    rewriteTree x =
        let x' = f x
        in x' { subForest = map rewriteTree $ subForest x' }


findFirsts :: (a -> Bool) -> Forest a -> Forest a
findFirsts p =
    concatMap rec
  where
    rec t@Node{..} =
      if p rootLabel
          then [t]
          else concatMap rec subForest
