{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Notmuch.Message where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Notmuch.Class
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.CaseInsensitive as CI
import qualified Data.Vector as V

import qualified Data.Tree as TR


newtype MessageID = MessageID { unMessageID :: String }
  deriving (Show, Read, Eq, FromJSON)

type MessageHeaders = M.Map (CI.CI T.Text) T.Text

data MessageContent = ContentText T.Text
                    | ContentRaw LBS8.ByteString Int
                    | ContentMultipart [MessagePart]
                    | ContentMsgRFC822 [(MessageHeaders, [MessagePart])]
  deriving (Show)

data MessagePart = MessagePart {
      partID :: Int
    , partContentType :: CI.CI T.Text
    , partContentCharset :: Maybe (CI.CI T.Text)
    , partContentFilename :: Maybe T.Text
    , partContent :: MessageContent
}
  deriving (Show)

instance Eq MessagePart where
    a == b = partID a == partID b


contentSize :: MessageContent -> Int
contentSize (ContentText text) = T.length text
contentSize (ContentMultipart parts) = sum $ map (contentSize . partContent) parts
contentSize (ContentMsgRFC822 xs) = sum $ map (sum . map (contentSize . partContent) . snd) xs
contentSize (ContentRaw _ contentLength) = contentLength


parseRFC822 :: V.Vector Value -> Parser MessageContent
parseRFC822 lst = ContentMsgRFC822 . V.toList <$> V.mapM p lst
    where
        p (Object o) = do h <- M.mapKeys CI.mk <$> o .: "headers"
                          b <- o .: "body"
                          return (h, b)
        p _ = fail "Invalid rfc822 body"

instance FromJSON MessagePart where
    parseJSON (Object v) = do
        i <- v .: "id"
        t <- CI.mk . T.toLower <$> v .: "content-type"
        x <- v .:? "content"
        f <- v .:? "filename"
        cs <- fmap CI.mk <$> v .:? "content-charset"
        maybeContentLength <- v .:? "content-length"
        let ctype = CI.map (T.takeWhile (/= '/')) t
        case (ctype, x, maybeContentLength) of
            ("multipart", Just (Array _), _) ->
                MessagePart i t cs f . ContentMultipart <$> v .: "content"

            ("message", Just (Array lst), _) | t == "message/rfc822" ->
                MessagePart i t cs f <$> parseRFC822 lst

            (_, Just (String c), _) ->
                return $ MessagePart i t cs f $ ContentText c

            (_, Nothing, Just contentLength) ->
                return $ MessagePart i t cs f $ ContentRaw "" contentLength

            (_, _, _) ->
                return $ MessagePart i t cs f $ ContentText ("Unknown content-type: " <> CI.original t)


    parseJSON x = fail $ "Error parsing part: " ++ show x


data Message = Message {
      messageId :: MessageID
    , messageTime :: UTCTime
    , messageHeaders :: MessageHeaders
    , messageBody :: [MessagePart]
    , messageExcluded :: Bool
    , messageMatch :: Bool
    , messageTags :: [T.Text]
    , messageFilename :: FilePath
}
  deriving (Show)

instance Eq Message where
    a == b = messageId a == messageId b


instance HasNotmuchId Message where
    notmuchId = unMessageID . messageId


instance FromJSON Message where
    parseJSON (Object v) = Message <$> (MessageID . ("id:"<>) <$> v .: "id")
                                   <*> (posixSecondsToUTCTime . fromInteger <$> v .: "timestamp")
                                   <*> (M.mapKeys CI.mk <$> v .: "headers")
                                   <*> v .: "body"
                                   <*> v .: "excluded"
                                   <*> v .: "match"
                                   <*> v .: "tags"
                                   <*> v .: "filename"
    parseJSON (Array _) = return $ Message (MessageID "") defTime M.empty [] True False [] ""
        where defTime = UTCTime (ModifiedJulianDay 0) 0
    parseJSON x = fail $ "Error parsing message: " ++ show x

hasTag :: T.Text -> Message -> Bool
hasTag tag = (tag `elem`) . messageTags



newtype Thread = Thread { threadForest :: TR.Forest Message }

instance FromJSON Thread where
    parseJSON (Array vs) = Thread <$> mapM parseTree (V.toList vs)
    parseJSON _ = fail "Thread is not an array"

parseTree :: Value -> Parser (TR.Tree Message)
parseTree vs@(Array _) = do
    (msg, Thread t) <- parseJSON vs
    return $ TR.Node msg t
parseTree _ = fail "Tree is not an array"
