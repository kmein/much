{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Notmuch.SearchResult where

import Control.Applicative
import Data.Aeson
import Data.Text
import Data.Time.Clock
import Data.Time.Clock.POSIX


newtype ThreadID = ThreadID String
  deriving (Show,Read,Eq,FromJSON,ToJSON)


-- | A single entry returned from the notmuch search command.
data SearchResult = SearchResult {
      searchThread :: ThreadID
    , searchTime :: UTCTime
    , searchDateRel :: Text
    , searchSubject :: Text
    , searchAuthors :: Text
    , searchQuery :: [Maybe Text] -- TODO (Text, Maybe Text)
    , searchTags :: [Text]
    , searchMatched :: Int
    , searchTotal :: Int
    }
  deriving (Show)


instance Eq SearchResult where
    s1 == s2 =
        searchThread s1 == searchThread s2


instance FromJSON SearchResult where
    parseJSON (Object v) = SearchResult <$> v .: "thread"
                                        <*> (posixSecondsToUTCTime . fromInteger <$> v .: "timestamp")
                                        <*> v .: "date_relative"
                                        <*> v .:? "subject" .!= ""
                                        <*> v .:? "authors" .!= ""
                                        <*> v .:? "query" .!= []
                                        <*> v .: "tags"
                                        <*> v .: "matched"
                                        <*> v .: "total"
    parseJSON x = fail $ "Error parsing search: " ++ show x

--instance ToJSON SearchResult where
--    toJSON s = object [ "thread" .= searchThread s
--                      , "time" .= searchTime s
--                      , "date_relative" .= searchDateRel s
--                      , "subject" .= searchSubject s
--                      , "authors" .= searchAuthors s
--                      , "tags" .= searchTags s
--                      , "matched" .= searchMatched s
--                      , "total" .= searchTotal s
--                      ]
