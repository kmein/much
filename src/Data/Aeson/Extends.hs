module Data.Aeson.Extends (module Data.Aeson.Extends) where

import Data.Aeson as Data.Aeson.Extends

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy.Encoding as LT


eitherDecodeLenient' :: FromJSON a => LBS.ByteString -> Either String a
eitherDecodeLenient' s =
    either (const $ eitherDecode' $ lenientReencode s) id (eitherDecode' s)
  where
    lenientReencode = LT.encodeUtf8 . LT.decodeUtf8With TE.lenientDecode

