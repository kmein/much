{-# LANGUAGE DeriveGeneric #-}
module Much.Config where

import GHC.Generics (Generic)
import GHC.Word (Word8)
import Data.Aeson (FromJSON)
import Much.State (ColorConfig)
import qualified Data.Text as T
import qualified Data.Map as M

data Config = Config
  { colorConfig :: Maybe (ColorConfig (Maybe [Word8]))
  , query :: Maybe String
  , tagSymbols :: Maybe (M.Map T.Text T.Text)
  , attachmentOverwrite :: Maybe Bool
  , attachmentDirectory :: Maybe FilePath
  } deriving (Generic, Show)

instance FromJSON Config
