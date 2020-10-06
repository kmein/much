{-# LANGUAGE DeriveGeneric #-}
module Much.Config where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Much.State (ColorConfig)
import qualified Data.Text as T
import qualified Data.Map as M

data Config = Config
  { colorConfig :: Maybe (ColorConfig Maybe)
  , query :: Maybe String
  , tagSymbols :: Maybe (M.Map T.Text T.Text)
  , attachmentOverwrite :: Maybe Bool
  , attachmentDirectory :: Maybe FilePath
  } deriving (Generic, Show)

instance FromJSON Config
