module Much.API.Config where

import Data.Default

data Config = Config
    { socketPath :: FilePath
    }

instance Default Config where
  def =
    Config
      { socketPath = "/tmp/much.api.sock"
      }
