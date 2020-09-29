module Much.API.Config where

data Config = Config
    { socketPath :: FilePath
    }

emptyConfig :: Config
emptyConfig =
    Config
      { socketPath = "/tmp/much.api.sock"
      }
