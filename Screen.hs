{-# LANGUAGE RecordWildCards #-}
module Screen (Screen(..), setScreen, withScreen) where

import Control.Exception
import Data.List
import System.IO

data Screen = Screen
    { stdinEcho :: Bool
    , stdinBufferMode :: BufferMode
    , stdoutBufferMode :: BufferMode
    , decsetPm :: [Int]
    , decrstPm :: [Int]
    }

setScreen :: Screen -> IO Screen
setScreen Screen{..} = get <* set where
  get = Screen <$> hGetEcho stdin
               <*> hGetBuffering stdin
               <*> hGetBuffering stdout
               <*> pure decrstPm
               <*> pure decsetPm
  set = do
      hSetEcho stdin stdinEcho
      hSetBuffering stdin stdinBufferMode
      hSetBuffering stdout stdoutBufferMode
      hPutStr stdout $ "\ESC[?" ++ intercalate ";" (map show decsetPm) ++ "h"
      hPutStr stdout $ "\ESC[?" ++ intercalate ";" (map show decrstPm) ++ "l"
      hFlush stdout

withScreen :: Screen -> (Screen -> IO a) -> IO a
withScreen s = bracket (setScreen s) setScreen
