module Much.Event where

import Blessings
import Much.State
import Scanner

data Event =
    EFlash (Blessings String) |
    EScan Scan |
    EShutdown |
    EReload |
    EResize Int Int |
    EStateGet (State -> IO ())
  deriving Show
