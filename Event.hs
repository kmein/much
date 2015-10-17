module Event where

import Blessings
import Scanner

data Event =
    EFlash (Blessings String) |
    EScan Scan |
    EReload |
    EResize Int Int
  deriving Show
