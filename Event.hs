module Event where

import Trammel


data Event =
    EFlash (Trammel String) |
    EKey String |
    EMouse MouseInfo |
    EReload |
    EResize Int Int
  deriving Show


data MouseInfo = MouseInfo
    { mouseButton :: Int -- 0 = release
    , mouseShift :: Bool
    , mouseMeta :: Bool
    , mouseControl :: Bool
    , mouseX :: Int
    , mouseY :: Int
    }
  deriving Show
