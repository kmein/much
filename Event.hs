module Event
    ( Event (..)
    ) where

import Trammel


data Event =
    EFlash (Trammel String) |
    EKey String |
    EReload |
    EResize Int Int
  deriving Show
