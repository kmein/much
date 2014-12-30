module Event
    ( Event (..)
    ) where

import Trammel


data Event =
    EFlash (Trammel String) |
    EKey String |
    EMouse Char Int Int | -- TODO s/Char/..
    EReload |
    EResize Int Int
  deriving Show
