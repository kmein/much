module Notmuch.Class where

class HasNotmuchId a where
    notmuchId :: a -> String
