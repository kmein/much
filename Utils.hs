module Utils where

import Control.Exception
import Data.Monoid
import System.Directory
import System.IO


withTempFile :: FilePath -> String -> ((FilePath, Handle) -> IO a) -> IO a
withTempFile tmpdir template =
    bracket (openTempFile tmpdir template) (removeFile . fst)


mintercalate :: Monoid b => b -> [b] -> b
mintercalate c (h:t) = foldl (\acc x -> acc <> c <> x) h t
mintercalate _ [] = mempty
