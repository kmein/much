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


padl :: Int -> a -> [a] -> [a]
padl n c s =
    if length s < n
        then padl n c (c:s)
        else s

padr :: Int -> a -> [a] -> [a]
padr n c s =
    if length s < n
        then padr n c (s ++ [c])
        else s
