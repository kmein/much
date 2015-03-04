module MappedSets (invert, mk) where

import           Control.Arrow
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set   (Set)
import qualified Data.Set as Set


mk :: (Ord a, Ord b) => [(a, [b])] -> Map a (Set b)
mk =
    Map.fromList . map (second Set.fromList)


invert :: (Ord a, Ord b) => Map a (Set b) -> Map b (Set a)
invert =
    Map.foldrWithKey invert1 Map.empty


invert1 :: (Ord a, Ord b) => a -> Set b -> Map b (Set a) -> Map b (Set a)
invert1 k v a =
    Set.foldr (upsert k) a v


upsert :: (Ord a, Ord b) => a -> b -> Map b (Set a) -> Map b (Set a)
upsert k =
    Map.alter (Just . Set.insert k . fromMaybe Set.empty)
