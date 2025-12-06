{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle5
    ( puzzle5
    ) where

import Util
import Data.List.Split (splitOn)

puzzle5 :: Int -> Solution Int
puzzle5 1 = length . uncurry onlyMatching . parseInput
puzzle5 2 = sum . map rangeSize . foldl intersect [] . fst . parseInput

onlyMatching :: [Range] -> [ID] -> [[Range]]
onlyMatching rs = filter (not . null) . map (`matchingRanges` rs)

matchingRanges :: ID -> [Range] -> [Range]
matchingRanges i = filter (matches i)

matches :: ID -> Range -> Bool
matches i (l, r) = l <= i && i <= r

-- >>> foldl intersect [] [(1,3), (2,2), (3,3), (1,4), (5,6), (1, 7)]
-- [(1,3),(4,4),(5,6),(7,7)]
intersect :: [Range] -> Range -> [Range]
intersect [] y = [y]
intersect (x@(l1, r1) : rs) y@(l2, r2)
  | r1 <  l2 || r2 <  l1 = x : intersect rs y
  | l1 <= l2 && r2 <= r1 = x : rs
  | l2 <  l1 && r1 <  r2 = intersect rs y
  | l2 <  l1             = x : intersect rs (l2, l1 - 1)
  |             r1 <  r2 = x : intersect rs (r1 + 1, r2)


parseInput :: [String] -> ([Range], [ID])
parseInput input = case splitOn [""] input of
  [rs, ids] -> (map parseRange rs, map read ids)

-- >>> parseRange "123-123"
-- (123,123)
parseRange :: String -> Range
parseRange str = case splitOn "-" str of
  [l, r] -> (read l, read r)

rangeSize :: Range -> Int
rangeSize (l, r) = r - l + 1

type Range = (Int, Int)
type ID = Int
