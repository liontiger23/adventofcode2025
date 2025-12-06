{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle5
    ( puzzle5
    ) where

import Util
import Data.List.Split (splitOn)

puzzle5 :: Int -> Solution Int
puzzle5 1 = length . uncurry onlyMatching . parseInput
puzzle5 2 = undefined

onlyMatching :: [Range] -> [ID] -> [[Range]]
onlyMatching rs = filter (not . null) . map (`matchingRanges` rs)

matchingRanges :: ID -> [Range] -> [Range]
matchingRanges i = filter (matches i)

matches :: ID -> Range -> Bool
matches i (l, r) = l <= i && i <= r


parseInput :: [String] -> ([Range], [ID])
parseInput input = case splitOn [""] input of
  [rs, ids] -> (map parseRange rs, map read ids)

-- >>> parseRange "123-123"
-- (123,123)
parseRange :: String -> Range
parseRange str = case splitOn "-" str of
  [l, r] -> (read l, read r)


type Range = (Int, Int)
type ID = Int
