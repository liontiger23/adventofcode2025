{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Puzzle12
    ( puzzle12
    ) where

import Util
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.List (unsnoc)

puzzle12 :: Int -> Solution Int
puzzle12 1 = solve1 . parseInput
puzzle12 2 = undefined

solve1 :: ([Shape], [Region]) -> Int
solve1 (shapes, regions) = length $ filter (checkRegion shapes) regions

checkRegion :: [Shape] -> Region -> Bool
checkRegion shapes (Region n m qs) = totalCells >= shapeCells
 where
  totalCells = n * m
  shapeCells = sum $ zipWith (\s q -> length s * q) shapes qs

parseInput :: [String] -> ([Shape], [Region])
parseInput input = (map parseShape p1, map parseRegion p2)
 where
  Just (p1, p2) = unsnoc $ splitOn [""] input

-- >>> parseShape ["", "###", ".#.", "###"]
-- fromList [(0,0),(0,2),(1,0),(1,1),(1,2),(2,0),(2,2)]
parseShape :: [String] -> Shape
parseShape (_ : xs) = S.fromList $ map snd $ filter ((== '#') . fst) $ zip (concat xs) [(i, j) | j <- [0..2], i <- [0..2]]

-- >>> parseRegion "12x5: 1 0 1 0 2 2"
-- Region 12 5 [1,0,1,0,2,2]
parseRegion :: String -> Region
parseRegion s = case map read (splitOn " " $ map (\c -> if c == 'x' then ' ' else c) $ filter (/= ':') s) of
  (n : m : qs) -> Region n m qs

type Shape = S.Set (Int, Int)

data Region = Region Int Int [Int]
  deriving (Show, Eq)
