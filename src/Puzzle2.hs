{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle2
    ( puzzle2
    ) where

import Util
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

puzzle2 :: Int -> Solution Int
puzzle2 1 = sum . concatMap (invalidIn . parseRange) . splitOn "," . fromJust . safeHead
puzzle2 2 = undefined

invalidIn :: (Int, Int) -> [Int]
invalidIn (l, r) = takeWhile (<= r) $ dropWhile (< l) invalid

invalid :: [Int]
invalid = go 1
  where
    go :: Int -> [Int]
    go part = read (show part <> show part) : go (part + 1)

parseRange :: String -> (Int, Int)
parseRange str = case map read (splitOn "-" str) of
  [start, end] -> (start, end)
  xs -> error $ show xs
