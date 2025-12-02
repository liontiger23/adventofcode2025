{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle2
    ( puzzle2
    ) where

import Util
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.List (nub)

puzzle2 :: Int -> Solution Int
puzzle2 1 = sum . concatMap (invalidIn . parseRange) . splitOn "," . fromJust . safeHead
puzzle2 2 = sum . concatMap (invalidByNIn . parseRange) . splitOn "," . fromJust . safeHead

invalidIn :: (Int, Int) -> [Int]
invalidIn (l, r) = takeWhile (<= r) $ dropWhile (< l) invalid

invalid :: [Int]
invalid = invalidN 2

invalidN :: Int -> [Int]
invalidN n = go 1
  where
    go :: Int -> [Int]
    go part = read (concat $ replicate n (show part)) : go (part + 1)

parseRange :: String -> (Int, Int)
parseRange str = case map read (splitOn "-" str) of
  [start, end] -> (start, end)
  xs -> error $ show xs

invalidByNIn :: (Int, Int) -> [Int]
invalidByNIn (l, r) = nub $ concatMap (takeWhile (<= r) . dropWhile (< l)) $
  take (length $ show r) invalidByN

-- >>> map (take 5) $ take 5 invalidByN 
-- [[11,22,33,44,55],[111,222,333,444,555],[1111,2222,3333,4444,5555],[11111,22222,33333,44444,55555],[111111,222222,333333,444444,555555]]
--
invalidByN :: [[Int]]
invalidByN = map invalidN [2..]

