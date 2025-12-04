{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle3
    ( puzzle3
    ) where

import Util
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.List (tails)

puzzle3 :: Int -> Solution Int
puzzle3 1 = sum . map (maxJoltage 2)
puzzle3 2 = sum . map (maxJoltage 12)

maxJoltage :: Int -> [Char] -> Int
maxJoltage n s = joltages s ! (n, s)

-- >>> joltages "123"
-- fromList [((1,""),0),((1,"123"),3),((1,"23"),3),((1,"3"),3),((2,""),0),((2,"123"),23),((2,"23"),23),((2,"3"),0),((3,""),0),((3,"123"),123),((3,"23"),0),((3,"3"),0)]
joltages :: [Char] -> Map (Int, [Char]) Int
joltages str = m
  where
    m = Map.fromList [((k, s), joltage k s) | k <- [1..length str], s <- tails str]
    maxJolt n xs = m ! (n, xs)
    joltage :: Int -> [Char] -> Int
    joltage _ [] = 0
    joltage 1 (x : xs) = read [x] `max` maxJolt 1 xs
    joltage n (x : xs)
      | t > 0     = read (x : show (maxJolt (n - 1) xs)) `max` maxJolt n xs
      | otherwise = maxJolt n xs
      where
        t = maxJolt (n - 1) xs

