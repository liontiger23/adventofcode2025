{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle3
    ( puzzle3
    ) where

import Util

puzzle3 :: Int -> Solution Int
puzzle3 1 = sum . map (maxJoltage 2)
puzzle3 2 = undefined

maxJoltage :: Int -> [Char] -> Int
maxJoltage n = read . maximum . splits n

-- >>> splits 2 "123123"
-- ["12","23","31","12","23"]
splits :: Int -> [Char] -> [[Char]]
splits _ [] = []
splits 1 (x : xs) = [x] : splits 1 xs
splits n (x : xs) = map (x :) (splits (n - 1) xs) <> splits n xs
