{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle1
    ( puzzle1
    ) where

import Util

puzzle1 :: Int -> Solution Int
puzzle1 1 = length . filter (== 0) . scanl (rotate 100) 50 . map distance
puzzle1 2 = sum . map snd . scanl (rotateClicks 100) (50, 0) . map distance

-- >>> rotate 100 50 (-68)
-- 82
-- >>> rotate 100 82 (-30)
-- 52
-- >>> rotate 100 52 (48)
-- 0
-- >>> scanl (rotate 100) 50 [-68, -30, 48]
-- [50,82,52,0]
rotate :: Int -> Int -> Int -> Int
rotate size x y = (x + y) `mod` size

distance :: String -> Int
distance ('L' : num) = - (read num)
distance ('R' : num) =    read num

-- >>> scanl (rotateClicks 100) (50, 0) [-68, -30, 48, -5, 60, -55, -1, -99, 14, -82]
-- [(50,0),(82,1),(52,0),(0,1),(95,0),(55,1),(0,1),(99,0),(0,1),(14,0),(32,1)]
-- >>> scanl (rotateClicks 100) (50, 0) [1000, 1050, 50, -1000, -1050]
-- [(50,0),(50,10),(0,11),(50,0),(50,10),(0,11)]
rotateClicks :: Int -> (Int, Int) -> Int -> (Int, Int)
rotateClicks size (x, _) y = (res, clicks)
  where
    res = rotate size x y
    clicks = fullClicks + sign
    fullClicks = abs y `div` size
    y' = y - (signum y * fullClicks * size)
    sign = if res == 0 || (x + y' < 0 && x /= 0) || (x + y' > size) then 1 else 0

