{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle1
    ( puzzle1
    ) where

import Util

puzzle1 :: Int -> Solution Int
puzzle1 1 = length . filter (== 0) . scanl (rotate 100) 50 . map distance
puzzle1 2 = undefined

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
