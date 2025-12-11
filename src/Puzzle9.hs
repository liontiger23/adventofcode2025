{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle9
    ( puzzle9
    ) where

import Util
import Data.List.Split (splitOn)

puzzle9 :: Int -> Solution Int
puzzle9 1 = maximum . map area . allRects . map parseCoordinate
puzzle9 2 = undefined

allRects :: [Coordinate] -> [Rect]
allRects cs = [rect a b | a <- cs, b <- cs]

-- >>> rect (2,5) (9,7)
-- Rect 9 7 2 5
rect :: Coordinate -> Coordinate -> Rect
rect (x1, y1) (x2, y2) = Rect (x1 `max` x2) (y1 `max` y2) (x1 `min` x2) (y1 `min` y2)

data Rect = Rect Int Int Int Int
  deriving Show

-- >>> area $ rect (2,5) (9,7)
-- 24
area :: Rect -> Int
area (Rect n e s w) = (abs (n - s) + 1) * (abs (w - e) + 1)

parseCoordinate :: String -> Coordinate
parseCoordinate input = case map read (splitOn "," input) of [x, y] -> (x, y)

type Coordinate = (Int, Int)

