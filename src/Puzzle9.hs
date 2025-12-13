{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Puzzle9
    ( puzzle9
    ) where

import Util
import Data.List.Split (splitOn)
import Data.Ix
import Data.Map ((!))
import qualified Data.Map.Strict as M
import Control.Monad.State
import Data.List (sort)

puzzle9 :: Int -> Solution Int
puzzle9 1 = maximum . map area . allRects . map parseCoordinate
puzzle9 2 = maximum . map area . allFilledRects . map parseCoordinate

allRects :: [Coordinate] -> [Rect]
allRects cs = [rect a b | a <- cs, b <- cs]

-- >>> rect (2,5) (9,7)
-- Rect 7 9 5 2
rect :: Coordinate -> Coordinate -> Rect
rect (x1, y1) (x2, y2) = Rect (y1 `max` y2) (x1 `max` x2) (y1 `min` y2) (x1 `min` x2)

data Rect = Rect Int Int Int Int
  deriving Show

-- >>> area $ rect (2,5) (9,7)
-- 24
area :: Rect -> Int
area (Rect n e s w) = (abs (n - s) + 1) * (abs (e - w) + 1)

parseCoordinate :: String -> Coordinate
parseCoordinate input = case map read (splitOn "," input) of [x, y] -> (x, y)

type Coordinate = (Int, Int)

allFilledRects :: [Coordinate] -> [Rect]
allFilledRects cs = filter (filled tiles) $ allRects cs
 where
  tiles = fillLines cs

filled :: TileRanges -> Rect -> Bool
filled m r = all (\(HSegm y lr) -> contains lr (m ! y)) $ rectLines r


fillLines :: [Coordinate] -> TileRanges
fillLines cs = execState (do
    mapM_ collectV segs
    modify (M.map (fillRanges . sort))
    mapM_ fillH segs
    modify (M.map (joinRanges . sort))
  ) M.empty
 where
  segs = path cs
  collectV :: Segm -> State TileRanges ()
  collectV (HSegm _ _) = pure ()
  collectV (VSegm x r) = mapM_ (\y -> modify (M.insertWith (++) y [(x,x)])) (init $ range r)
  fillH :: Segm -> State TileRanges ()
  fillH (HSegm y r) = modify (M.insertWith (++) y [r])
  fillH (VSegm _ _) = pure ()

fillRanges :: [Range] -> [Range]
fillRanges []  = []
fillRanges (x : y : xs) = expand x y : fillRanges xs

joinRanges :: [Range] -> [Range]
joinRanges []  = []
joinRanges [x] = [x]
joinRanges (x : y : xs)
  | intersects x y = joinRanges (expand x y : xs)
  | otherwise      = x : joinRanges (y : xs)

expand :: Range -> Range -> Range
expand (l1, _) (_, r2) = (l1, r2)

intersects :: Range -> Range -> Bool
intersects d (l, r) = inRange d l || inRange d r

type TileRanges = M.Map Int [Range]

contains :: Range -> [Range] -> Bool
contains r = any (subrange r)

subrange :: Range -> Range -> Bool
subrange (l1, r1) r = inRange r l1 && inRange r r1

type Range = (Int, Int)

path :: [Coordinate] -> [Segm]
path (c : cs) = zipWith segm (c : cs) (cs ++ [c])

data Segm = VSegm Int Range | HSegm Int Range
  deriving (Show, Eq)

rectLines :: Rect -> [Segm]
rectLines (Rect n e s w) = [ HSegm y (w, e) | y <- [s..n] ]

segm :: Coordinate -> Coordinate -> Segm
segm (x1, y1) (x2, y2)
  | x1 == x2 = VSegm x1 (sorted (y1, y2))
  | y1 == y2 = HSegm y1 (sorted (x1, x2))

sorted :: Ord a => (a, a) -> (a, a)
sorted (x, y) = (x `min` y, x `max` y)

