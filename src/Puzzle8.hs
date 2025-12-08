{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle8
    ( puzzle8
    ) where

import Util
import qualified Data.Map as M
import Data.List.Split (splitOn)
import DisjointSet (DisjointSet)
import qualified DisjointSet as DS
import Data.List (sortOn, sortBy)
import Control.Monad.State
import Data.Ord (comparing, Down (..))

puzzle8 :: Int -> Solution Int
puzzle8 1 = product . take 3 . sortBy (comparing Down) . map length . DS.equivalenceClasses . connectN 1000 . dists . map parseCoordinate
puzzle8 2 = undefined

connectN :: Int -> M.Map (Coordinate, Coordinate) Int -> DisjointSet Coordinate
connectN n m = execState (mapM_ connect $ take n $ map fst sorted) $ DS.fromList allCoords
 where
  allCoords = concatMap (\(x, y) -> [x, y]) $ M.keys m
  sorted :: [((Coordinate, Coordinate), Int)]
  sorted = sortOn snd $ M.toList m

connect :: (Coordinate, Coordinate) -> State (DisjointSet Coordinate) ()
connect (x, y) = do
  modify (DS.insert x . DS.insert y)
  DS.unionS x y


dists :: [Coordinate] -> M.Map (Coordinate, Coordinate) Int
dists cs = M.fromList [((a, b), dist2 a b) | a <- cs, b <- cs, a /= b, a < b]

parseCoordinate :: String -> Coordinate
parseCoordinate input = case map read (splitOn "," input) of [x, y, z] -> (x, y, z)

type Coordinate = (Int, Int, Int)

dist2 :: Coordinate -> Coordinate -> Int
dist2 (x1, y1, z1) (x2, y2, z2) = square (x1 - x2) + square (y1 - y2) + square (z1 - z2)

square :: Int -> Int
square x = x^(2 :: Int)
