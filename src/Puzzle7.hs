{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle7
    ( puzzle7
    ) where

import Util
import Data.Map ((!))
import qualified Data.Map as M
import Control.Monad.State
import Data.List (nub, partition)

puzzle7 :: Int -> Solution Int
puzzle7 1 = length . evalState (propagateBeams . (: []) . startCoordinate =<< get) . parseInput
puzzle7 2 = undefined

-- returns coordinates of splits
propagateBeams :: [Coordinate] -> State Map [Coordinate]
propagateBeams cs = do
  m <- get
  let downCS = filter (`M.member` m) $ map (\(x, y) -> (x, y + 1)) cs
      (splitCS, nextDownCS) = partition ((== '^') . (m !)) downCS
      nextCS = nub $ nextDownCS ++ concatMap (filter (`M.member` m) . (\(x, y) -> [(x - 1, y), (x + 1, y)])) splitCS
  if null downCS
  then pure []
  else (splitCS ++) <$> propagateBeams nextCS

startCoordinate :: Map -> Coordinate
startCoordinate m = case M.keys (M.filter (== 'S') m) of
  [c] -> c

parseInput :: [String] -> Map
parseInput input = M.fromList
  [ ((x, y), c)
  | (y, l) <- zip [0..] input
  , (x, c) <- zip [0..] l
  ]

type Map = M.Map Coordinate Char

type Coordinate = (Int, Int)
