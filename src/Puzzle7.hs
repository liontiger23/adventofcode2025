{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle7
    ( puzzle7
    ) where

import Util
import Data.Map ((!))
import qualified Data.Map as M
import Control.Monad.State
import Data.List (nub, partition)
import Data.Monoid (Endo(..))
import Data.Maybe (fromMaybe)

puzzle7 :: Int -> Solution Int
puzzle7 1 = length . evalState (propagateBeams . (: []) . startCoordinate =<< get) . parseInput
puzzle7 2 = maximum . countBeams . execState (propagateBeams . (: []) . startCoordinate =<< get) . parseInput

-- returns coordinates of splits
propagateBeams :: [Coordinate] -> State Map [Coordinate]
propagateBeams cs = do
  m <- get
  let downCS = filter (`M.member` m) $ map (\(x, y) -> (x, y + 1)) cs
      (splitCS, nextDownCS) = partition ((== '^') . (m !)) downCS
      nextCS = nub $ nextDownCS ++ concatMap (filter (`M.member` m) . (\(x, y) -> [(x - 1, y), (x + 1, y)])) splitCS
  modify (appEndo $ mconcat $ map (Endo . (`M.insert` '|')) nextCS)
  modify (appEndo $ mconcat $ map (Endo . (`M.insert` 'X')) splitCS)
  m' <- get
  if null downCS && debug (showMap m') True
  then pure []
  else (splitCS ++) <$> propagateBeams nextCS

startCoordinate :: Map -> Coordinate
startCoordinate m = case M.keys (M.filter (== 'S') m) of
  [c] -> c

countBeams :: Map -> M.Map Coordinate Int
countBeams m = countM
 where
  countM :: M.Map Coordinate Int
  countM = M.mapWithKey countC m
  countC :: Coordinate -> Char -> Int
  countC (x, y) '|' = fromMaybe 1 $ M.lookup (x, y + 1) countM
  countC (x, y) 'X' = countM ! (x - 1, y) + countM ! (x + 1, y)
  countC (x, y) 'S' = countM ! (x, y + 1)
  countC _      '.' = 0
  countC _      '^' = 0

parseInput :: [String] -> Map
parseInput input = M.fromList
  [ ((x, y), c)
  | (y, l) <- zip [0..] input
  , (x, c) <- zip [0..] l
  ]

showMap :: Map -> String
showMap m = unlines
  [ [ m ! (x, y) | x <- [0..nx] ]
  | y <- [0..ny] ]
 where
  (nx, ny) = fst $ M.findMax m

type Map = M.Map Coordinate Char

type Coordinate = (Int, Int)
