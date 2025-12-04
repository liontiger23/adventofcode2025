{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle4
    ( puzzle4
    ) where

import Util
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Control.Monad.State

puzzle4 :: Int -> Solution Int
puzzle4 1 = length . evalState detectAccessible . parseInput
puzzle4 2 = undefined

detectAccessible :: State Map [Coordinate]
detectAccessible = do
  m <- get
  let accessible c =
        m ! c == '@' &&
        length (filter (== '@') $ neighbours m c) < 4
  pure $ filter accessible $ M.keys m

neighbours :: Map -> Coordinate -> [Char]
neighbours m (x, y) =
  [ c
  | x' <- [x - 1..x + 1]
  , y' <- [y - 1..y + 1]
  , (x, y) /= (x', y')
  , c <- maybeToList $ M.lookup (x', y') m
  ]

parseInput :: [String] -> Map
parseInput input = M.fromList
  [ ((x, y), c)
  | (x, l) <- zip [0..] input
  , (y, c) <- zip [0..] l
  ]

type Map = M.Map Coordinate Char

type Coordinate = (Int, Int)
