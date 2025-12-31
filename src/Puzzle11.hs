{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle11
    ( puzzle11
    ) where

import Util
import Data.Map ((!))
import qualified Data.Map as M
import Data.List (uncons)
import Data.Maybe (fromJust)

puzzle11 :: Int -> Solution Int
puzzle11 1 = length . pathsFromRoot "out" . buildGraph "you" . parseInput
puzzle11 2 = solve2 . parseInput

data Node a = Node a [Node a]
  deriving (Show, Eq)

parseInput :: [String] -> M.Map String [String]
parseInput = M.fromList . (("out", []) :) . map (fromJust . uncons . words . filter (/= ':'))

buildGraph :: (Ord a) => a -> M.Map a [a] -> Node a
buildGraph start m = nodes ! start
 where
  nodes = M.mapWithKey (\x outs -> Node x (map (nodes !) outs)) m

pathsFromRoot :: Eq a => a -> Node a -> [[a]]
pathsFromRoot to (Node from outs)
  | to == from = [[from]]
  | otherwise  = concatMap (map (from :) . filter (not . null) . pathsFromRoot to) outs

solve2 :: M.Map String [String] -> Int
solve2 m 
  | paths ! ("fft", "dac") > 0 = paths ! ("svr", "fft") * paths ! ("fft", "dac") * paths ! ("dac", "out")
  | otherwise                  = paths ! ("svr", "dac") * paths ! ("dac", "fft") * paths ! ("fft", "out")
 where
  paths :: M.Map (String, String) Int
  paths = M.fromList [((from, to), pathsNum from to) | from <- M.keys m, to <- M.keys m]
  pathsNum from to
    | from == to = 1
    | otherwise  = sum (map (\x -> paths ! (x, to)) (m ! from))
