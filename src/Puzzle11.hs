{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

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
puzzle11 2 = undefined

data Node a = Node a [Node a]
  deriving (Show, Eq, Functor, Foldable)


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
