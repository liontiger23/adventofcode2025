{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle6
    ( puzzle6
    ) where

import Util
import Data.Char (isSpace)
import Data.List (uncons)
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)

puzzle6 :: Int -> Solution Int
puzzle6 1 = sum . map calc . parseInput . reverse . map skipSpaces
puzzle6 2 = sum . map (calc . parseEquation2) . splitOn [[]] . map skipSpaces . transpose

parseInput :: [[String]] -> [Equation]
parseInput ([] : _) = []
parseInput ((op:ops) : args) = eq : parseInput (ops : map snd args')
 where
  args' = fromJust $ mapM uncons args
  eq = case op of
    "+" -> Add $ map (read . fst) args'
    "*" -> Mul $ map (read . fst) args'

calc :: Equation -> Int
calc (Add xs) = sum xs
calc (Mul xs) = product xs

data Equation = Add [Int] | Mul [Int]

-- >>> skipSpaces "   123 3241  234 "
-- ["123","3241","234"]
-- >>> skipSpaces "     "
-- []
skipSpaces :: String -> [String]
skipSpaces = go ""
 where
  go ""  [] = []
  go acc [] = [acc]
  go acc (x : xs)
    | isSpace x = if null acc then go "" xs else acc : go "" xs
    | otherwise = go (acc ++ [x]) xs

parseEquation2 :: [[String]] -> Equation
parseEquation2 (argOp : args) =  case op of
  '+' -> Add $ map read (arg : concat args)
  '*' -> Mul $ map read (arg : concat args)
 where
  -- this looks weird, but it is needed to handle cases
  -- where argOp is either ["123", "+"] or ["123+"]
  (op, arg) = case reverse (concat argOp) of
    (op' : revArg) -> (op', reverse revArg)
parseEquation2 xs = error $ show xs
