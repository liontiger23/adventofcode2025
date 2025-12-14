{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BinaryLiterals #-}

module Puzzle10
    ( puzzle10
    ) where

import Util
import Data.List.Split (splitOn)
import Data.Bits
import Data.Map ((!))
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad (forM_)

puzzle10 :: Int -> Solution Int
puzzle10 1 = sum . map (length . calcIndicatorPresses . parseMachine)
puzzle10 2 = undefined

calcIndicatorPresses :: Machine -> [Button]
calcIndicatorPresses (Machine _ target buttons _) = execState go initState ! target
 where
  initState = M.singleton 0 []
  go :: State (M.Map Indicators [Button]) ()
  go =
    forM_ buttons $ \b -> do
      is <- gets M.toList
      forM_ is $ \(i, path) -> do
        let next = pressIndicators i b
            nextPath = b : path
        v <- gets (M.lookup next)
        case v of
          Just path' | length path' <= length nextPath -> pure ()
          _  -> modify (M.insert next nextPath)

-- >>> pressIndicators 0b0110 0b0011
-- 5
-- >>> pressIndicators 0b0110 0b0101
-- 3
pressIndicators :: Indicators -> Button -> Indicators
pressIndicators i b = unmasked .|. masked
 where
  unmasked = i .&. complement b
  masked = (i .^. b) .&. b

parseMachine :: String -> Machine
parseMachine input = case stripHeadAndLast (splitOn " " $ filter (not . (`elem` "[](){}")) input) of
  (indicators, buttons, joltages) -> Machine
    (length indicators)
    (parseIndicators indicators)
    (map parseButton buttons)
    (parseJoltages joltages)

parseIndicators :: String -> Indicators
parseIndicators [] = 0
parseIndicators (x : xs) = shiftL (parseIndicators xs) 1 +
  case x of
    '.' -> 0
    '#' -> 1

parseButton :: String -> Button
parseButton = sum . map (setBit 0 . read) . splitOn ","

parseJoltages :: String -> [Joltage]
parseJoltages = map read . splitOn ","

stripHeadAndLast :: [String] -> (String, [String], String)
stripHeadAndLast (x : xs) = (x, init xs, last xs)

data Machine = Machine Int Indicators [Button] [Joltage]
  deriving (Show, Eq)

type Indicators = Int

type Button = Int

type Joltage = Int
