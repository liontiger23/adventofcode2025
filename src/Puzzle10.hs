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
import Data.Maybe (fromJust, maybeToList, mapMaybe)

puzzle10 :: Int -> Solution Int
puzzle10 1 = sum . map (length . calcIndicatorPresses . parseMachine)
puzzle10 2 = sum . map (calcJoltagePresses . parseMachine)

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

calcJoltagePresses :: Machine -> Int
calcJoltagePresses (Machine n _ buttons target) = fromJust $ calcFrom target
 where
  enum = enumButtonPresses n buttons
  calcFrom :: [Joltage] -> Maybe Int
  calcFrom js
    | all (== 0) js = Just 0
    | any (<0) js = Nothing
    | any odd  js = safeMinimum enumChoices
    | otherwise   = safeMinimum (maybeToList ((2 *) <$> calcFrom (jdiv 2 js)) ++ enumChoices)
   where
    enumChoices = mapMaybe (\(js', k) -> (\r -> k + 2 * r) <$> calcFrom (jdiv 2 js')) choices
    choices = mapMaybe (\(ps, k) -> let js' = js `jsub` ps in if all even js' then Just (js', k) else Nothing) $ M.toList enum

jsub :: [Joltage] -> [Joltage] -> [Joltage]
jsub = zipWith (-)

jdiv :: Int -> [Joltage] -> [Joltage]
jdiv k = map (`div` k)

safeMinimum :: (Ord a) => [a] -> Maybe a
safeMinimum [] = Nothing
safeMinimum xs = Just $ minimum xs

enumButtonPresses :: Int -> [Button] -> M.Map [Joltage] Int
enumButtonPresses _ [] = M.empty
enumButtonPresses n (b : bs) = M.unionWith min bsEnum bbsEnum
 where
  bsEnum = enumButtonPresses n bs
  bbsEnum = M.fromList $ (pressJoltages (zeroJoltages n) b, 1) : map (\(j, k) -> (pressJoltages j b, k + 1)) (M.toList bsEnum)

pressJoltages :: [Joltage] -> Button -> [Joltage]
pressJoltages js 0 = js
pressJoltages (j : js) b = j + b .&. 1 : pressJoltages js (shiftR b 1)

zeroJoltages :: Int -> [Joltage]
zeroJoltages n = replicate n 0

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
