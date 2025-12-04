{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Puzzle3
    ( puzzle3
    ) where

import Util
import Data.Map (Map)
import qualified Data.Map as Map

puzzle3 :: Int -> Solution Int
puzzle3 1 = sum . map maxJoltage
puzzle3 2 = undefined

maxJoltage :: [Char] -> Int
maxJoltage = maximum . map (read . pairToList) . Map.toDescList . Map.map maximum . Map.filter (not . null) . collectLeft

-- reverse needed because fromList keeps the last duplicate key entry instead of the first
-- >>> collectLeft "123123"
-- fromList [('1',"23123"),('2',"3123"),('3',"123")]
collectLeft :: [Char] -> Map Char [Char]
collectLeft = Map.fromList . reverse . splits

-- >>> splits "123123"
-- [('1',"23123"),('2',"3123"),('3',"123"),('1',"23"),('2',"3"),('3',"")]
splits :: [Char] -> [(Char, [Char])]
splits [] = []
splits (x : xs) = (x, xs) : splits xs

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]
