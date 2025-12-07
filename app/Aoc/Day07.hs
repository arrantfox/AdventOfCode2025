module Aoc.Day07 (
    part1,
    part2,
)
where

import Data.List (elemIndices, nub)
import Data.Maybe (fromJust)
import GHC.OldList (elemIndex)

start :: String -> Int
start = fromJust . elemIndex 'S' . head . lines

part1 :: String -> String
part1 i = show $ propagate [start i] $ lines i
  where
    propagate :: [Int] -> [String] -> Int
    propagate _ [] = 0
    propagate bs (l : ls) =
        let splitters = elemIndices '^' l
            splits = length $ filter (`elem` bs) splitters
            newBs = nub $ mconcat $ map (\b -> if b `elem` splitters then [b - 1, b + 1] else [b]) bs
         in splits + propagate newBs ls

part2 :: String -> String
part2 i = show $ sum $ propagate (lines i) $ initialState i
  where
    initialState :: String -> [Int]
    initialState = map (\c -> if c == 'S' then 1 else 0) . head . lines
    propagate :: [String] -> [Int] -> [Int]
    propagate [] s = s
    propagate (l : ls) s =
        let splitters = elemIndices '^' l
            indecies = [0 .. (length s - 1)]
            newState = map (\i -> (if (i - 1) `elem` splitters then s !! (i - 1) else 0) + (if i `elem` splitters then 0 else s !! i) + (if (i + 1) `elem` splitters then s !! (i + 1) else 0)) indecies
         in propagate ls newState
