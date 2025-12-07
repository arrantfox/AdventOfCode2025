module Aoc.Day05 (
    part1,
    part2,
)
where

import Control.Monad (join)
import Data.List.Split (splitOn)

type Range = (Integer, Integer)
type Id = Integer

parse :: [Char] -> ([Range], [Id])
parse input = let (r : i : _) = map lines $ splitOn "\n\n" input in (map (toTuple . map read . splitOn "-") r, map read i)
  where
    toTuple (a : b : _) = (a, b)

inRange :: Id -> Range -> Bool
inRange i (min, max) = i >= min && i <= max

isFresh :: [Range] -> Id -> Bool
isFresh rs i = any (inRange i) rs

part1 :: String -> String
part1 i = show $ length $ filter (isFresh rs) ids
  where
    (rs, ids) = parse i

merge :: Range -> Range -> Maybe Range
merge (a, b) (x, y)
    | inRange x (a, b) || inRange y (a, b) = Just (min a x, max b y)
    | otherwise = Nothing

part2 :: String -> String
part2 i = show $ allFresh $ iterate mergeFirst rs !! length rs
  where
    (rs, _) = parse i
    mergeFirst (a : b : rs) = maybe (b : mergeFirst (a : rs)) (: rs) (merge a b)
    mergeFirst [a] = [a]
    allFresh = sum . map (\(a, b) -> b - a + 1)
