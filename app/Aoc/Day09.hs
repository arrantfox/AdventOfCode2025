module Aoc.Day09 (
    part1,
    part2,
)
where

import Data.List (sort)
import Data.List.Split (splitOn)

parse :: String -> [(Int, Int)]
parse i = map ((\[a, b] -> (a, b)) . map read . splitOn ",") $ lines i

pairs :: [a] -> [(a, a)]
pairs a = [(x, y) | (i, x) <- zip [1 ..] a, y <- drop i a]

area ((a, b), (a1, b1)) = (abs (a - a1) + 1) * (abs (b - b1) + 1)

part1 :: String -> String
part1 i = show $ maximum (map area $ pairs $ parse i)

part2 :: String -> String
part2 i = ""
