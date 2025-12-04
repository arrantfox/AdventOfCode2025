module Aoc.Day03 (
    part1,
    part2,
)
where

import Data.Char (digitToInt)

part1 :: String -> String
part1 = show . sum . map (maxJ . map digitToInt) . lines
  where
    maxJ xs =
        let a = foldl max 0 (init xs)
            b = foldl max 0 $ tail $ dropWhile (/= a) xs
         in a * 10 + b

part2 :: String -> String
part2 = show . sum . map (maxJ 11 . map digitToInt) . lines
  where
    maxJ (-1) _ = 0
    maxJ n xs = let a = nthJ n xs in a * 10 ^ n + maxJ (n - 1) (tail $ dropWhile (/= a) xs)
    nthJ n xs = foldl max 0 $ take (length xs - n) xs
