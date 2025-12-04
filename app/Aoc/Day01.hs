module Aoc.Day01 (
    part1,
    part2,
)
where

parse :: String -> [Int]
parse = map parseTurn . lines
  where
    parseTurn ('L' : n) = -(read n) :: Int
    parseTurn (_ : n) = read n :: Int

part1 :: String -> String
part1 = show . length . filter (== 0) . scanl (\a b -> (a + b) `mod` 100) 50 . parse

part2 :: String -> String
part2 = show . f 50 . parse
  where
    f s (x : xs) = zeros + abs (x `quot` 100) + f ((s + x) `mod` 100) xs
      where
        zeros = if (s + x `rem` 100 >= 100 || s + x `rem` 100 <= 0) && s /= 0 then 1 else 0
    f _ [] = 0
