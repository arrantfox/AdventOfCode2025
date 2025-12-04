module Aoc.Day02 (
    part1,
    part2,
)
where

import Data.List (nub)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)

parse :: String -> [(Int, Int)]
parse = map (toTuple . map read . splitOn "-") . splitOn "," . init
  where
    toTuple [a, b] = (a, b)

part1 :: String -> String
part1 = show . sum . mconcat . map invalids . parse
  where
    invalids (a, b) = filter (>= a) $ takeWhile (<= b) $ map double [(takeHalf a) ..]
    takeHalf n = read $ '0' : take (length (show n) `div` 2) (show n)
    double n = n + n * 10 ^ length (show n)

part2 :: String -> String
part2 = show . sum . mconcat . map invalids . parse
  where
    invalids (a, b) = nub $ mconcat $ map (nthInvalids (a, b)) [2 .. length (show b)]
    nthInvalids (a, b) n =
        let
            l = length (show a) `div` n
            minChunk = read $ '0' : take l (show a) :: Integer
         in
            filter (>= a) $ takeWhile (<= b) $ map (repeatNumber n) [minChunk ..]
    repeatNumber n = read . mconcat . replicate n . show
