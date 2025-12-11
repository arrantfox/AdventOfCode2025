module Aoc.Day08 (
    part1,
    part2,
)
where

import Data.List (findIndex, nub, sortOn)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import qualified Data.Ord
import GHC.OldList (sortBy)

type Box = (Int, Int, Int)

parse :: String -> [Box]
parse = map (toTuple . map read . splitOn ",") . lines
  where
    toTuple [a, b, c] = (a, b, c)

addConnection :: [[Int]] -> (Int, Int) -> [[Int]]
addConnection cs (a, b) =
    let (ac, r) = maybe ([a], cs) (extract cs) (findIndex (a `elem`) cs)
        (bc, r2) = maybe ([b], r) (extract r) (findIndex (b `elem`) r)
     in nub (ac ++ bc) : r2

extract cs i = let (a, r : b) = splitAt i cs in (r, a ++ b)

closest :: [Box] -> [(Int, Int)]
closest xs = sortOn (\(a, b) -> distSqr (xs !! a) (xs !! b)) $ pairs [0 .. (length xs - 1)]

pairs :: [a] -> [(a, a)]
pairs a = [(x, y) | (i, x) <- zip [1 ..] a, y <- drop i a]

distSqr :: Box -> Box -> Int
distSqr (x, y, z) (a, b, c) = ((x - a) ^ 2) + ((y - b) ^ 2) + ((z - c) ^ 2)

part1 :: String -> String
part1 = show . product . take 3 . sortBy (comparing Data.Ord.Down) . map length . foldl addConnection [] . take 1000 . closest . parse

part2 :: String -> String
part2 i = show $ connectAll [] (closest boxes)
  where
    boxes = parse i
    fst (a, _, _) = a
    connectAll :: [[Int]] -> [(Int, Int)] -> Int
    connectAll cs (x@(f, s) : xs)
        | length newCs == 1 && length (head newCs) == length boxes = fst (boxes !! f) * fst (boxes !! s)
        | otherwise = connectAll newCs xs
      where
        newCs = addConnection cs x
