module Aoc.Day04 (
    part1,
    part2,
)
where
import Debug.Trace (traceShow)

canRemove :: [[Char]] -> [(Int, Int)]
canRemove xs = [(a, b) | a <- [0 .. w], b <- [0 .. h], tile (a, b) == '@', length (filter (== '@') $ map tile $ adjacent (a, b)) <= 4]
  where
    w = length xs - 1
    h = length (head xs) - 1
    tile (x, y) = xs !! x !! y
    adjacent (x, y) =
        [ (x + a, y + b)
        | a <- [-1 .. 1]
        , x + a >= 0
        , b <- [-1 .. 1]
        , y + b >= 0
        , x + a <= w
        , y + b <= h
        ]

part1 :: String -> String
part1 = show . length . canRemove . lines

part2 :: String -> String
part2 i = show $ f diagram
    where
        f d = let rs = canRemove d in length rs + (if not (null rs) then f (remove rs d) else 0)
        diagram = lines i
        w = length diagram - 1
        h = length (head diagram) - 1
        remove rs d = [[if (a, b) `elem` rs then '.' else d !! a !! b | b <- [0..h]] | a <- [0..w]]
