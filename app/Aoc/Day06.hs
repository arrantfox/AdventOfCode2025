module Aoc.Day06 (
    part1,
    part2,
)
where
import Data.List (transpose)
import Data.List.Split (splitWhen)


part1 :: String -> String
part1 = show . sum . map solve . parse
    where
        parse = map (\l -> (map read $ init l, last l)) . transpose . map words . lines

        solve (ns, "+") = sum ns
        solve (ns, "*") = product ns

part2 :: String -> String
part2 = show . sum . map solve . parse
    where
        parse i = zip (map (map read) $ splitWhen (all (== ' ')) $ transpose $ init $ lines i) $ words $ last $ lines i

        solve (ns, "+") = sum ns
        solve (ns, "*") = product ns
