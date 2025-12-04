module Main where

import Aoc.Days
import Control.Monad (unless)
import Options.Applicative

data Args = Args
    { day :: Int
    , part1 :: Bool
    , part2 :: Bool
    , example :: Bool
    }
    deriving (Show)

args :: Parser Args
args =
    Args
        <$> option
            auto
            ( long "day"
                <> short 'd'
                <> help "Day to run"
                <> metavar "NUM"
            )
        <*> switch
            ( long "part1"
                <> short '1'
                <> help "Run only part1"
            )
        <*> switch
            ( long "part2"
                <> short '2'
                <> help "Run only part2"
            )
        <*> switch
            ( long "example"
                <> short 'e'
                <> help "Run on example input"
            )

main :: IO ()
main = greet =<< execParser opts
  where
    opts =
        info
            (args <**> helper)
            ( fullDesc
                <> header "Advent Of Code 2025"
            )

greet :: Args -> IO ()
greet args@Args{day = day, part1 = only1, part2 = only2, example = e} = do
    input <- readFile inputPath

    unless only2 $ putStrLn $ "Day " ++ show day ++ " (part 1): " ++ part1 input
    unless only1 $ putStrLn $ "Day " ++ show day ++ " (part 2): " ++ part2 input
  where
    (part1, part2) = Aoc.Days.days !! (day - 1)
    inputPath =
        if e
            then
                "./inputs/example" ++ show day ++ ".txt"
            else "./inputs/input" ++ show day ++ ".txt"
