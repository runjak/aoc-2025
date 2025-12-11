module Main where

import Data.List (partition)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

exampleFile1 = "./inputs/11/example1.txt"
exampleFile2 = "./inputs/11/example2.txt"

inputFile = "./inputs/11/input.txt"

type ReactorGraph = Map String [String]

readInput :: String -> ReactorGraph
readInput = Map.fromList . mapMaybe (go . words) . lines
  where
    go :: [String] -> Maybe (String, [String])
    go (label : rest) = Just (init label, rest)
    go [] = Nothing

next :: String -> ReactorGraph -> [String]
next from reactor = fromMaybe [] $ Map.lookup from reactor

fromTo :: String -> String -> ReactorGraph -> Int
fromTo from to reactor =
  let (arrived, rest) = partition (== to) $ next from reactor
   in sum $ (length arrived :) $ map (\f -> fromTo f to reactor) rest

solution1 :: String -> String
solution1 = show . fromTo "you" "out" . readInput

unreachable :: String -> ReactorGraph -> ReactorGraph
unreachable node reactor = Map.map (filter (/=node)) reactor

solve2 :: ReactorGraph -> Int
solve2 reactor = do
  let noFft = unreachable "fft" reactor
      noDac = unreachable "dac" reactor
      noBoth = unreachable "fft" noDac
  -- sum [
  --   fromTo "svr" "dac" reactor * fromTo "dac" "fft" reactor * fromTo "fft" "out" reactor,
  --   fromTo "svr" "fft" reactor * fromTo "fft" "dac" reactor * fromTo "dac" "out" reactor
  --   ]
  sum [
    fromTo "svr" "dac" noFft * fromTo "dac" "fft" noDac * fromTo "fft" "out" noBoth,
    fromTo "svr" "fft" noDac * fromTo "fft" "dac" noFft * fromTo "dac" "out" noBoth
    ]

test = readInput <$> readFile inputFile

solution2 :: String -> String
solution2 = show . solve2 . readInput

main :: IO ()
main = do
  putStrLn "Task 11"

  example1 <- readFile exampleFile1
  example2 <- readFile exampleFile2
  input <- readFile inputFile

  putStrLn "solution1:"
  putStrLn $ "example: " <> solution1 example1
  putStrLn $ "input: " <> solution1 input

  putStrLn "solution2:"
  putStrLn $ "example: " <> solution2 example2
  putStrLn $ "input: " <> solution2 input
