module Main where

import Data.List (partition)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

exampleFile = "./inputs/11/example.txt"

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

test = readInput <$> readFile exampleFile

solution1 :: String -> String
solution1 = show . fromTo "you" "out" . readInput

solution2 :: String -> String
solution2 = const "Not implemented"

main :: IO ()
main = do
  putStrLn "Task 11"

  example <- readFile exampleFile
  input <- readFile inputFile

  putStrLn "solution1:"
  putStrLn $ "example: " <> solution1 example
  putStrLn $ "input: " <> solution1 input

  putStrLn "solution2:"
  putStrLn $ "example: " <> solution2 example
  putStrLn $ "input: " <> solution2 input
