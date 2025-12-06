module Main where

import Data.List (transpose)
import Data.Maybe (mapMaybe)
import Distribution.Compat.Prelude (readMaybe)

exampleFile = "./inputs/06/example.txt"

inputFile = "./inputs/06/input.txt"

type Z = Integer

data Operator = Plus | Times
  deriving (Show)

parseOperator :: String -> Maybe Operator
parseOperator ('+' : _) = Just Plus
parseOperator ('*' : _) = Just Times
parseOperator _ = Nothing

type Column = ([Z], Operator)

solveColumn :: Column -> Z
solveColumn (ns, Plus) = sum ns
solveColumn (ns, Times) = product ns

type Homework = [Column]

parseHomework1 :: String -> Homework
parseHomework1 input = do
  let fields = map words $ lines input
      numbers = transpose . map (mapMaybe readMaybe) $ init fields
      operators = mapMaybe parseOperator $ last fields
  zip numbers operators

solution1 :: String -> String
solution1 = show . sum . map solveColumn . parseHomework1

-- TODO write with tranpose?
separateColumns :: String -> [[String]]
separateColumns input = do
  let ls = lines input
      ns = init ls
      os = last ls

      go :: String -> [String] -> [[String]]
      go "" _ = []
      go os ns = do
        let prefix = take 1 os <> takeWhile (== ' ') (drop 1 os)
            pL = length prefix
            (n, ns') = unzip $ map (splitAt pL) ns
            (o, os') = splitAt pL os
        (n <> [o]) : go os' ns'

  go os ns

parseColumn :: [String] -> Maybe Column
parseColumn column = do
  o <- parseOperator $ last column
  let ns = mapMaybe readMaybe $ transpose $ init column
  return (ns, o)

parseHomework2 :: String -> Homework
parseHomework2 = mapMaybe parseColumn . separateColumns

solution2 :: String -> String
solution2 = show . sum . map solveColumn . parseHomework2

main :: IO ()
main = do
  putStrLn "Task 06"

  example <- readFile exampleFile
  input <- readFile inputFile

  putStrLn "solution1:"
  putStrLn $ "example: " <> solution1 example
  putStrLn $ "input: " <> solution1 input

  putStrLn "solution2:"
  putStrLn $ "example: " <> solution2 example
  putStrLn $ "input: " <> solution2 input
