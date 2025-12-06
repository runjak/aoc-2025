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
parseOperator "+" = Just Plus
parseOperator "*" = Just Times
parseOperator _ = Nothing

type Column = ([Z], Operator)

solveColumn :: Column -> Z
solveColumn (ns, Plus) = sum ns
solveColumn (ns, Times) = product ns

type Homework = [Column]

parseHomework :: String -> Homework
parseHomework input = do
  let fields = map words $ lines input
      numbers = transpose . map (mapMaybe readMaybe) $ init fields
      operators = mapMaybe parseOperator $ last fields
  zip numbers operators

solution1 :: String -> String
solution1 = show . sum . map solveColumn . parseHomework

solution2 :: String -> String
solution2 = const "not implemented"

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
