module Main where

import Control.Monad (guard)
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Distribution.Compat.Prelude (readMaybe)

exampleFile = "./inputs/12/example.txt"

inputFile = "./inputs/12/input.txt"

type Position = (Int, Int)

type Shape = Set Position

readShape :: String -> Shape
readShape input = Set.fromList [(x, y) | (y, line) <- zip [0 ..] (lines input), (x, f) <- zip [0 ..] line, f == '#']

readShapeWithIndex :: String -> Maybe Shape
readShapeWithIndex input = do
  let parts = lines input
  head <- listToMaybe parts
  guard $ List.isSuffixOf ":" head
  return $ readShape $ unlines $ drop 1 parts

type Dimension = (Int, Int)
type Region = (Dimension, [Int])

readDimension :: String -> Maybe Dimension
readDimension input = case Split.splitOn "x" input of
  [a,b] -> (,) <$> readMaybe a <*> readMaybe b
  _ -> Nothing

readRegion :: String -> Maybe Region
readRegion input = do
  case Split.splitOn ": " input of
    [a,b] -> (,) <$> readDimension a <*> (Just $ mapMaybe readMaybe $ words b)
    _ -> Nothing

{-
Concept:
Parse inputs
Create classes of shapes
Instance problems
Solve problems
Success
-}

solution1 :: String -> String
solution1 = const "Not implemented"

solution2 :: String -> String
solution2 = const "Not implemented"

main :: IO ()
main = do
  putStrLn "Task 12"

  example <- readFile exampleFile
  input <- readFile inputFile

  putStrLn "solution1:"
  putStrLn $ "example: " <> solution1 example
  putStrLn $ "input: " <> solution1 input

  putStrLn "solution2:"
  putStrLn $ "example: " <> solution2 example
  putStrLn $ "input: " <> solution2 input
