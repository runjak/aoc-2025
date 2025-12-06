module Main where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Distribution.Compat.Prelude (fromMaybe, mapMaybe, readMaybe)

exampleFile = "./inputs/04/example.txt"

inputFile = "./inputs/04/input.txt"

type Z = Int

type Position = (Z, Z)

data Field = Paper | Empty
  deriving (Show, Eq, Ord)

parseField :: Char -> Maybe Field
parseField '@' = Just Paper
parseField '.' = Just Empty
parseField _ = Nothing

type Department = Map Position Field

parseInput :: String -> Department
parseInput = Map.fromList . concat . zipWith go [0 ..] . lines
  where
    go :: Z -> String -> [((Z, Z), Field)]
    go y = zipWith (\x f -> ((x, y), f)) [0 ..] . mapMaybe parseField

adjacent :: Position -> [Position]
adjacent (x, y) =
  [(x', y') | x' <- [x - 1, x, x + 1], y' <- [y - 1, y, y + 1], (x', y') /= (x, y)]

hasPaper :: Department -> Position -> Bool
hasPaper department = (== Paper) . fromMaybe Empty . flip Map.lookup department

adjacentPaper :: Department -> Position -> [Position]
adjacentPaper department = filter (hasPaper department) . adjacent

accessiblePaper :: Department -> [Position]
accessiblePaper department = do
  let paperPositions = filter (hasPaper department) $ Map.keys department
  filter ((< 4) . length . adjacentPaper department) paperPositions

solution1 :: String -> String
solution1 = show . length . accessiblePaper . dropEmpty . parseInput

dropEmpty :: Department -> Department
dropEmpty = Map.filter (/= Empty)

dropPositions :: Department -> [Position] -> Department
dropPositions department = Map.withoutKeys department . Set.fromList

-- TODO use iterate?
transitiveAccessiblePaper :: Department -> [Position]
transitiveAccessiblePaper department = do
  let paperPositions = accessiblePaper department
      pCount = length paperPositions
      next = (if null paperPositions then [] else transitiveAccessiblePaper $ dropPositions department paperPositions)
  paperPositions <> next

solution2 :: String -> String
solution2 = show . length . transitiveAccessiblePaper . dropEmpty . parseInput

main :: IO ()
main = do
  putStrLn "Task 04"

  example <- readFile exampleFile
  input <- readFile inputFile

  putStrLn "solution1:"
  putStrLn $ "example: " <> solution1 example
  putStrLn $ "input: " <> solution1 input

  putStrLn "solution2:"
  putStrLn $ "example: " <> solution2 example
  putStrLn $ "input: " <> solution2 input
