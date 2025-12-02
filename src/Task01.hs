module Main where

import Data.Maybe (mapMaybe)
import Distribution.Compat.Prelude (readMaybe)

data Rotation = L Int | R Int
  deriving (Show)

parseInput :: String -> [Rotation]
parseInput = mapMaybe go . lines
  where
    go :: String -> Maybe Rotation
    go ('L' : ds) = L <$> readMaybe ds
    go ('R' : ds) = R <$> readMaybe ds
    go _ = Nothing

rotate :: Int -> Rotation -> Int
rotate x (L y) = rotate x (R (-y))
rotate x (R y) = go $ x + y
  where
    go n
      | n < 0 = go $ n + 100
      | otherwise = n `mod` 100

rotationHistory :: Int -> [Rotation] -> [Int]
rotationHistory x (r : rs) = let x' = rotate x r in x' : rotationHistory x' rs
rotationHistory _ [] = []

solution1 :: Int -> String -> Int
solution1 start = length . filter (0 ==) . rotationHistory start . parseInput

rotateHell :: [Rotation] -> [Rotation]
rotateHell = concatMap go
  where
    go :: Rotation -> [Rotation]
    go (L n) = replicate n (L 1)
    go (R n) = replicate n (R 1)

solution2 :: Int -> String -> Int
solution2 start = length . filter (0 ==) . rotationHistory start . rotateHell . parseInput

main :: IO ()
main = do
  putStrLn "Task 01"

  example <- readFile "./inputs/01/example.txt"
  input <- readFile "./inputs/01/input.txt"

  putStrLn "solution1:"
  putStrLn $ "example: " <> show (solution1 50 example)
  putStrLn $ "input: " <> show (solution1 50 input)

  putStrLn "solution2:"
  putStrLn $ "example: " <> show (solution2 50 example)
  putStrLn $ "input: " <> show (solution2 50 input)
