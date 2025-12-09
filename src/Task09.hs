module Main where

import Data.Maybe (mapMaybe)
import Distribution.Compat.Prelude (readMaybe)

exampleFile = "./inputs/09/example.txt"

inputFile = "./inputs/09/input.txt"

type Vec = [Int]

readInput :: String -> [Vec]
readInput = mapMaybe (readMaybe . (\line -> "[" <> line <> "]")) . lines

delta :: Int -> Int -> Int
delta x y = (+ 1) $ abs (x - y)

area :: Vec -> Vec -> Int
area v1 v2 = product $ zipWith delta v1 v2

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs <> pairs xs

solution1 :: String -> String
solution1 = show . maximum . map (uncurry area) . pairs . readInput

solution2 :: String -> String
solution2 = const "Not implemented"

main :: IO ()
main = do
  putStrLn "Task 09"

  example <- readFile exampleFile
  input <- readFile inputFile

  putStrLn "solution1:"
  putStrLn $ "example: " <> solution1 example
  putStrLn $ "input: " <> solution1 input

  putStrLn "solution2:"
  putStrLn $ "example: " <> solution2 example
  putStrLn $ "input: " <> solution2 input
