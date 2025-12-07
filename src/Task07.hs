module Main where

import Data.List (isPrefixOf)

exampleFile = "./inputs/07/example.txt"

inputFile = "./inputs/07/input.txt"

wavefront :: String -> String -> String
wavefront [] _ = []
wavefront _ [] = []
wavefront a b
  | isPrefixOf "S" a && isPrefixOf "." b = "|" <> wavefront (drop 1 a) (drop 1 b)
  | isPrefixOf "|" a && isPrefixOf "." b = "|" <> wavefront (drop 1 a) (drop 1 b)
  | isPrefixOf ".|" a && isPrefixOf ".^" b = "|" <> wavefront (drop 1 a) (drop 1 b)
  | isPrefixOf "|." a && isPrefixOf "^." b = "^|" <> wavefront (drop 2 a) (drop 2 b)
  | otherwise = take 1 b <> wavefront (drop 1 a) (drop 1 b)

propagate :: [String] -> [String]
propagate (a : b' : xs) =
  let b = wavefront a b'
   in a : propagate (b : xs)
propagate xs = xs

countSplits :: [String] -> Int
countSplits [] = 0
countSplits [_] = 0
countSplits (a : b : cs) =
  let splits = length . filter (\(a, b) -> a == '|' && b == '^') $ zip a b
   in splits + countSplits (b : cs)

solution1 :: String -> String
solution1 = show . countSplits . propagate . lines

solution2 :: String -> String
solution2 = const "Not implemented"

main :: IO ()
main = do
  putStrLn "Task 07"

  example <- readFile exampleFile
  input <- readFile inputFile

  putStrLn "solution1:"
  putStrLn $ "example: " <> solution1 example
  putStrLn $ "input: " <> solution1 input

  putStrLn "solution2:"
  putStrLn $ "example: " <> solution2 example
  putStrLn $ "input: " <> solution2 input
