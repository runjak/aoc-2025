module Main where

import Data.List (tails)
import Distribution.Compat.Prelude (mapMaybe, readMaybe)

exampleFile = "./inputs/03/example.txt"

inputFile = "./inputs/03/input.txt"

type Z = Int

parseInput :: String -> [[Z]]
parseInput = map (mapMaybe (\digit -> readMaybe [digit])) . lines

maxJoltage :: [Z] -> Z
maxJoltage bank = do
  let maxFirst = maximum $ init bank
      maxSecond = maximum . drop 1 $ dropWhile (/= maxFirst) bank
  maxFirst * 10 + maxSecond

solution1 :: String -> String
solution1 = show . sum . map maxJoltage . parseInput

main :: IO ()
main = do
  putStrLn "Task 03"

  example <- readFile exampleFile
  input <- readFile inputFile

  putStrLn "solution1:"
  putStrLn $ "example: " <> solution1 example
  putStrLn $ "input: " <> solution1 input
