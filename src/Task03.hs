module Main where

import Distribution.Compat.Prelude (mapMaybe, readMaybe)

exampleFile = "./inputs/03/example.txt"

inputFile = "./inputs/03/input.txt"

type Z = Int

parseInput :: String -> [[Z]]
parseInput = map (mapMaybe (\digit -> readMaybe [digit])) . lines

solution1 :: String -> String
solution1 = show . sum . map (fst . maxJoltage 2) . parseInput

findBest :: [Z] -> (Z, [Z])
findBest bank = do
  let best = maximum bank
      rest = drop 1 . dropWhile (/= best) $ bank
  (best, rest)

{-
  depth: how many batteries to switch on
  bank: batteries to choose from
  -> (maximum joltage, remaining bank)
-}
maxJoltage :: Z -> [Z] -> (Z, [Z])
maxJoltage depth bank
  | depth <= 1 = findBest bank
  | otherwise = do
      let (j1, currentBank') = maxJoltage (depth - 1) $ init bank
          (j2, nextBank) = findBest $ currentBank' <> [last bank]
      (j1 * 10 + j2, nextBank)

solution2 :: String -> String
solution2 = show . sum . map (fst . maxJoltage 12) . parseInput

main :: IO ()
main = do
  putStrLn "Task 03"

  example <- readFile exampleFile
  input <- readFile inputFile

  putStrLn "solution1:"
  putStrLn $ "example: " <> solution1 example
  putStrLn $ "input: " <> solution1 input

  putStrLn "solution2:"
  putStrLn $ "example: " <> solution2 example
  putStrLn $ "input: " <> solution2 input
