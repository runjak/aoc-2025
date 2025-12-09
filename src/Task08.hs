module Main where

import Data.List (partition, sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Distribution.Compat.Prelude (readMaybe)

exampleFile = "./inputs/08/example.txt"

inputFile = "./inputs/08/input.txt"

type Vec = [Int]

replace :: Char -> Char -> String -> String
replace x y = map (\c -> if c == x then y else c)

readInput :: String -> [Vec]
readInput = map (mapMaybe readMaybe . words . replace ',' ' ') . lines

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs <> pairs xs

squareDistance :: Vec -> Vec -> Int
squareDistance a b = sum $ zipWith (\x y -> (^ 2) $ x - y) a b

sortedPairs :: [Vec] -> [(Vec, Vec)]
sortedPairs = sortOn (uncurry squareDistance) . pairs

type Circuit = Set Vec

connect :: [Circuit] -> (Vec, Vec) -> [Circuit]
connect circuits (v1, v2) =
  let (affected, rest) = partition (\s -> Set.member v1 s || Set.member v2 s) circuits
   in Set.unions (Set.fromList [v1, v2] : affected) : rest

solve1 :: Int -> [Vec] -> Int
solve1 n = product . take 3 . sortOn negate . map Set.size . foldl connect [] . take n . sortedPairs

solution1 :: Int -> String -> String
solution1 n = show . solve1 n . readInput

-- connect, but keeping the last connected pair
connectLast :: ((Vec, Vec), [Circuit]) -> (Vec, Vec) -> ((Vec, Vec), [Circuit])
connectLast last@(_, circuits) (v1, v2) = do
  let (affected, rest) = partition (\s -> Set.member v1 s || Set.member v2 s) circuits
      unchanged = not (null affected) && all (\s -> Set.member v1 s && Set.member v2 s) affected
      next = ((v1, v2), Set.unions (Set.fromList [v1, v2] : affected) : rest)
  if unchanged then last else next

solve2 :: [Vec] -> Int
solve2 boxes =
  let (v1, v2) = fst . foldl connectLast (([], []), []) $ sortedPairs boxes
   in fromMaybe 0 . listToMaybe $ zipWith (*) v1 v2

solution2 :: String -> String
solution2 = show . solve2 . readInput

main :: IO ()
main = do
  putStrLn "Task 08"

  example <- readFile exampleFile
  input <- readFile inputFile

  putStrLn "solution1:"
  putStrLn $ "example: " <> solution1 10 example
  putStrLn $ "input: " <> solution1 1000 input

  putStrLn "solution2:"
  putStrLn $ "example: " <> solution2 example
  putStrLn $ "input: " <> solution2 input
