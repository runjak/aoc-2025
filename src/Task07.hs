module Main where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)

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

startCounts :: String -> [Int]
startCounts = map (\c -> if c == 'S' then 1 else 0)

picks :: [Int] -> String -> Int
picks counts = sum . zipWith (\c s -> if s == '|' then c else 0) counts

countfront :: [Int] -> String -> String -> [Int]
countfront [] _ _ = []
countfront _ [] _ = []
countfront _ _ [] = []
countfront counts line1 line2
  | isPrefixOf "|." line1 && isPrefixOf "|^" line2 = [picks counts "|."] <> countfront (drop 1 counts) (drop 1 line1) (drop 1 line2)
  | isPrefixOf ".|" line1 && isPrefixOf "|^" line2 = [picks counts ".|"] <> countfront (drop 1 counts) (drop 1 line1) (drop 1 line2)
  | isPrefixOf "||" line1 && isPrefixOf "|^" line2 = [picks counts "||"] <> countfront (drop 1 counts) (drop 1 line1) (drop 1 line2)
  | isPrefixOf "|.." line1 && isPrefixOf "^|^" line2 = [0, picks counts "|.."] <> countfront (drop 2 counts) (drop 2 line1) (drop 2 line2)
  | isPrefixOf ".|." line1 && isPrefixOf "^|^" line2 = [0, picks counts ".|."] <> countfront (drop 2 counts) (drop 2 line1) (drop 2 line2)
  | isPrefixOf "..|" line1 && isPrefixOf "^|^" line2 = [0, picks counts "..|"] <> countfront (drop 2 counts) (drop 2 line1) (drop 2 line2)
  | isPrefixOf "||." line1 && isPrefixOf "^|^" line2 = [0, picks counts "||."] <> countfront (drop 2 counts) (drop 2 line1) (drop 2 line2)
  | isPrefixOf "|.|" line1 && isPrefixOf "^|^" line2 = [0, picks counts "|.|"] <> countfront (drop 2 counts) (drop 2 line1) (drop 2 line2)
  | isPrefixOf ".||" line1 && isPrefixOf "^|^" line2 = [0, picks counts ".||"] <> countfront (drop 2 counts) (drop 2 line1) (drop 2 line2)
  | isPrefixOf "|||" line1 && isPrefixOf "^|^" line2 = [0, picks counts "|||"] <> countfront (drop 2 counts) (drop 2 line1) (drop 2 line2)
  | isPrefixOf "|." line1 && isPrefixOf "^|" line2 = [0, picks counts "|."] <> countfront (drop 2 counts) (drop 2 line1) (drop 2 line2)
  | isPrefixOf ".|" line1 && isPrefixOf "^|" line2 = [0, picks counts ".|"] <> countfront (drop 2 counts) (drop 2 line1) (drop 2 line2)
  | isPrefixOf "||" line1 && isPrefixOf "^|" line2 = [0, picks counts "||"] <> countfront (drop 2 counts) (drop 2 line1) (drop 2 line2)
  | "|" `isPrefixOf` line2 = take 1 counts <> countfront (drop 1 counts) (drop 1 line1) (drop 1 line2)
  | otherwise = [0] <> countfront (drop 1 counts) (drop 1 line1) (drop 1 line2)

propagateCounts :: [String] -> [Int]
propagateCounts [] = []
propagateCounts field = do
  let counts = startCounts . fromMaybe "" $ listToMaybe field
  go counts $ drop 1 field
  where
    go :: [Int] -> [String] -> [Int]
    go counts (a : b : cs) = go (countfront counts a b) (b : cs)
    go counts _ = counts

test = propagate . lines <$> readFile exampleFile

solution2 :: String -> String
solution2 = show . sum . propagateCounts . propagate . lines

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
