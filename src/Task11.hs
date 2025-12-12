module Main where

import Data.List (partition)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Numeric.LinearAlgebra ((<#), (!))
import qualified Numeric.LinearAlgebra as LA

exampleFile1 = "./inputs/11/example1.txt"
exampleFile2 = "./inputs/11/example2.txt"

inputFile = "./inputs/11/input.txt"

type ReactorGraph = Map String [String]

readInput :: String -> ReactorGraph
readInput = Map.fromList . mapMaybe (go . words) . lines
  where
    go :: [String] -> Maybe (String, [String])
    go (label : rest) = Just (init label, rest)
    go [] = Nothing

next :: String -> ReactorGraph -> [String]
next from reactor = fromMaybe [] $ Map.lookup from reactor

fromTo :: String -> String -> ReactorGraph -> Int
fromTo from to reactor =
  let (arrived, rest) = partition (== to) $ next from reactor
   in sum $ (length arrived :) $ map (\f -> fromTo f to reactor) rest

solution1 :: String -> String
solution1 = show . fromTo "you" "out" . readInput

type Vector = LA.Vector LA.R

type Labeling = Map String Int

type Matrix = LA.Matrix LA.R

indexToVector :: Int -> Int -> Vector
indexToVector size i = LA.vector $ take size [if x == i then 1 else 0|x <- [0..]]

reactorToMatrix :: ReactorGraph -> (Labeling, Matrix)
reactorToMatrix reactor' =
  let reactor = Map.union reactor' $ Map.singleton "out" []
      labels = Map.fromList $ zip (Map.keys reactor) [0..]
      size = Map.size labels
      matrix = LA.fromRows . map (sum . map (indexToVector size . fromMaybe (-1) . (Map.lookup `flip` labels))) $ Map.elems reactor
   in (labels, matrix)

square :: Matrix -> Matrix
square m = m <> m

power :: Matrix -> Int -> Matrix
power m 0 = LA.ident $ LA.rows m
power m n = foldl (<>) m $ replicate (n-1) m

powerBy :: (Int -> Matrix) -> Int -> Matrix -> Matrix
powerBy lookup 0 m = LA.ident $ LA.rows m
powerBy lookup 1 m = m
powerBy lookup n m
  | even n = let m' = lookup (n `div` 2) in m' <> m'
  | otherwise = m <> lookup (n - 1)

powerMemo :: (Int, Int) -> Matrix -> Int -> Matrix
powerMemo (from, to) m i =
  let memo = Map.fromList $ map (\k -> (k, powerBy (\k' -> fromJust $ Map.lookup k' memo) k m)) [from..to] 
   in fromJust $ Map.lookup i memo

mFromTo' :: Matrix -> Int -> Int -> Int
mFromTo' matrix from to =
  let fromVector = indexToVector (LA.rows matrix) from
      toVector = fromVector <# matrix
   in round $ toVector ! to

mFromTo :: Matrix -> Labeling -> String -> String -> Maybe Int
mFromTo matrix labeling from to = mFromTo' matrix <$> Map.lookup from labeling <*> Map.lookup to labeling

solve2 :: ReactorGraph -> Int
solve2 reactor = do
  let size = Map.size reactor
      (labeling, matrix) = reactorToMatrix reactor
      pm = powerMemo (0, size) matrix
      m = sum $ map pm [1..size]
      aToB = \ a b -> fromJust $ mFromTo m labeling a b
  sum [
    aToB "svr" "dac" * aToB "dac" "fft" * aToB "fft" "out",
    aToB "svr" "fft" * aToB "fft" "dac" * aToB "dac" "out"
    ]

solution2 :: String -> String
solution2 = show . solve2 . readInput

main :: IO ()
main = do
  putStrLn "Task 11"

  example1 <- readFile exampleFile1
  example2 <- readFile exampleFile2
  input <- readFile inputFile

  putStrLn "solution1:"
  putStrLn $ "example: " <> solution1 example1
  putStrLn $ "input: " <> solution1 input

  putStrLn "solution2:"
  putStrLn $ "example: " <> solution2 example2
  putStrLn $ "input: " <> solution2 input
