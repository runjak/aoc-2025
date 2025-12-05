{- HLINT ignore "Use tuple-section" -}
module Main where

import Data.Set (Set)
import Data.Set qualified as Set
import Distribution.Compat.Prelude (liftM2, mapMaybe, readMaybe)

exampleFile = "./inputs/05/example.txt"

inputFile = "./inputs/05/input.txt"

type Z = Integer

type Range = (Z, Z)

inRange :: Z -> Range -> Bool
inRange x (a, b) = x >= a && x <= b

parseRange :: String -> Maybe Range
parseRange input =
  let (from, to') = span (/= '-') input
   in liftM2 (,) (readMaybe from) (readMaybe $ drop 1 to')

parseInput :: String -> (Set Range, [Z])
parseInput input = do
  let (ranges', rest) = span (/= "") $ lines input
      ranges = Set.fromList $ mapMaybe parseRange ranges'
  (ranges, mapMaybe readMaybe rest)

isFresh :: Set Range -> Z -> Bool
isFresh ranges i =
  let wanted = fst $ Set.spanAntitone ((<= i) . fst) ranges
   in any (inRange i) $ Set.elems wanted

solution1 :: String -> String
solution1 = show . length . uncurry λ . parseInput
  where
    λ :: Set Range -> [Z] -> [Z]
    λ ranges = filter (isFresh ranges)

rangeLength :: Range -> Z
rangeLength (a, b) = b - a + 1

sanitizeRanges :: Set Range -> Set Range
sanitizeRanges ranges = undefined

nonOverlapping :: Set Range -> [Range]
nonOverlapping = go . Set.elems
  where
    go :: [Range] -> [Range]
    go [] = []
    go (r : rs) = do
      let rEnd = snd r
          (overlapping, others) = span ((<= rEnd) . fst) rs
          fixed = filter (uncurry (<)) $ map ((\e -> (rEnd + 1, e)) . snd) overlapping
      r : go (fixed <> others)

solution2 :: String -> String
solution2 = show . sum . map rangeLength . nonOverlapping . fst . parseInput

main :: IO ()
main = do
  putStrLn "Task 05"

  example <- readFile exampleFile
  input <- readFile inputFile

  putStrLn "solution1:"
  putStrLn $ "example: " <> solution1 example
  putStrLn $ "input: " <> solution1 input

  putStrLn "solution2:"
  putStrLn $ "example: " <> solution2 example
  putStrLn $ "input: " <> solution2 input
