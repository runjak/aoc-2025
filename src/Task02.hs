module Main where

import Control.Monad (liftM2)
import Distribution.Compat.Prelude (mapMaybe, readMaybe)

exampleFile = "./inputs/02/example.txt"

inputFile = "./inputs/02/input.txt"

(?:) :: Bool -> (a, a) -> a
True ?: (a, _) = a
False ?: (_, a) = a

type Z = Int

parseInput :: String -> [(Z, Z)]
parseInput = mapMaybe parseRange . splitRanges
  where
    splitRanges :: String -> [String]
    splitRanges = words . map (\c -> (c == ',') ?: (' ', c))

    parseRange :: String -> Maybe (Z, Z)
    parseRange = λ . words . map (\c -> (c == '-') ?: (' ', c))
      where
        λ :: [String] -> Maybe (Z, Z)
        λ [from, to] = liftM2 (,) (readMaybe from) (readMaybe to)
        λ _ = Nothing

expandRange :: (Z, Z) -> [Z]
expandRange (from, to) = [from .. to]

countDigits :: Z -> Z
countDigits = (+ 1) . floor . logBase 10 . fromIntegral

isInvalidId :: Z -> Bool
isInvalidId x = do
  let digits = countDigits x
      isEven = even digits
      actual = show x
      firstHalf = take (digits `div` 2) actual
      expected = firstHalf <> firstHalf
  isEven ?: (actual == expected, False)

solution1 :: String -> String
solution1 input = show . sum . concatMap (filter isInvalidId . expandRange) $ parseInput input

main :: IO ()
main = do
  putStrLn "Task 02"

  example <- readFile exampleFile
  input <- readFile inputFile

  putStrLn "solution1:"
  putStrLn $ "example: " <> solution1 example
  putStrLn $ "input: " <> solution1 input
