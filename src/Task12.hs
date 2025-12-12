module Main where

import Control.Monad (guard, forM_)
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Distribution.Compat.Prelude (readMaybe)

exampleFile = "./inputs/12/example.txt"

inputFile = "./inputs/12/input.txt"

type Position = (Int, Int)

type Shape = Set Position

readShape :: String -> Shape
readShape input = Set.fromList [(x, y) | (y, line) <- zip [0 ..] (lines input), (x, f) <- zip [0 ..] line, f == '#']

readShapeWithIndex :: String -> Maybe Shape
readShapeWithIndex input = do
  let parts = lines input
  head <- listToMaybe parts
  guard $ List.isSuffixOf ":" head
  return $ readShape $ unlines $ drop 1 parts

type Dimension = (Int, Int)
type Region = (Dimension, [Int])

readDimension :: String -> Maybe Dimension
readDimension input = case Split.splitOn "x" input of
  [a,b] -> (,) <$> readMaybe a <*> readMaybe b
  _ -> Nothing

readRegion :: String -> Maybe Region
readRegion input =
  case Split.splitOn ": " input of
    [a,b] -> (,) <$> readDimension a <*> (Just $ mapMaybe readMaybe $ words b)
    _ -> Nothing

readInput :: String -> ([Shape], [Region])
readInput input =
  let chunks = Split.splitOn "\n\n" input
      shapes = mapMaybe readShapeWithIndex $ init chunks
      regions = mapMaybe readRegion $ lines $ last chunks
  in (shapes, regions)

test = readInput <$> readFile exampleFile

mirrorX :: Shape -> Shape
mirrorX = Set.map (\(x, y) -> (2 - x, y))

rotate90 :: Shape -> Shape
rotate90 = Set.map λ
  where
    {-
    00 01 02    02 12 22
    10 11 12 -> 01 11 21
    20 21 22    00 10 20
    -}
    λ (0, 0) = (0, 2)
    λ (0, 1) = (1, 2)
    λ (0, 2) = (2, 2)
    λ (1, 0) = (0, 1)
    λ (1, 1) = (1, 1)
    λ (1, 2) = (2, 1)
    λ (2, 0) = (0, 0)
    λ (2, 1) = (1, 0)
    λ (2, 2) = (2, 0)

variants :: Shape -> Set Shape
variants shape = Set.fromList $ map ($ shape) [
    id
  , rotate90
  , rotate90 . rotate90
  , rotate90 . rotate90 . rotate90
  , mirrorX
  , rotate90 .  mirrorX
  , rotate90 . rotate90 . mirrorX
  , rotate90 . rotate90 . rotate90 . mirrorX
  ]

showShape :: Shape -> String
showShape shape = unlines [[if Set.member (x,y) shape then '#' else '.'|x<-[0..2]]|y<-[0..2]]

testShapeVariants :: IO ()
testShapeVariants = do
  let example = unlines ["###", "..#", "..."]
  putStrLn $ "Variants of shape:\n" <> example <> "-----"
  forM_ (Set.elems $ variants $ readShape example) $ \s -> putStrLn $ showShape s

-- all variants and placements of a shape in an area of the given dimension
shapePlacements :: Dimension -> Shape -> Set Shape
shapePlacements (xMax, yMax) shape =
  let vs = variants shape
      candidates = [Set.map (\(x, y) -> (x + deltaX, y + deltaY)) v|deltaX <- [0..xMax], deltaY <- [0..yMax], v <- Set.elems vs ]
  in Set.fromList $ filter (all (\(x, y) -> x < xMax && y < yMax) . Set.elems) candidates

{-
  Given a list of shapes and a region:
  This function produces a list of sets of shapes.
  One shape from each set MUST be picked without collisions in order for the shapes to be placeable without collisions.
-}
placementProblem :: [Shape] -> Region -> [Set Shape]
placementProblem shapes (dimension, shapeCounts) =
  concat $ zipWith (\shape count -> replicate count $ shapePlacements dimension shape) shapes shapeCounts

{-
  Instance problems
  Solve problems
  Success
-}

solution1 :: String -> String
solution1 = const "Not implemented"

solution2 :: String -> String
solution2 = const "Not implemented"

main :: IO ()
main = do
  putStrLn "Task 12"

  example <- readFile exampleFile
  input <- readFile inputFile

  putStrLn "solution1:"
  putStrLn $ "example: " <> solution1 example
  putStrLn $ "input: " <> solution1 input

  putStrLn "solution2:"
  putStrLn $ "example: " <> solution2 example
  putStrLn $ "input: " <> solution2 input
