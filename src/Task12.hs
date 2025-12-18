module Main where

import Control.Monad (forM, forM_, guard, filterM, liftM)
import Data.Bifunctor (first)
import Data.List qualified as List
import Data.List.Split qualified as Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Distribution.Compat.Prelude (readMaybe)
import MiniSat (Lit, Solver)
import qualified MiniSat

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
  [a, b] -> (,) <$> readMaybe a <*> readMaybe b
  _ -> Nothing

readRegion :: String -> Maybe Region
readRegion input =
  case Split.splitOn ": " input of
    [a, b] -> (,) <$> readDimension a <*> Just (mapMaybe readMaybe $ words b)
    _ -> Nothing

type Input = ([Shape], [Region])

readInput :: String -> Input
readInput input =
  let chunks = Split.splitOn "\n\n" input
      shapes = mapMaybe readShapeWithIndex $ init chunks
      regions = mapMaybe readRegion $ lines $ last chunks
   in (shapes, regions)

mirrorX :: Shape -> Shape
mirrorX = Set.map (first (2 -))

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
variants shape =
  Set.fromList $
    map
      ($ shape)
      [ id,
        rotate90,
        rotate90 . rotate90,
        rotate90 . rotate90 . rotate90,
        mirrorX,
        rotate90 . mirrorX,
        rotate90 . rotate90 . mirrorX,
        rotate90 . rotate90 . rotate90 . mirrorX
      ]

showShape :: Shape -> String
showShape shape = unlines [[if Set.member (x, y) shape then '#' else '.' | x <- [0 .. 2]] | y <- [0 .. 2]]

testShapeVariants :: IO ()
testShapeVariants = do
  let example = unlines ["###", "..#", "..."]
  putStrLn $ "Variants of shape:\n" <> example <> "-----"
  forM_ (Set.elems $ variants $ readShape example) $ \s -> putStrLn $ showShape s

-- all variants and placements of a shape in an area of the given dimension
shapePlacements :: Dimension -> Shape -> Set Shape
shapePlacements (xMax, yMax) shape =
  let vs = variants shape
      candidates = [Set.map (\(x, y) -> (x + deltaX, y + deltaY)) v | deltaX <- [0 .. xMax], deltaY <- [0 .. yMax], v <- Set.elems vs]
   in Set.fromList $ filter (all (\(x, y) -> x < xMax && y < yMax) . Set.elems) candidates

{-
A problem is a list of sets of shapes.
For every set we need to place exactly one set.
If this works all gifts can be placed.
-}
type Problem = [Set Shape]

{-
  Given a list of shapes and a region:
  This function produces a list of sets of shapes.
  One shape from each set MUST be picked without collisions in order for the shapes to be placeable without collisions.
-}
placementProblem :: [Shape] -> Region -> Problem
placementProblem shapes (dimension, shapeCounts) =
  concat $ zipWith (\shape count -> replicate count $ shapePlacements dimension shape) shapes shapeCounts

{-
An enumerated problem is one
where the alternative shapes of a Problem are no longer a set,
but we've instead got a list of tuples of indices and shapes.

The indices are to be used as variables indicating which shape was chosen.

We can then reformulate our problem as linear equations
by specifying that exactly one variable of the inner lists is 1,
and by adding constraints that ensure to not pick colliding variables for other shapes.
-}
type EnumeratedProblem = [[(Lit, Shape)]]

enumerateProblem :: Solver -> Problem -> IO EnumeratedProblem
enumerateProblem solver problem =
  forM problem $ \stack ->
    forM (Set.elems stack) $ \shape ->
      (,shape) <$> MiniSat.newLit solver

collisionsPerPosition :: EnumeratedProblem -> Map Position (Set Lit)
collisionsPerPosition = Map.unionsWith Set.union . map perStack
  where
    perStack :: [(Lit, Shape)] -> Map Position (Set Lit)
    perStack = Map.unionsWith Set.union . map perEntry

    perEntry :: (Lit, Shape) -> Map Position (Set Lit)
    perEntry (i, ps) = Map.fromListWith Set.union [(p, Set.singleton i)|p <- Set.elems ps]

atMostOne :: [Lit] -> [[Lit]]
atMostOne = pairs . map MiniSat.neg
  where
    pairs :: [Lit] -> [[Lit]]
    pairs [] = []
    pairs (a:as) = [[a,b]|b <- as] <> pairs as

exactlyOne :: [Lit] -> [[Lit]]
exactlyOne as = as : atMostOne as

encodeProblem :: Problem -> IO Solver
encodeProblem problem = do
  solver <- MiniSat.newSolver
  eProblem <- enumerateProblem solver problem

  -- We choose exactly one position for each shape:
  mapM_ (MiniSat.addClause solver) $ concatMap (exactlyOne . map fst) eProblem

  -- Shapes must not collide:
  let collisionGroups = map Set.elems . Map.elems $ collisionsPerPosition eProblem 
  mapM_ (MiniSat.addClause solver) $ concatMap atMostOne collisionGroups

  return solver

inputToProblems :: Input -> [Problem]
inputToProblems (shapes, regions) = map (placementProblem shapes) regions

countSatisfiable :: [Problem] -> IO Int
countSatisfiable = liftM length . filterM λ . zip [1..]
  where
    λ :: (Int, Problem) -> IO Bool
    λ (i, problem) = do
      putStrLn $ "Problem " <> show i
      s <- encodeProblem problem
      MiniSat.simplify s
      MiniSat.solve s []

possibleRegions :: Input -> Input
possibleRegions (shapes, regions) =
  (shapes,) $ filter (\r -> regionSize r >= sizeRequirements shapes (snd r)) regions
  where
    regionSize :: Region -> Int
    regionSize = uncurry (*) . fst

    sizeRequirements :: [Shape] -> [Int] -> Int
    sizeRequirements shapes counts = sum $ zipWith (\shape count -> Set.size shape * count) shapes counts

solution1 :: String -> IO String
solution1 = liftM show . countSatisfiable . inputToProblems . possibleRegions . readInput

solution1' :: String -> IO String
solution1' = return . show . length . snd . possibleRegions . readInput

solution2 :: String -> String
solution2 = const "Not implemented"

main :: IO ()
main = do
  putStrLn "Task 12"

  example <- readFile exampleFile
  input <- readFile inputFile

  putStrLn "solution1:"
  s1e <- solution1' example
  putStrLn $ "example: " <> s1e
  s1i <- solution1' input
  putStrLn $ "input: " <> s1i

  putStrLn "solution2:"
  putStrLn $ "example: " <> solution2 example
  putStrLn $ "input: " <> solution2 input
