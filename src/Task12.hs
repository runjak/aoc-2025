module Main where

import Control.Monad (forM_, guard)
import Data.Bifunctor (first)
import Data.List qualified as List
import Data.List.Split qualified as Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Distribution.Compat.Prelude (readMaybe)
import Numeric.LinearProgramming (Bound (..))
import Numeric.LinearProgramming qualified as LP

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
type EnumeratedProblem = [[(Int, Shape)]]

enumerateProblem :: Problem -> EnumeratedProblem
enumerateProblem = go [1 ..]
  where
    go :: [Int] -> Problem -> EnumeratedProblem
    go _ [] = []
    go ns (set : rest) =
      let ns' = drop (Set.size set) ns
          es = zip ns $ Set.elems set
       in es : go ns' rest

pickExactlyOne :: [Int] -> LP.Bound [(Double, Int)]
pickExactlyOne variables = [(1, v) | v <- variables] :==: 1

pickAtMostOne :: [Int] -> LP.Bound [(Double, Int)]
pickAtMostOne variables = [(1, v) | v <- variables] :<=: 1

choices :: [a] -> [(a, [a])]
choices xs = catMaybes $ zipWith (\i t -> (,i <> drop 1 t) <$> listToMaybe t) (List.inits xs) (List.tails xs)

collisions :: EnumeratedProblem -> [[Int]]
collisions = concatMap (uncurry go) . choices
  where
    go :: [(Int, Shape)] -> EnumeratedProblem -> [[Int]]
    go stack others' = do
      let others = concat others'
      (variable, shape) <- stack
      let colliding = map fst $ filter (not . Set.null . Set.intersection shape . snd) others
      return $ variable : colliding

foo :: (Int, Shape) -> Map Position (Set Int)
foo (i, ps) = Map.fromList [(p, Set.singleton i)|p <- Set.elems ps]

foo' :: [(Int, Shape)] -> Map Position (Set Int)
foo' = Map.unionsWith Set.union . map foo

foo'' :: EnumeratedProblem -> Map Position (Set Int)
foo'' = Map.unionsWith Set.union . map foo'

otherCollisions :: EnumeratedProblem -> [[Int]]
otherCollisions eProblem = do
  let maxUsedVar = maximum $ concatMap (map fst) eProblem
      vars = [maxUsedVar..]
  -- We need a map from positions to sets of involved vars
  -- type WantedSet = Map Position (Set Int)
  let wantedMap = foo'' eProblem
  map Set.elems $ Map.elems wantedMap

simplex :: EnumeratedProblem -> LP.Solution
simplex problem =
  let onePerStack = map (pickExactlyOne . map fst) problem
      noCollisions = map pickAtMostOne $ otherCollisions problem
      constraints = LP.General $ onePerStack <> noCollisions
      variables = map fst $ concat problem
      optimization = LP.Maximize $ replicate (length variables) 1
      bounds = [v :&: (0, 1) | v <- variables]
   in LP.simplex optimization constraints bounds

solved :: LP.Solution -> Bool
solved (LP.Feasible _) = True
solved (LP.Optimal _) = True
solved _ = False

solve1 :: Input -> Int
solve1 (shapes, regions) = length . filter solved $ map (simplex . enumerateProblem . placementProblem shapes) regions

test = solve1 . readInput <$> readFile exampleFile

testSimplex :: IO ()
testSimplex = do
  (shapes, regions) <- readInput <$> readFile exampleFile
  forM_ regions $ \region -> do
    let problem = enumerateProblem $ placementProblem shapes region
    print $ simplex problem
    return ()
  return ()

solution1 :: String -> String
solution1 = show . solve1 . readInput

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
