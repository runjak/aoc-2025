module Main where

import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Distribution.Compat.Prelude (readMaybe)
import Numeric.LinearProgramming (Bound ((:==:)))
import Numeric.LinearProgramming qualified as LP

exampleFile = "./inputs/10/example.txt"

inputFile = "./inputs/10/input.txt"

type Lights = [Bool]

type Button = [Int]

type Joltages = [Int]

readInput :: String -> [(Lights, [Button], Joltages)]
readInput = map readLine . lines
  where
    readLine :: String -> (Lights, [Button], Joltages)
    readLine line = do
      let parts = words line
          lights = readLights $ head parts
          buttons = map readButton . drop 1 $ init parts
          joltages = readJoltages $ last parts
      (lights, buttons, joltages)

    readLights :: String -> Lights
    readLights = mapMaybe readLight

    readLight :: Char -> Maybe Bool
    readLight '.' = Just False
    readLight '#' = Just True
    readLight _ = Nothing

    readButton :: String -> Button
    readButton part' = do
      let part = "[" <> init (drop 1 part') <> "]"
      fromMaybe [] $ readMaybe part

    readJoltages :: String -> [Int]
    readJoltages part' = do
      let part = "[" <> init (drop 1 part') <> "]"
      fromMaybe [] $ readMaybe part

buttonAsLights :: Button -> Lights
buttonAsLights [] = []
buttonAsLights ns'@(n : ns)
  | n > 0 = False : buttonAsLights (map (subtract 1) ns')
  | otherwise = True : buttonAsLights (map (subtract 1) ns)

type Toggle = Lights -> Lights

toggle :: Button -> Toggle
toggle button = zipWith (\b l -> if b then not l else l) (buttonAsLights button <> repeat False)

toggles :: [Toggle] -> Lights -> [Lights]
toggles toggles lights = map ($ lights) toggles

toggleStream :: [Button] -> Lights -> [[Lights]]
toggleStream buttons initialLights =
  let ts = map toggle buttons
   in iterate (concatMap (List.nub . toggles ts)) [initialLights]

solveMachine :: Lights -> [Button] -> Int
solveMachine lights buttons =
  head . map fst . filter (any (all not) . snd) . zip [0 ..] $ toggleStream buttons lights

solution1 :: String -> String
solution1 = show . sum . map (\(lights, buttons, _) -> solveMachine lights buttons) . readInput

buttonAsJoltages :: Button -> Joltages
buttonAsJoltages = map (\l -> if l then 1 else 0) . buttonAsLights

buttonAsJoltages' :: Int -> Button -> Joltages
buttonAsJoltages' length button = take length (buttonAsJoltages button <> repeat 0)

lpProblem :: [Button] -> LP.Optimization
lpProblem buttons = LP.Minimize $ replicate (length buttons) 1

lpConstraints :: [Button] -> Joltages -> LP.Constraints
lpConstraints buttons joltages =
  let bJoltages = map (buttonAsJoltages' (length joltages)) buttons
      bounds = map (\bs -> zip (map fromIntegral bs) [1 ..]) $ List.transpose bJoltages
  in LP.General $ zipWith (:==:) bounds $ map fromIntegral joltages

test = do
  f <- readFile inputFile
  let inputs = readInput f
  return $ map (\(_, buttons, joltages) -> solveJoltages joltages buttons) inputs

solveJoltages :: Joltages -> [Button] -> Maybe Int
solveJoltages joltages buttons = go $ LP.simplex (lpProblem buttons) (lpConstraints buttons joltages) []
  where
    go :: LP.Solution -> Maybe Int
    go (LP.Optimal (n, _)) = Just $ round n
    go _ = Nothing

solution2 :: String -> String
solution2 = show . sum . mapMaybe (\(_, buttons, joltages) -> solveJoltages joltages buttons) . readInput

main :: IO ()
main = do
  putStrLn "Task 10"

  example <- readFile exampleFile
  input <- readFile inputFile

  putStrLn "solution1:"
  putStrLn $ "example: " <> solution1 example
  putStrLn $ "input: " <> solution1 input

  putStrLn "solution2:"
  putStrLn $ "example: " <> solution2 example
  putStrLn $ "input: " <> solution2 input
