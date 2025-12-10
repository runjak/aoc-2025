module Main where

import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Distribution.Compat.Prelude (readMaybe)

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
          buttons = map readButton . tail $ init parts
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
      let part = "[" <> init (tail part') <> "]"
      fromMaybe [] $ readMaybe part

    readJoltages :: String -> [Int]
    readJoltages part' = do
      let part = "[" <> init (tail part') <> "]"
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

type JoltageMap = Map Joltages Int

initialMap :: [Button] -> Joltages -> JoltageMap
initialMap buttons joltages = do
  Map.fromList $ map ((,1) . buttonAsJoltages' (length joltages)) buttons

mergeJoltages :: (Joltages, Int) -> (Joltages, Int) -> (Joltages, Int)
mergeJoltages (j1, steps1) (j2, steps2) = (zipWith (+) j1 j2, steps1 + steps2)

nextJoltages :: [(Joltages, Int)] -> [(Joltages, Int)]
nextJoltages (j : js) = [mergeJoltages j j' | j' <- js] <> nextJoltages js
nextJoltages [] = []

restrict :: Joltages -> JoltageMap -> JoltageMap
restrict joltages joltageMap = do
  let (keep, found, _) = Map.splitLookup joltages joltageMap
      foundMap = maybe Map.empty (Map.singleton joltages) found
  Map.union keep foundMap

joltageSteps :: Joltages -> JoltageMap -> JoltageMap
joltageSteps joltages prevMap = do
  let nextMap = restrict joltages $ Map.fromListWith min $ nextJoltages $ Map.toList prevMap
  Map.unionWith min prevMap nextMap

joltageStream :: [Button] -> Joltages -> [JoltageMap]
joltageStream buttons joltages = iterate (joltageSteps joltages) $ initialMap buttons joltages

solveJoltages :: Joltages -> [Button] -> Int
solveJoltages joltages buttons = head . mapMaybe (Map.lookup joltages) $ joltageStream buttons joltages

solution2 :: String -> String
solution2 = show . sum . map (\(_, buttons, joltages) -> solveJoltages joltages buttons) . readInput

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
