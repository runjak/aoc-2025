module Main where

import Data.List qualified as List
import Data.Maybe (fromMaybe, mapMaybe)
import Distribution.Compat.Prelude (readMaybe)

exampleFile = "./inputs/10/example.txt"

inputFile = "./inputs/10/input.txt"

type Lights = [Bool]

type Button = [Int]

type Joltages = String

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

    readJoltages :: String -> String
    readJoltages = id

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
toggleStream buttons initialLights = do
  let ts = map toggle buttons
  iterate (concatMap (List.nub . toggles ts)) [initialLights]

solveMachine :: Lights -> [Button] -> Int
solveMachine lights buttons =
  head . map fst . filter (any (all not) . snd) . zip [0 ..] $ toggleStream buttons lights

solution1 :: String -> String
solution1 = show . sum . map (\(lights, buttons, _) -> solveMachine lights buttons) . readInput

solution2 :: String -> String
solution2 = const "Not implemented"

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
