module Main where

import Data.Maybe (mapMaybe, fromMaybe)
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

test = readInput <$> readFile exampleFile

solution1 :: String -> String
solution1 = const "Not implemented"

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
