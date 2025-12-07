module Main where

import AoCUtils
import Data.Char (isSpace)
import Data.List (transpose)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = case args of
        ("test" : _) -> "inputs/day06_test.txt"
        ("example" : _) -> "inputs/day06_example.txt"
        _ -> "inputs/day06.txt"

  putStrLn $ "Running with: " ++ inputFile
  rawInput <- readInput inputFile

  let part1 = solve1 rawInput
  putStrLn $ "Part 1: " ++ show part1

  let part2 = solve2 rawInput
  putStrLn $ "Part 2: " ++ show part2

data Operator = Multiply | Add

instance Show Operator where
  show Multiply = "*"
  show Add = "+"

type Problem = (Operator, [Int])

solveProblem :: Problem -> Int
solveProblem (Multiply, vs) = product vs
solveProblem (Add, vs) = sum vs

parseInputP1 :: String -> [Problem]
parseInputP1 s = map parseProblem transposedInput
  where
    transposedInput = map reverse . transpose . map words . lines $ s
    parseProblem [] = error "empty problem"
    parseProblem (op : vs)
      | op == "*" = (Multiply, map read vs)
      | op == "+" = (Add, map read vs)
      | otherwise = error "unexpected problem input"

solve1 :: String -> Int
solve1 = sum . map solveProblem . parseInputP1

-- parseInputP2 :: String -> [Problem]
-- parseInputP2 s = map parseProblem transformedInput
--   where
--     inputLines = lines s -- split to lines
--     ops = last inputLines
--     nums = tail inputLines

-- | Splits input list at **all** elements that are only whitespace
splitByColumns :: [String] -> [[String]]
splitByColumns xs = case break (all isSpace) xs of
  (chunk, []) -> [chunk] -- end of list
  (chunk, _ : rest) -> chunk : splitByColumns rest

charToOp :: String -> Operator
charToOp "*" = Multiply
charToOp "+" = Add
charToOp _ = error "unexpected operator character"

solve2 :: String -> Int
solve2 s =
  let inputLines = reverse . lines $ s
      ops = map charToOp . words . head $ inputLines
      nums = (map . map) read . splitByColumns . transpose . reverse . tail $ inputLines :: [[Int]]
      zippedOpsNums = zip ops nums
   in -- ops = head transposedInput
      -- nums = tail transposedInput
      sum . map solveProblem $ zippedOpsNums

-- inputLines