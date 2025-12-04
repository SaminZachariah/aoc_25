module Main where

import AoCUtils
import Data.Char (digitToInt)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = case args of
        ("test" : _) -> "inputs/day03_test.txt"
        ("example" : _) -> "inputs/day03_example.txt"
        _ -> "inputs/day03.txt"

  putStrLn $ "Running with: " ++ inputFile
  rawInput <- readInput inputFile

  let banks = parseLines (map digitToInt) rawInput
  -- putStrLn $ "Banks: " ++ show banks

  -- Part 1
  let part1 = solve1 banks
  putStrLn $ "Part 1: " ++ show part1

  -- Part 2
  let part2 = solve2 banks
  putStrLn $ "Part 2: " ++ show part2

type Bank = [Int] -- list of digits from puzzle input

intFromDigits :: [Int] -> Int
-- helper to convert [1,2,3] => ["1", "2", "3"] => "123" => 123
intFromDigits = read . concatMap show

solve1 :: [Bank] -> Int
-- point-free style: s = (f . g), means s(x) == (f . g)(x) == f(g(x))
-- solve = (sum . map (bankJoltage N))
-- means solve(banks) == sum(map(bankJoltage(N), banks))
-- and map(f, [x1,x2,x3]) == [f(x1), f(x2), f(x3)]
solve1 = sum . map (bankJoltage 2)

solve2 :: [Bank] -> Int
solve2 = sum . map (bankJoltage 12)

bankJoltage :: Int -> Bank -> Int
-- Divide the input into [x0, x1 ... | x_n-c,...,x_n] aka [rest | cell]
-- Walk through 'rest' from right-to-left one element at a time.
-- At each step, see if we can improve our current cell by using the new element
bankJoltage cellSize bank =
  intFromDigits $ foldr optimizeCell cell rest
  where
    -- Divide the input into [x0, x1, ... | x_n-c, x_n-c+1,...x_n]
    (rest, cell) = splitAt (length bank - cellSize) bank

optimizeCell :: Int -> [Int] -> [Int]
-- Recursively optimize an existing cell by comparing a canditate digit
-- against the most-significant-digit of the cell.
-- e.g: candidate 3, cell [2,1].
-- Use 3 as msd: recurse with candidate 2, cell [1]
-- Use 2 as next msd: recurse with candidate 1, cell []
-- Base case hit: return to get 3:2:[] == [3,2]
optimizeCell _ [] = [] -- base-case empty list, nothing left to compare/swap
optimizeCell candidate (msd : rest)
  | candidate < msd = msd : rest -- drop canditate (it's strictly worse)
  | otherwise = candidate : optimizeCell msd rest -- use candidate, recurse
