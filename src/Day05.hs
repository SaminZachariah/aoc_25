module Main where

import AoCUtils
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = case args of
        ("test" : _) -> "inputs/day05_test.txt"
        ("example" : _) -> "inputs/day05_example.txt"
        _ -> "inputs/day05.txt"

  putStrLn $ "Running with: " ++ inputFile
  inputLines <- readInputLines inputFile

  let database = parseData inputLines

  let part1 = solve1 database
  putStrLn $ "Part 1: " ++ show part1

  let part2 = solve2 database
  putStrLn $ "Part 2: " ++ show part2

type IngredientID = Int

type IDRange = (Int, Int)

type Database = ([IDRange], [IngredientID])

parseData :: [String] -> Database
parseData strLines =
  case splitOn [""] strLines of
    [rs, ids] -> (map parseIDRange rs, map parseIngredientID ids)
    _ -> error "unexpected input format, missing empty-string element"
  where
    parseIngredientID = read
    parseIDRange r =
      case splitOn "-" r of
        [s, e] -> (read s, read e)
        _ -> error "unexpected range format"

isFresh :: [IDRange] -> IngredientID -> Bool
isFresh rs ing = any (inRange ing) rs
  where
    inRange i (s, e) = s <= i && i <= e

solve1 :: Database -> Int
solve1 (ranges, ingredients) = length available
  where
    available = filter id $ map (isFresh ranges) ingredients

solve2 :: Database -> Int
solve2 _ = 0
