module Main where

import AoCUtils
import Data.List (foldl')
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
solve1 (ranges, ingredients) = length $ filter (isFresh ranges) ingredients

-- Part 2 Solution Overview:
-- Build a list of **disjoint** intervals, starting from an empty list
-- For each interval in input, merge it in while preserving disjointedness

-- | Helper to check if 2 intervals overlap
isOverlap :: IDRange -> IDRange -> Bool
isOverlap (s', e') (s, e)
  | e' < s = False -- I' ends before I starts
  | s' > e = False -- I' starts after I ends
  | otherwise = True -- Some kind of overlap, but we don't care about how

-- | Assumes that intervals are KNOWN to overlap
mergeTwo :: IDRange -> IDRange -> IDRange
mergeTwo (s', e') (s, e) = (min s' s, max e' e)

-- | Recursively merge a new interval I' with existing intervals
-- | Compare I' to the first existing interval I
-- | If no more intervals to check, i' is disjoint, so simply add it in
-- | If no overlap, keep I as-is, continue scanning with I'
-- | If overlap, merge I I', continue scan with newly merged interval
mergeInsert :: [IDRange] -> IDRange -> [IDRange]
mergeInsert [] i' = [i'] -- base case: no existing intervals to check
mergeInsert (i : is) i'
  | not (isOverlap i i') = i : mergeInsert is i' -- keep I intact, recurse
  | otherwise = mergeInsert is (mergeTwo i i') -- merge I I', recurse

-- | Reduce overlapping intervals to disjoint intervals
-- | Begin with empty list as accumulator to return
-- | Scan over each input interval and merge into accumulator
-- | point-free on input-intervals
insertAll :: [IDRange] -> [IDRange]
insertAll = foldl' mergeInsert []

solve2 :: Database -> Int
solve2 (rs, _) = sum . map countIDs $ insertAll rs
  where
    countIDs (s, e) = e - s + 1 -- tally IDs in ONE interval