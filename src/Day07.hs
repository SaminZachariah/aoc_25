module Main where

import AoCUtils
import Data.Set qualified as Set
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let inputFile = case args of
        ("test" : _) -> "inputs/day07_test.txt"
        ("example" : _) -> "inputs/day07_example.txt"
        _ -> "inputs/day07.txt"

  putStrLn $ "Running with: " ++ inputFile
  rawInput <- readInput inputFile

  let part1 = solve1 rawInput
  putStrLn $ "Part 1: " ++ show part1

  let part2 = solve2 rawInput
  putStrLn $ "Part 2: " ++ show part2

type BeamPos = Set.Set Int

type SplitPos = Set.Set Int

parseInput :: String -> (BeamPos, [SplitPos])
parseInput s = (Set.fromList beam, map Set.fromList splitters)
  where
    allLines = map (zip [0 ..]) . lines $ s
    (start, rest) = case allLines of
      [] -> ([], [])
      (x : xs) -> (x, xs)
    beam = map fst . filter ((== 'S') . snd) $ start
    splitters = map (map fst . filter ((== '^') . snd)) rest

-- | Assumes input beam should be split, returns {b-1,b+1}
splitOneBeam :: Int -> BeamPos
splitOneBeam pos = Set.fromList [pos - 1, pos + 1]

-- compute new split-count and new beams given current-beams and next-splitters
-- e.g. beams on row 2, splitters on row 3, produce beams' on row3
stepOnce :: (BeamPos, Int) -> SplitPos -> (BeamPos, Int)
stepOnce (b, c) splits = (b', c')
  where
    unsplitBeams = Set.difference b splits -- beams that are not directly above a splitter
    beamsToSplit = Set.intersection b splits -- beams directly above a splitter
    splitBeams = Set.unions . Set.map splitOneBeam $ beamsToSplit -- union result of all splits
    b' = Set.union unsplitBeams splitBeams
    c' = c + Set.size beamsToSplit -- count beams

-- foldl over rows of splitters, applying stepOnce each time, accumulating
-- new-beam positions and total count
solve1 :: String -> Int
solve1 input = splitCount
  where
    (startBeam, splitters) = parseInput input
    (_, splitCount) = foldl stepOnce (startBeam, 0) splitters

solve2 :: String -> Int
solve2 _ = 0