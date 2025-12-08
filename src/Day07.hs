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

-- compute split-count and new beams given beams and next-splitters
-- e.g. beams on row 2, splitters on row 3, produce beams' on row3
stepOnce :: (BeamPos, SplitPos) -> (BeamPos, Int)
stepOnce (b, ss) = (Set.union unsplitBeams splitBeams, Set.size beamsToSplit)
  where
    unsplitBeams = Set.difference b ss
    beamsToSplit = Set.intersection b ss
    splitBeams = Set.unions $ Set.map splitOneBeam beamsToSplit

-- repeatedly call stepOnce and accumulate (BeamPos, Count)
-- if splitters is empty, we've reach the end, so return
-- otherwise, call stepOnce to get new beams, and update count
stepAll :: (BeamPos, Int) -> [SplitPos] -> (BeamPos, Int)
stepAll (bpos, c) splitters = case splitters of
  [] -> (bpos, c)
  s : ss ->
    let (bpos', cToAdd) = stepOnce (bpos, s)
     in stepAll (bpos', c + cToAdd) ss

solve1 :: String -> Int
solve1 input = splitCount
  where
    (startBeam, splitters) = parseInput input
    (_, splitCount) = stepAll (startBeam, 0) splitters

solve2 :: String -> Int
solve2 _ = 0