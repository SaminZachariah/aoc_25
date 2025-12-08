module Main where

import AoCUtils
import Data.Map.Strict qualified as Map
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

-- for part2, instead of counting the number of splits as we push all beams forward one row,
-- we use the 'quantum' interpretation and count the number of *timelines* which is
-- equivalent to the number of possible paths where a split either goes left, or it goes right.
-- seems like a good case for recursion?

-- Instead of tracking a set of beams, we follow one beam (still one step at a time)
-- If the beam reached the end, return 1 (since we found one end-to-end path)
-- If there is no splitter in front of the beam, move it forward, but leave pathcount the same
-- If there is a splitter, make 2 recursive calls to consider the left and right timeline,
-- and return the sum of the left-option-path-count + right-option-path-count

-- | Takes (beam-pos, rows of splitters), returns path-count
quantumSplit :: (Int, [SplitPos]) -> Int
quantumSplit (_, []) = 1 -- Base case, we found a path
quantumSplit (b, s : ss)
  | Set.notMember b s = quantumSplit (b, ss) -- no splitter, push forward
  | otherwise = quantumSplit (b - 1, ss) + quantumSplit (b + 1, ss) -- go left and right

-- | Memoized implementation of quantumSplit to address exponential time complexity
-- | naive implementation made 2 calls at each splitter, so O(2^n) for n rows (n=140).
-- | memoized implementation is O(n^2), since we could compute pathCount for every point in grid

-- | The memo-table would be more efficient if it were (Int, Int) to Int, using beam-pos row-idx
-- | but using (beam-pos, list-of-remaining-splitters) explicitly matches naive strategy
-- | and helps mentally distinguish memoization logic from puzzle-logic
type MemoTable = Map.Map (Int, [SplitPos]) Int -- (beam-pos, splitter-list) to pathCount

quantumSplitMemo :: MemoTable -> (Int, [SplitPos]) -> (MemoTable, Int)
quantumSplitMemo memtab (_, []) = (memtab, 1) -- base case
quantumSplitMemo memtab (b, s : ss) =
  case Map.lookup (b, ss) memtab of
    Just pathCount -> (memtab, pathCount) -- Cache hit, return precomputed result
    Nothing -> (newMemTab, count') -- Cache miss, updated memo-table and count from recursion
      where
        newMemTab = Map.insert (b, ss) count' memtab' -- Update the memo table
        (memtab', count') -- Actual result from recursive call
          | Set.notMember b s = quantumSplitMemo memtab (b, ss) -- no split, push forward
          | otherwise = (mem2, lcount + rcount) -- sum sub-problem results
          where
            (mem1, lcount) = quantumSplitMemo memtab (b - 1, ss) -- use input memo table
            (mem2, rcount) = quantumSplitMemo mem1 (b + 1, ss) -- use newer mem1 table

solve2 :: String -> Int
solve2 input = finalCount
  where
    (beamSet, splitters) = parseInput input
    initBeam = Set.elemAt 0 beamSet
    (_, finalCount) = quantumSplitMemo Map.empty (initBeam, splitters)