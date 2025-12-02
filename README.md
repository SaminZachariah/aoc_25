# Advent of Code 2025 - Haskell

## Setup

### Install Dependencies
```bash
cabal update
cabal build
```

### Running Solutions
```bash
# Run a specific day
cabal run day01

# Run on example data
cabal run day01 example

# Or build and run directly
cabal build day01
cabal exec day01
```

## Project Structure

- `lib/AoCUtils.hs` - Common utilities for parsing and reading inputs
- `src/` - Daily solution files (Day01.hs, Day02.hs, etc.)
- `inputs/` - Input files (day01.txt, day02.txt, etc.)

## Parsing Utilities

The `AoCUtils` module provides helper functions:

### Reading Input
- `readInput :: FilePath -> IO String` - Read entire file as String
- `readInputLines :: FilePath -> IO [String]` - Read file as list of lines
- `readInputText :: FilePath -> IO Text` - Read file as Text

### Parsing Helpers
- `parseLines :: (String -> a) -> String -> [a]` - Parse each line
- `parseInts :: String -> [Int]` - Parse integers (one per line)
- `parseGrid :: String -> [[Char]]` - Parse 2D character grid
- `splitOnBlankLines :: String -> [String]` - Split on blank lines

### Utilities
- `count :: (a -> Bool) -> [a] -> Int` - Count elements matching predicate
- `applyN :: Int -> (a -> a) -> a -> a` - Apply function n times

## Adding New Days

1. Add a new executable in `aoc2025.cabal`:
```cabal
executable day02
    import:           shared-properties
    main-is:          Day02.hs
    hs-source-dirs:   src
    build-depends:    aoc2025
```

2. Create `src/Day02.hs` following the template in `Day01.hs`

3. Add your input to `inputs/day02.txt`
