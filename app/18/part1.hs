{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

data Dir = U | D | L | R deriving (Show, Read, Eq, Ord, Enum)

type Instruction = (Dir, Int)

move :: Pos -> Dir -> Pos
move (r, c) U = (r - 1, c)
move (r, c) D = (r + 1, c)
move (r, c) L = (r, c - 1)
move (r, c) R = (r, c + 1)

step :: Pos -> [Instruction] -> [Pos]
step _ [] = []
step pos ((_, 0) : xs) = step pos xs
step pos ((dir, n) : xs) = pos : step (move pos dir) ((dir, n - 1) : xs)

neighbors :: Pos -> Set Pos
neighbors pos = Set.fromList [move pos d | d <- [U ..]]

dig :: Set Pos -> Set Pos -> Set Pos
dig seen currs | Set.null currs = seen
dig seen currs = dig newSeen newCurrs
  where
    newSeen = Set.union seen currs
    newCurrs = Set.unions (Set.map neighbors currs) `Set.difference` seen

findAnswer :: [Instruction] -> Int
findAnswer instructions = Set.size dug
  where
    edge = Set.fromList $ step (1, 1) instructions
    dug = dig edge (Set.singleton (2, 2))

parseLine :: String -> Instruction
parseLine line = (read rawDir, read rawN)
  where
    [rawDir, rawN, _] = words line

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let instructions = map parseLine input
  print $ findAnswer instructions
