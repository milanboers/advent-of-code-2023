{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (elemIndex, nub, sort)
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric (readHex)

type Pos = (Int, Int)

data Dir = U | D | L | R deriving (Show, Read, Eq, Ord, Enum)

type Instruction = (Dir, Int)

type GridLines = ([Int], [Int])

type Grid = (GridLines, Matrix Int)

intervals :: [Int] -> [Int]
intervals [] = []
intervals [_] = [0]
intervals (x : y : ys) = (y - x) : intervals (y : ys)

gridify :: [Pos] -> Grid
gridify poss = ((gridRows, gridCols), sizes)
  where
    gridRows = sort $ nub $ concatMap ((\r -> [r - 1, r, r + 1]) . fst) poss
    gridCols = sort $ nub $ concatMap ((\c -> [c - 1, c, c + 1]) . snd) poss
    (gridRowsInt, gridColsInt) = (intervals gridRows, intervals gridCols)
    sizes = Matrix.fromLists [[ir * ic | ic <- gridColsInt] | ir <- gridRowsInt]

realSize :: Grid -> Pos -> Int
realSize (_, sizes) pos = sizes Matrix.! pos

normalize :: Grid -> Pos -> Pos
normalize ((gridRows, gridCols), _) (r, c) = (rowIndex + 1, colIndex + 1)
  where
    rowIndex = fromJust $ elemIndex r gridRows
    colIndex = fromJust $ elemIndex c gridCols

move :: Pos -> Dir -> Int -> Pos
move (r, c) U n = (r - n, c)
move (r, c) D n = (r + n, c)
move (r, c) L n = (r, c - n)
move (r, c) R n = (r, c + n)

step :: Pos -> [Instruction] -> [Pos]
step _ [] = []
step pos ((_, 0) : xs) = step pos xs
step pos ((dir, n) : xs) = pos : step (move pos dir n) xs

neighbors :: Pos -> Set Pos
neighbors pos = Set.fromList [move pos d 1 | d <- [U ..]]

dig :: Set Pos -> Set Pos -> Set Pos
dig seen currs | Set.null currs = seen
dig seen currs = dig newSeen newCurrs
  where
    newSeen = Set.union seen currs
    newCurrs = Set.unions (Set.map neighbors currs) `Set.difference` seen

expand :: [Pos] -> [Pos]
expand [] = []
expand [_] = []
expand ((r1, c1) : (r2, c2) : xs) =
  [(r, c) | r <- [min r1 r2 .. max r1 r2], c <- [min c1 c2 .. max c1 c2]]
    ++ expand ((r2, c2) : xs)

findAnswer :: [Instruction] -> Int
findAnswer instructions = sum $ map (realSize grid) (Set.toList dug)
  where
    edge = step (1, 1) instructions
    grid = gridify edge
    normEdge = map (normalize grid) edge
    expNormEdge = expand (normEdge ++ [head normEdge])
    (firstNormR, firstNormC) = head normEdge
    dug = dig (Set.fromList expNormEdge) (Set.singleton (firstNormR + 1, firstNormC + 1))

numToDir :: Char -> Dir
numToDir '0' = R
numToDir '1' = D
numToDir '2' = L
numToDir '3' = U
numToDir _ = error "unrecognized direction"

parseLine :: String -> Instruction
parseLine line = (numToDir dirNum, d)
  where
    [_, _, rawHex] = words line
    hex = init . tail . tail $ rawHex
    dirNum = last hex
    rawN = init hex
    [(d, _)] = readHex rawN

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let instructions = map parseLine input
  print $ findAnswer instructions
