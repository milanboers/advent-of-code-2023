{-# LANGUAGE TupleSections #-}

import Data.List.Split (splitOn)
import Data.Matrix (Matrix, ncols, nrows, safeGetRow, (!))
import qualified Data.Matrix as Matrix

type Pattern = Matrix Bool

data Dir = Hor | Ver deriving (Show, Eq)

reflects :: Pattern -> Int -> Int -> Bool
reflects p l1 l2 = case (safeGetRow l1 p, safeGetRow l2 p) of
  (Just x, Just y) -> x == y && reflects p (l1 - 1) (l2 + 1)
  _ -> True

isReflectionLine :: Pattern -> Int -> Bool
isReflectionLine p l = reflects p l (l + 1)

findReflectionLines :: Pattern -> [Int]
findReflectionLines p = filter (isReflectionLine p) [1 .. (nrows p - 1)]

findReflectionLinesWithDir :: Pattern -> [(Int, Dir)]
findReflectionLinesWithDir p = horLines ++ verLines
  where
    horLines = map (,Hor) $ findReflectionLines (Matrix.transpose p)
    verLines = map (,Ver) $ findReflectionLines p

findReflectionLineWithSmudge :: Pattern -> (Int, Dir)
findReflectionLineWithSmudge p = head smudgedReflections
  where
    originalLines = findReflectionLinesWithDir p
    smudgedPatterns = [Matrix.setElem (not (p ! (r, c))) (r, c) p | r <- [1 .. nrows p], c <- [1 .. ncols p]]
    smudgedReflections = [x | p' <- smudgedPatterns, x <- findReflectionLinesWithDir p', x `notElem` originalLines]

patternScore :: Pattern -> Int
patternScore p = case findReflectionLineWithSmudge p of
  (x, Hor) -> x
  (x, Ver) -> x * 100

findAnswer :: [Pattern] -> Int
findAnswer = sum . map patternScore

parsePattern :: [String] -> Pattern
parsePattern = Matrix.fromLists . (map . map) (== '#')

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let rawPatterns = splitOn [""] input
  let patterns = map parsePattern rawPatterns
  print $ findAnswer patterns
