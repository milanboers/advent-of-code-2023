import Data.List (find)
import Data.List.Split (splitOn)
import Data.Matrix (Matrix, nrows, safeGetRow)
import qualified Data.Matrix as Matrix
import Data.Maybe (fromMaybe)

type Pattern = Matrix Bool

reflects :: Pattern -> Int -> Int -> Bool
reflects p l1 l2 = case (safeGetRow l1 p, safeGetRow l2 p) of
  (Just x, Just y) -> x == y && reflects p (l1 - 1) (l2 + 1)
  _ -> True

isReflectionLine :: Pattern -> Int -> Bool
isReflectionLine p l = reflects p l (l + 1)

findReflectionLine :: Pattern -> Maybe Int
findReflectionLine p = find (isReflectionLine p) [1 .. (nrows p - 1)]

patternScore :: (Pattern, Bool) -> Int
patternScore (p, True) = fromMaybe 0 (findReflectionLine p)
patternScore (p, False) = 100 * patternScore (p, True)

findAnswer :: [(Pattern, Bool)] -> Int
findAnswer = sum . map patternScore

parsePattern :: [String] -> [(Pattern, Bool)]
parsePattern raw = [(parsed, False), (Matrix.transpose parsed, True)]
  where
    parsed = Matrix.fromLists . (map . map) (== '#') $ raw

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let rawPatterns = splitOn [""] input
  let patterns = concatMap parsePattern rawPatterns
  print $ findAnswer patterns
