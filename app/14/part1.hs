import Data.List (intercalate, sortBy)
import Data.List.Split (splitOn)
import Data.Matrix (Matrix, ncols, nrows, (!))
import qualified Data.Matrix as Matrix
import Data.Ord (Down (Down), comparing)
import qualified Data.Vector as Vector

type Field = Matrix Char

orderRocks :: String -> String
orderRocks = intercalate "#" . map (sortBy (comparing Down)) . splitOn "#"

getCol :: Field -> Int -> String
getCol field c = Vector.toList $ Matrix.getCol c field

setCol :: Field -> Int -> String -> Field
setCol field c vals = foldl (\f (r, v) -> Matrix.setElem v (r, c) f) field $ zip [1 ..] vals

rollCol :: Field -> Int -> Field
rollCol field c = setCol field c $ orderRocks $ getCol field c

rollCols :: Field -> Field
rollCols field = foldl rollCol field [1 .. ncols field]

findAnswer :: Field -> Int
findAnswer field = sum [nrows field - r + 1 | r <- [1 .. nrows field], c <- [1 .. ncols field], rolled ! (r, c) == 'O']
  where
    rolled = rollCols field

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let field = Matrix.fromLists input
  print $ findAnswer field