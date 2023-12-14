import Data.List (elemIndex, intercalate, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Matrix (Matrix, ncols, nrows, (!))
import qualified Data.Matrix as Matrix
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import qualified Data.Vector as Vector

type Field = Matrix Char

data Dir = N | W | S | E deriving (Show, Eq)

orderRocks :: String -> String
orderRocks = intercalate "#" . map sort . splitOn "#"

orderRocksR :: String -> String
orderRocksR = intercalate "#" . map (sortBy (comparing Down)) . splitOn "#"

getCol :: Field -> Int -> String
getCol field c = Vector.toList $ Matrix.getCol c field

setCol :: Field -> Int -> String -> Field
setCol field c vals = foldl (\f (r, v) -> Matrix.setElem v (r, c) f) field $ zip [1 ..] vals

getRow :: Field -> Int -> String
getRow field r = Vector.toList $ Matrix.getRow r field

setRow :: Field -> Int -> String -> Field
setRow field r vals = foldl (\f (c, v) -> Matrix.setElem v (r, c) f) field $ zip [1 ..] vals

rollColS :: Field -> Int -> Field
rollColS field c = setCol field c $ orderRocks $ getCol field c

rollColN :: Field -> Int -> Field
rollColN field c = setCol field c $ orderRocksR $ getCol field c

rollRowE :: Field -> Int -> Field
rollRowE field r = setRow field r $ orderRocks $ getRow field r

rollRowW :: Field -> Int -> Field
rollRowW field r = setRow field r $ orderRocksR $ getRow field r

roll :: Dir -> Field -> Field
roll S field = foldl rollColS field [1 .. ncols field]
roll N field = foldl rollColN field [1 .. ncols field]
roll E field = foldl rollRowE field [1 .. nrows field]
roll W field = foldl rollRowW field [1 .. nrows field]

rollDirs :: Field -> Field
rollDirs = roll E . roll S . roll W . roll N

rollAll :: Field -> [Field]
rollAll = iterate rollDirs

findCycle :: [Field] -> (Int, Int)
findCycle = findCycle' [] 0
  where
    findCycle' _ _ [] = error "empty list"
    findCycle' seen i (x : xs) | x `elem` seen = (i, 1 + fromJust (elemIndex x xs))
    findCycle' seen i (x : xs) = findCycle' (x : seen) (i + 1) xs

load :: Field -> Int
load field = sum [nrows field - r + 1 | r <- [1 .. nrows field], c <- [1 .. ncols field], field ! (r, c) == 'O']

findAnswer :: Field -> Int
findAnswer field = load $ rolled !! afterTotal
  where
    rolled = rollAll field
    (beforeCycles, cycleLength) = findCycle rolled
    afterCycleOffset = (1000000000 - beforeCycles) `mod` cycleLength
    afterTotal = beforeCycles + afterCycleOffset

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let field = Matrix.fromLists input
  print $ findAnswer field