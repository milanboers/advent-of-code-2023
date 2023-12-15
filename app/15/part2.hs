{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Char (ord)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

type Lens = (String, Int)

type Box = [Lens]

type Boxes = Map Int Box

data Step = Replace Lens | Remove String deriving (Show, Eq)

stepLabel :: Step -> String
stepLabel (Replace (l, _)) = l
stepLabel (Remove l) = l

hashC :: Int -> Char -> Int
hashC i c = ((i + ord c) * 17) `mod` 256

hash :: String -> Int
hash = foldl hashC 0

boxElem :: Box -> Lens -> Bool
boxElem box (label, _) = any (\(l, _) -> l == label) box

updateLens :: Box -> Lens -> Box
updateLens ((label, _) : ls) (uLabel, uFl) | label == uLabel = (uLabel, uFl) : ls
updateLens (l : ls) u = l : updateLens ls u
updateLens [] _ = []

updateBox :: Box -> Step -> Box
updateBox box (Replace lens) | boxElem box lens = updateLens box lens
updateBox box (Replace lens) = box ++ [lens]
updateBox box (Remove label) = filter (\(l, _) -> l /= label) box

doStep :: Boxes -> Step -> Boxes
doStep boxes step = Map.insert boxNo newBox boxes
  where
    boxNo = hash (stepLabel step)
    box = boxes ! boxNo
    newBox = updateBox box step

boxPower :: Int -> [Lens] -> Int
boxPower boxNo = sum . zipWith (curry (\(i, (_, l)) -> (1 + boxNo) * i * l)) [1 ..]

power :: Boxes -> Int
power boxes = sum $ Map.mapWithKey boxPower boxes

findAnswer :: [Step] -> Int
findAnswer steps = power afterSteps
  where
    initBoxes = Map.fromList [(i, []) | i <- [0 .. 255]]
    afterSteps = foldl doStep initBoxes steps

parseStep :: String -> Step
parseStep xs | last xs == '-' = Remove (init xs)
parseStep xs = Replace (label, read rawFocalLength)
  where
    [label, rawFocalLength] = splitOn "=" xs

main :: IO ()
main = do
  contents <- getContents
  let rawSteps = splitOn "," contents
  let steps = map parseStep rawSteps
  print $ findAnswer steps
