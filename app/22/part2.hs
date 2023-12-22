{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

type Pos2 = (Int, Int)

type Pos3 = (Int, Int, Int) -- (z, x, y)

type Brick = (Int, (Pos2, Pos2), Int) -- (z, (xy, xy), original brick no)

getXYs :: Brick -> Set (Int, Int)
getXYs (_, ((x1, y1), (x2, y2)), _) = Set.fromList [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

fallBrick :: Brick -> Int -> Brick
fallBrick (_, xys, i) z = (z, xys, i)

steps :: Int -> Set (Int, Int) -> Set Brick -> Set Brick
steps _ _ bricks | Set.null bricks = bricks
steps z mask bricks = Set.insert newCurrBrick $ steps z newMask rest
  where
    (currBrick, rest) = Set.deleteFindMin bricks
    currBrickXYs = getXYs currBrick
    newMask = Set.union mask currBrickXYs
    canFall = Set.null $ Set.intersection currBrickXYs mask
    newCurrBrick = if canFall then fallBrick currBrick z else currBrick

collapse :: Int -> Set Brick -> Set Brick
collapse z bricks | z > maxZ = bricks
  where
    maxZ = (\(z', _, _) -> z') $ Set.findMax bricks
collapse z bricks = collapse (z + 1) newBricks
  where
    (ltBricks, gtBricks) = Set.split (z, ((0, 0), (0, 0)), 0) bricks
    newGtBricks = steps z Set.empty gtBricks
    newBricks = Set.union ltBricks newGtBricks

brickNoAndZ :: Brick -> (Int, Int)
brickNoAndZ (z, _, i) = (i, z)

disintegrate :: Set Brick -> Int -> Int
disintegrate bricks brickNo = Set.size . Set.map fst $ Set.difference withoutBrickNo' collapsed'
  where
    withoutBrickNo = Set.filter (\(_, _, i) -> i /= brickNo) bricks
    withoutBrickNo' = Set.map brickNoAndZ withoutBrickNo
    collapsed = collapse 1 withoutBrickNo
    collapsed' = Set.map brickNoAndZ collapsed

findAnswer :: Set Brick -> Int
findAnswer bricks = safeBricks
  where
    collapsed = collapse 1 bricks
    brickNos = Set.map (\(_, _, i) -> i) bricks
    safeBricks = sum $ map (disintegrate collapsed) $ Set.toList brickNos

listToPos :: [a] -> (a, a, a)
listToPos [x, y, z] = (z, x, y)
listToPos _ = error "list is not of length 3"

parsePos :: String -> Pos3
parsePos = listToPos . map read . splitOn ","

listToBrick :: [Pos3] -> (Pos3, Pos3)
listToBrick [x, y] = (x, y)
listToBrick _ = error "list if not of length 2"

parseBrick :: String -> (Pos3, Pos3)
parseBrick = listToBrick . map parsePos . splitOn "~"

splitBrick :: (Int, (Pos3, Pos3)) -> [Brick]
splitBrick (i, ((z1, x1, y1), (z2, x2, y2))) | z1 == z2 = [(z1, ((x1, y1), (x2, y2)), i)] -- not a vertical brick
splitBrick (i, ((z1, x1, y1), (z2, x2, y2))) = [(z, ((x1, y1), (x2, y2)), i) | z <- [z1 .. z2]]

splitBricks :: [(Int, (Pos3, Pos3))] -> [Brick]
splitBricks = concatMap splitBrick

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let cubes = Set.fromList $ splitBricks $ zip [1 ..] $ map parseBrick input
  print $ findAnswer cubes
