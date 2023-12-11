import Data.List ((\\))

type Galaxy = (Int, Int)

emptyRows :: Int -> [Galaxy] -> [Int]
emptyRows nr galaxies = [1 .. nr] \\ map fst galaxies

emptyCols :: Int -> [Galaxy] -> [Int]
emptyCols nc galaxies = [1 .. nc] \\ map snd galaxies

expandRowsGalaxy :: [Int] -> Galaxy -> Galaxy
expandRowsGalaxy er (gr, gc) = (gr + length (filter (< gr) er) * 999999, gc)

expandColsGalaxy :: [Int] -> Galaxy -> Galaxy
expandColsGalaxy ec (gr, gc) = (gr, gc + length (filter (< gc) ec) * 999999)

expandRows :: [Int] -> [Galaxy] -> [Galaxy]
expandRows er = map (expandRowsGalaxy er)

expandCols :: [Int] -> [Galaxy] -> [Galaxy]
expandCols ec = map (expandColsGalaxy ec)

expand :: [Int] -> [Int] -> [Galaxy] -> [Galaxy]
expand er ec = expandCols ec . expandRows er

mhd :: Galaxy -> Galaxy -> Int
mhd (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

findAnswer :: (Int, Int) -> [Galaxy] -> Int
findAnswer (nr, nc) galaxies = sum . map (uncurry mhd) $ pairs
  where
    er = emptyRows nr galaxies
    ec = emptyCols nc galaxies
    expandedGalaxies = expand er ec galaxies
    pairs = [(g1, g2) | g1 <- expandedGalaxies, g2 <- expandedGalaxies, g1 < g2]

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let nrows = length input
  let ncols = length $ head input
  let galaxies = [(r, c) | (r, line) <- zip [1 ..] input, (c, x) <- zip [1 ..] line, x == '#']
  print $ findAnswer (nrows, ncols) galaxies
