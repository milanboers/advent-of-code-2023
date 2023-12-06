import Data.List.Split (splitOn)

gameOpt :: (Int, Int) -> Int -> Bool
gameOpt (t, r) h = (t - h) * h > r

gameOpts :: (Int, Int) -> Int
gameOpts (t, r) = sum . map (fromEnum . gameOpt (t, r)) $ [0 .. t]

findAnswer :: [(Int, Int)] -> Int
findAnswer = product . map gameOpts

parseLine :: String -> [Int]
parseLine = map read . words . (!! 1) . splitOn ":"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let times = parseLine $ head input
  let distances = parseLine $ last input
  let games = zip times distances
  print $ findAnswer games
