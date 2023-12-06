import Data.List.Split (splitOn)

gameOpt :: (Int, Int) -> Int -> Bool
gameOpt (t, r) h = (t - h) * h > r

gameOpts :: (Int, Int) -> Int
gameOpts (t, r) = sum . map (fromEnum . gameOpt (t, r)) $ [0 .. t]

findAnswer :: (Int, Int) -> Int
findAnswer = gameOpts

parseLine :: String -> Int
parseLine = read . concat . words . (!! 1) . splitOn ":"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let t = parseLine $ head input
  let d = parseLine $ last input
  print $ findAnswer (t, d)
