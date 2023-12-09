type History = [Int]

diffs :: History -> History
diffs [] = []
diffs [_] = []
diffs (x : y : ys) = (y - x) : diffs (y : ys)

prevNumber :: History -> Int
prevNumber xs | all (== 0) xs = 0
prevNumber xs = head xs - prevNumber (diffs xs)

findAnswer :: [History] -> Int
findAnswer = sum . map prevNumber

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let histories = map (map read . words) input
  print $ findAnswer histories
