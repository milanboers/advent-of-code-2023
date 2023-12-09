type History = [Int]

diffs :: History -> History
diffs [] = []
diffs [_] = []
diffs (x : y : ys) = (y - x) : diffs (y : ys)

nextNumber :: History -> Int
nextNumber xs | all (== 0) xs = 0
nextNumber xs = last xs + nextNumber (diffs xs)

findAnswer :: [History] -> Int
findAnswer = sum . map nextNumber

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let histories = map (map read . words) input
  print $ findAnswer histories
