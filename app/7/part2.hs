{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Char (digitToInt, intToDigit)
import Data.Function (on)
import Data.List (group, sort, sortBy)
import Data.List.Split (splitOn)

type Card = Char

type Hand = [Card]

cardScore :: Card -> Int
cardScore 'J' = 1
cardScore 'T' = 10
cardScore 'Q' = 12
cardScore 'K' = 13
cardScore 'A' = 14
cardScore x = digitToInt x

compareCard :: Card -> Card -> Ordering
compareCard c1 c2 = compare (cardScore c1) (cardScore c2)

handScore :: Hand -> Int
handScore hand = case sortBy (compare `on` (negate . length)) $ group (sort hand) of
  [[_, _, _, _, _]] -> 7
  [[_, _, _, _], [_]] -> 6
  [[_, _, _], [_, _]] -> 5
  [[_, _, _], [_], [_]] -> 4
  [[_, _], [_, _], [_]] -> 3
  [[_, _], [_], [_], [_]] -> 2
  _ -> 1

cards :: [Char]
cards = 'A' : 'K' : 'Q' : 'T' : map intToDigit [2 .. 9]

expandJs :: Hand -> [Hand]
expandJs ('J' : xs) = [replacement : rest | replacement <- cards, rest <- expandJs xs]
expandJs (x : xs) = map (x :) (expandJs xs)
expandJs [] = [[]]

handScoreWithJ :: Hand -> Int
handScoreWithJ hand = maximum [handScore newHand | newHand <- expandJs hand]

compareHandEq :: Hand -> Hand -> Ordering
compareHandEq (x : xs) (y : ys) = case compareCard x y of
  EQ -> compareHandEq xs ys
  c -> c
compareHandEq _ _ = EQ

compareHand :: Hand -> Hand -> Ordering
compareHand h1 h2
  | h1Score == h2Score = compareHandEq h1 h2
  | otherwise = compare h1Score h2Score
  where
    h1Score = handScoreWithJ h1
    h2Score = handScoreWithJ h2

findAnswer :: [(Hand, Int)] -> Int
findAnswer = sum . zipWith (curry (\(r, (_, b)) -> b * r)) [1 ..] . sortBy (\(h1, _) (h2, _) -> compareHand h1 h2)

parseLine :: String -> (Hand, Int)
parseLine line = (rawHand, read rawBid)
  where
    [rawHand, rawBid] = splitOn " " line

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let game = map parseLine input
  print $ findAnswer game
