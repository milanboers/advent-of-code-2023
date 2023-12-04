{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

type Card = (Set Int, [Int])

winningNumbers :: Card -> [Int]
winningNumbers (win, own) = filter (`Set.member` win) own

score :: Card -> Int
score card = case length $ winningNumbers card of
  0 -> 0
  x -> 2 ^ (x - 1)

findAnswer :: [Card] -> Int
findAnswer = sum . map score

parseNumbers :: String -> [Int]
parseNumbers = map read . filter (not . null) . map (filter isDigit) . splitOn " "

parseCard :: String -> Card
parseCard line = (winNumbers, ownNumbers)
  where
    numbers = splitOn ":" line !! 1
    [rawWinNumbers, rawOwnNumbers] = splitOn "|" numbers
    winNumbers = Set.fromList . parseNumbers $ rawWinNumbers
    ownNumbers = parseNumbers rawOwnNumbers

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let cards = map parseCard input
  print $ findAnswer cards
