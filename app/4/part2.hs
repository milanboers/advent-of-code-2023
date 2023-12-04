{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Card = (Int, Set Int, [Int])

type Counts = Map Int Int

totalCards :: Counts -> Int
totalCards cs = sum $ Map.elems cs

winningNumbers :: Card -> [Int]
winningNumbers (_, win, own) = filter (`Set.member` win) own

scratch :: Counts -> Card -> Counts
scratch ns c@(i, _, _) = foldl (flip (Map.adjust (+ currentNum))) ns [i + 1 .. i + nWins]
  where
    currentNum = ns Map.! i
    nWins = length $ winningNumbers c

findAnswer :: [Card] -> Int
findAnswer cards = totalCards $ foldl scratch initCounts cards
  where
    initCounts = Map.fromList $ map (\(i, _, _) -> (i, 1)) cards

parseNumbers :: String -> [Int]
parseNumbers = map read . filter (not . null) . map (filter isDigit) . splitOn " "

parseCard :: String -> Card
parseCard line = (cardNr, winNumbers, ownNumbers)
  where
    [header, numbers] = splitOn ":" line
    [rawWinNumbers, rawOwnNumbers] = splitOn "|" numbers
    cardNr = read $ filter isDigit header
    winNumbers = Set.fromList . parseNumbers $ rawWinNumbers
    ownNumbers = parseNumbers rawOwnNumbers

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let cards = map parseCard input
  print $ findAnswer cards
