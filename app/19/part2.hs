{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (partition)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

type Category = Char

data Action = Accept | Reject | ToWorkflow String deriving (Show, Eq)

data Rule = Const Action | Gt Category Int Action | Lt Category Int Action deriving (Show, Eq)

type Workflow = [Rule]

type Workflows = Map String Workflow

type Range = (Int, Int)

type AcceptedRanges = Map Category [Range]

cutRanges :: [Range] -> Int -> [Range]
cutRanges ((l, h) : xs) x | l < x && x < h = (l, x) : (x + 1, h) : cutRanges xs x
cutRanges ((l, h) : xs) x = (l, h) : cutRanges xs x
cutRanges [] _ = []

evalAction :: Workflows -> AcceptedRanges -> Action -> Int
evalAction _ ranges Accept = product . Map.map (sum . map (\(l, h) -> h - l + 1)) $ ranges
evalAction _ _ Reject = 0
evalAction wfs ranges (ToWorkflow label) = evalWorkflowByLabel wfs ranges label

evalWorkflow :: Workflows -> AcceptedRanges -> Workflow -> Int
evalWorkflow wfs ranges ((Const action) : _) = evalAction wfs ranges action
evalWorkflow wfs ranges ((Gt category value action) : otherRules) = lteAccepted + gtAccepted
  where
    categoryRange = ranges ! category
    newRanges = cutRanges categoryRange value
    (gtRanges, lteRanges) = partition (\(l, _) -> l > value) newRanges
    lteNewRanges = Map.insert category lteRanges ranges
    lteAccepted = evalWorkflow wfs lteNewRanges otherRules
    gtNewRanges = Map.insert category gtRanges ranges
    gtAccepted = evalAction wfs gtNewRanges action
evalWorkflow wfs ranges ((Lt category value action) : otherRules) = ltAccepted + gteAccepted
  where
    categoryRange = ranges ! category
    newRanges = cutRanges categoryRange (value - 1)
    (ltRanges, gteRanges) = partition (\(_, h) -> h < value) newRanges
    ltNewRanges = Map.insert category ltRanges ranges
    ltAccepted = evalAction wfs ltNewRanges action
    gteNewRanges = Map.insert category gteRanges ranges
    gteAccepted = evalWorkflow wfs gteNewRanges otherRules
evalWorkflow _ _ [] = error "no matching rules"

evalWorkflowByLabel :: Workflows -> AcceptedRanges -> String -> Int
evalWorkflowByLabel wfs ranges label = evalWorkflow wfs ranges (wfs ! label)

findAnswer :: Workflows -> Int
findAnswer wfs = evalWorkflowByLabel wfs initRanges "in"
  where
    initRanges = Map.fromList $ map (,[(1, 4000)]) "xmas"

parseAction :: String -> Action
parseAction "A" = Accept
parseAction "R" = Reject
parseAction xs = ToWorkflow xs

parseRule :: String -> Rule
parseRule xs | ':' `elem` xs = case operator of
  ">" -> Gt category rating action
  "<" -> Lt category rating action
  _ -> error "impossible"
  where
    operator = filter (\x -> x == '>' || x == '<') xs
    [rawCondition, rawAction] = splitOn ":" xs
    [rawCategory, rawRating] = splitOn operator rawCondition
    category = head rawCategory
    rating = read rawRating
    action = parseAction rawAction
parseRule xs = Const $ parseAction xs

parseWorkflow :: String -> (String, Workflow)
parseWorkflow xs = (label, rules)
  where
    [label, rest] = splitOn "{" xs
    rawRules = splitOn "," $ init rest
    rules = map parseRule rawRules

parseWorkflows :: [String] -> Workflows
parseWorkflows = Map.fromList . map parseWorkflow

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let [rawWorkflows, _] = splitOn [""] input
  let workflows = parseWorkflows rawWorkflows
  print $ findAnswer workflows
