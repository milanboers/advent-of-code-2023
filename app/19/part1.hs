{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

type Category = Char

data Action = Accept | Reject | ToWorkflow String deriving (Show, Eq)

data Rule = Const Action | Gt Category Int Action | Lt Category Int Action deriving (Show, Eq)

type Workflow = [Rule]

type Workflows = Map String Workflow

type Part = Map Category Int

evalWorkflow :: Workflow -> Part -> Action
evalWorkflow ((Const action) : _) _ = action
evalWorkflow ((Gt category value action) : _) part | part ! category > value = action
evalWorkflow ((Gt {}) : xs) part = evalWorkflow xs part
evalWorkflow ((Lt category value action) : _) part | part ! category < value = action
evalWorkflow ((Lt {}) : xs) part = evalWorkflow xs part
evalWorkflow [] _ = error "no matching rules"

evalWorkflows :: Workflows -> String -> Part -> Bool
evalWorkflows wfs current part = case evalWorkflow (wfs ! current) part of
  Accept -> True
  Reject -> False
  ToWorkflow new -> evalWorkflows wfs new part

findAnswer :: Workflows -> [Part] -> Int
findAnswer wfs = sum . map sum . filter (evalWorkflows wfs "in")

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

parseRating :: [String] -> (Category, Int)
parseRating [category, rating] = (head category, read rating)
parseRating _ = error "list is not of length 2"

parsePart :: String -> Part
parsePart = Map.fromList . map (parseRating . splitOn "=") . splitOn "," . tail . init

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let [rawWorkflows, rawParts] = splitOn [""] input
  let workflows = parseWorkflows rawWorkflows
  let parts = map parsePart rawParts
  print $ findAnswer workflows parts
