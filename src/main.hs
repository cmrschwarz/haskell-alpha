import Solver
import Solver.Parser
import Solver.Backtracker

import System.Environment
import Data.List
import Data.Maybe

import Debug.Trace

handleSolution solution = do
    let solution' = map head $ group $ reverse solution
    putStrLn $ intercalate "\n" $ map show solution'

handleExpression str = do
    let expr        = parseExpression $ str
        solution    = solveExpression 6 expr

    handleSolution solution

handleEquation str = do
    let (left, right)   = case elemIndices '=' str of
            [pos]       -> splitAt pos str
            _           -> error "Input contains more than one ="
        leftExpr        = parseExpression left
        rightExpr       = parseExpression $ tail right
        equation        = Equation leftExpr rightExpr
        solution        = solveEquation "x" 6 equation

    handleSolution solution

main = do
    args <- getArgs
    let str = intercalate " " args
        isEquation  = isJust $ elemIndex '=' str

    if isEquation
        then handleEquation str
        else handleExpression str
