module Solver.Backtracker where

import Data.List
import Data.Ord

import Solver
import Solver.Equation.ShuffleEquation
import Solver.Expression.Factor
import Solver.Expression.Simplify

simplifyEquation (Equation left right) = Equation (simplify left) (simplify right)

applyOnEquation f (Equation left right) = left' ++ right'
    where
        left'       = map (flip Equation right) (f left)
        right'      = map (Equation left) (f right)

expressionTransforms    = [
        return . shorten,
        return . constFold,
        map shorten . factorIn,
        map shorten . factorOut
    ]

equationTransforms      = map applyOnEquation expressionTransforms ++ [
        shuffleEquation
    ]

expressionCost :: Expression -> Int
expressionCost (Value _)        = 0
expressionCost (Variable _)     = 0
expressionCost (Constant _)     = 0
expressionCost (Unary _ expr)   = 1 + expressionCost expr
expressionCost (Binary _ l r)   = 1 + expressionCost l + expressionCost r
expressionCost (Multi _ exprs)  = 1 + (sum $ map expressionCost exprs)

equationCost :: Variable -> Equation -> Int
equationCost var (Equation left right)
    | isSolved                  = -1
    | otherwise                 = expressionCost left + expressionCost right
    where
        Variable leftVar        = left
        rightContainsVar        = contains var right
        isSolved                = isVariable left && leftVar == var && (not rightContainsVar)

searchSolution :: Eq a => (a -> Int) -> [(a -> [a])] -> Int -> a -> [a]
searchSolution cost transforms maxSteps start = searchSolution' startState []
    where
        startCost               = cost start
        startState              = (start, startCost, startCost, 0, [])
        searchSolution' state@(curr, currCost, costSum, steps, previous) queue
            | currCost <= 0     = curr : previous
            | null queue''      = []
            | otherwise         = searchSolution' nextState queue''
            where
                seen x          = not $ any (\(_, _, _, _, y) -> x `elem` y) queue
                next            = filter seen $ concat $ map (flip ($) curr) transforms
                createState x   = (x, xCost, xCost + costSum, steps + 1, curr : previous)
                    where
                        xCost   = cost x
                queue'          = if steps >= maxSteps
                    then []
                    else map createState next
                queue''         = queue' ++ filter (/=state) queue
                nextState       = minimumBy compareQueueItems queue''
                compareQueueItems (_, _, x, lx, _) (_, _, y, ly, _)
                    | x == y    = compare lx ly
                    | otherwise = compare x y

solveExpression :: Int -> Expression -> [Expression]
solveExpression                 = searchSolution expressionCost expressionTransforms 

solveEquation :: Variable -> Int -> Equation -> [Equation]
solveEquation var maxSteps eq
    | null result               = []
    | null solvedRight          = result
    | otherwise                 = solvedRight' ++ result
    where
        result                  = searchSolution (equationCost var) equationTransforms maxSteps eq
        Equation left right     = head result
        solvedRight             = solveExpression maxSteps right
        solvedRight'            = map (Equation left) (init solvedRight)
