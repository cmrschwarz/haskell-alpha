module Solver.Backtracker (solveExpression, solveEquation) where

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

straightTransform f expr
    | expr == expr'     = []
    | otherwise         = [expr']
    where
        expr'           = f expr

expressionTransforms    = [
        straightTransform $ constFold . groupFactors,
        factorIn,
        factorOut
    ]

equationTransforms      = map applyOnEquation expressionTransforms ++ [
        shuffleEquation
    ]

expressionCost :: Expression -> Int
expressionCost (Value _)        = 0
expressionCost (Variable _)     = 0
expressionCost (Constant _)     = 0
expressionCost (Unary Minus x)  = expressionCost x
expressionCost (Unary Div x)    = expressionCost x
expressionCost (Unary _ expr)   = 1 + expressionCost expr
expressionCost (Binary _ l r)   = 2 + expressionCost l + expressionCost r
expressionCost (Multi _ exprs)  = length exprs + (sum $ map expressionCost exprs)

equationCost :: Variable -> Equation -> Int
equationCost var (Equation left right)
    | isSolved                  = -1
    | otherwise                 = expressionCost left + expressionCost right
    where
        Variable leftVar        = left
        rightContainsVar        = contains var right
        isSolved                = isVariable left && leftVar == var && (not rightContainsVar)

searchSolution :: Eq a => (a -> Int) -> [(a -> [a])] -> (a -> a) -> Int -> a -> [a]
searchSolution cost transforms simplifier maxSteps start
        = searchSolution' startState startState []
    where
        startCost               = cost start
        startState              = (start, startCost, False, 0, [])
        searchSolution' state best queue
            | currCost <= 0     = curr : previous
            | visited           = bestX : bestXS
            | otherwise         = searchSolution' nextState best' queue'
            where
                (curr, currCost, visited, steps, previous) = state
                (bestX, bestCost, _, _, bestXS) = best
                fst5 (x, _, _, _, _) = x
                seen x          = not $ any (\y -> fst5 x == fst5 y) queue
                next            = concat $ map (flip ($) curr) transforms
                createState x   = (x', xCost, False, steps + 1, x : curr : previous)
                    where
                        x'      = simplifier x
                        xCost   = cost x'
                itemsEqual x y  = fst5 x == fst5 y
                newQueue        = if steps >= maxSteps
                    then []
                    else nubBy itemsEqual $ filter seen $ map createState next
                oldQueue        = delete state queue
                queue'          = (curr, currCost, True, steps, previous) : newQueue ++ oldQueue
                best'           = if currCost < bestCost
                    then state
                    else best
                nextState       = minimumBy compareQueueItems queue'
                compareQueueItems (_, x, visitedX, lenX, _) (_, y, visitedY, lenY, _)
                    | visitedX          = GT
                    | visitedY          = LT
                    | x == y            = compare lenX lenY
                    | otherwise         = compare x y

solveExpression :: Int -> Expression -> [Expression]
solveExpression                 = searchSolution expressionCost expressionTransforms simplify

solveEquation :: Variable -> Int -> Equation -> [Equation]
solveEquation var maxSteps eq
    | null result               = []
    | null solvedRight          = result
    | otherwise                 = solvedRight' ++ result
    where
        searchSolution'         = searchSolution (equationCost var) equationTransforms simplifyEquation
        result                  = map head $ group $ searchSolution' maxSteps eq
        Equation left right     = head result
        solvedRight             = solveExpression maxSteps right
        solvedRight'            = map (Equation left) (init solvedRight)
