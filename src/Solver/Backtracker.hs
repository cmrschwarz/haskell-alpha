module Solver.Backtracker where

import Data.List
import Data.Ord

import Debug.Trace

import Solver
import Solver.Equation.ShuffleEquation
import Solver.Expression.Factor
import Solver.Expression.Simplify

{-
	        O
	  1           1
	2   2       2   2
   3 3 3 3     3 3 3 3
-}

simplifyEquation (Equation left right) = Equation (simplify left) (simplify right)

applyOnEquation f (Equation left right) = left' ++ right'
    where
        left'       = map (flip Equation right) (f left)
        right'      = map (Equation left) (f right)

expressionTransforms    = [
        map simplify . factorIn,
        map simplify . factorOut
    ]

equationTransforms      = map applyOnEquation expressionTransforms ++ [
        map simplifyEquation . shuffleEquation
    ]

expressionCost :: Expression -> Int
expressionCost expr     = 1 --TODO

equationCost :: Variable -> Equation -> Int
equationCost var (Equation left right)
    | isSolved                  = -1
    | otherwise                 = expressionCost left + expressionCost right
    where
        Variable leftVar        = left
        rightContainsVar        = contains var right
        isSolved                = isVariable left && leftVar == var && (not rightContainsVar)

searchSolution :: Eq a => (a -> Int) -> [(a -> [a])] -> Int -> a -> [a]
searchSolution cost transforms maxSteps start = searchSolution' (start, startCost, startCost, 0, []) []
    where
        startCost               = cost start
        searchSolution' state@(curr, currCost, costSum, steps, previous) queue
            | currCost < 0      = curr : previous
            | steps >= maxSteps = []
            | null queue''      = []
            | otherwise         = searchSolution' nextState queue''
            where
                seen x          = not $ any (\(_, _, _, _, y) -> x `elem` y) queue
                next            = filter seen $ concat $ map (flip ($) curr) transforms
                createState x   = (x, xCost, xCost + costSum, steps + 1, curr : previous)
                    where
                        xCost   = cost x
                queue'          = map createState next
                queue''         = queue' ++ filter (/=state) queue
                nextState       = minimumBy compareQueueItems queue''
                compareQueueItems (_, _, x, lx, _) (_, _, y, ly, _)
                    | x == y    = compare lx ly --TODO cache length?
                    | otherwise = compare x y
