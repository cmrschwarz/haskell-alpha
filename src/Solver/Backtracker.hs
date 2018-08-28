module Solver.Backtracker where

import Data.List
import Data.Ord

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

expressionSteps     = [
        map shorten . factorIn,
        map shorten . factorOut
    ]

equationSteps       = map applyOnEquation expressionSteps ++ [
        map simplifyEquation . shuffleEquation
    ]

expressionCost :: Expression -> Int
expressionCost expr = 1 --TODO

equationCost :: Variable -> Equation -> Int
equationCost var (Equation left right)
    | isSolved                  = -1
    | otherwise                 = expressionCost left + expressionCost right
    where
        Variable leftVar        = left
        rightContainsVar        = contains var right
        isSolved                = isVariable left && leftVar == var && (not rightContainsVar)

searchSolution :: Eq a => (a -> Int) -> [(a -> [a])] -> a -> Maybe a
searchSolution cost steps start = searchSolution' (start, cost start, []) []
    where
        searchSolution' state@(curr, currCost, previous) queue
            | currCost < 0      = Just curr
            | null queue'       = Nothing
            | otherwise         = searchSolution' nextState queue''
            where
                seen x          = not $ any (\(_, _, y) -> x `elem` y) queue
                next            = filter seen $ concat $ map (flip ($) curr) steps
                costs           = map ((+currCost) . cost) next
                queue'          = zip3 next costs (repeat (curr : previous))
                queue''         = filter (/=state) (queue' ++ queue)
                nextState       = minimumBy compareQueueItems queue''
                compareQueueItems (_, x, xs) (_, y, ys)
                    | x == y    = compare (length xs) (length ys) --TODO cache length?
                    | otherwise = compare x y
