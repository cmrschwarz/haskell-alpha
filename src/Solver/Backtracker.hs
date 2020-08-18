module Solver.Backtracker (solveExpression, solveEquation) where

import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.PSQueue as PQ

import Solver
import Solver.Equation.MoveOperands
import Solver.Expression.Factor
import Solver.Expression.Simplify
import Solver.Expression.Fraction
import Solver.Expression.Exponentiation

simplifyEquation (Equation left right) = Equation (simplify left) (simplify right)

applyOnEquation f (Equation left right) = left' ++ right'
    where
        left'       = map (flip Equation right) (f left)
        right'      = map (Equation left) (f right)

straightTransform f expr = [f expr]

expressionTransforms    = [
        straightTransform $ negativeExpToDiv,
        straightTransform $ divToNegativeExp,
        straightTransform $ groupBases,
        straightTransform $ groupExponents,
        straightTransform $ groupFactors,
        ungroupFactors,
        factorIn,
        factorOut,
        addFractions
    ]

equationTransforms      = map applyOnEquation expressionTransforms ++ [
        moveOperands
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
    | noVarInRight              = accumulatedCost
    | otherwise                 = 1000 + accumulatedCost
    where
        accumulatedCost         = expressionCost left + expressionCost right
        Variable leftVar        = left
        noVarInRight            = not $ contains var right
        isSolved                = isVariable left && leftVar == var && noVarInRight

data SearchState a = SearchState a Int Int [a]
    deriving(Show)

instance Eq a => Eq (SearchState a) where
    (==) (SearchState x _ _ _) (SearchState y _ _ _) = x == y

instance Eq a => Ord (SearchState a) where
    compare (SearchState _ x _ _) (SearchState _ y _ _) = compare x y

applyTransforms cost fSimplify transforms oldQueue state  = foldl applyTransform oldQueue transforms
    where
        SearchState curr _ remSteps steps       = state
        newSteps                                = curr : steps
        applyTransform queue f                  = foldl insertExpr queue (f curr)
        insertExpr queue newExpr                = newQueue
            where
                simpleExpr                      = fSimplify newExpr
                newCost                         = cost simpleExpr
                newState                        = SearchState simpleExpr newCost (remSteps - 1) newSteps
                newQueue                        = if isJust $ PQ.lookup newState queue
                    then queue
                    else PQ.insert newState newCost queue

searchSolution :: (Eq a, Show a) => (a -> Int) -> (a -> a) -> [(a -> [a])] -> Int -> a -> [a]
searchSolution cost fSimplify transforms maxSteps start   = searchSolution' $ PQ.singleton startState startCost
    where
        maxPrio                                 = maxBound :: Int
        startCost                               = cost start
        startState                              = SearchState start startCost maxSteps []
        searchSolution' queue                   = if cPrio == maxPrio || cRemSteps <= 0
                then bestExpr: bestSteps
                else searchSolution' $ applyTransforms cost fSimplify transforms queue' cSearchState
            where
                cBinding                            = fromJust $ PQ.findMin queue
                cSearchState                        = PQ.key cBinding
                cPrio                               = PQ.prio cBinding
                SearchState _ _ cRemSteps cSteps    = cSearchState
                queue'                              = PQ.adjust (const maxPrio) cSearchState queue
                SearchState bestExpr _ _ bestSteps  = minimum $ PQ.keys queue

solveExpression :: Int -> Expression -> [Expression]
solveExpression maxSteps expr   = searchSolution expressionCost simplify expressionTransforms maxSteps expr

solveEquation :: Variable -> Int -> Equation -> [Equation]
solveEquation var maxSteps eq
    | null result               = []
    | null solvedRight          = result
    | otherwise                 = solvedRight' ++ result
    where
        searchSolution'         = searchSolution (equationCost var) simplifyEquation equationTransforms
        result                  = searchSolution' maxSteps eq
        Equation left right     = head result
        solvedRight             = solveExpression maxSteps right
        solvedRight'            = map (Equation left) (init solvedRight)
