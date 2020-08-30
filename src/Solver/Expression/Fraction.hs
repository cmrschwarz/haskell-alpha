module Solver.Expression.Fraction (addFractions, splitDivs) where

import Solver
import Solver.Expression.Common
import Data.Ord
import Data.List
import Data.Tuple
import Data.Maybe

isDiv (Unary Div _)         = True
isDiv _                     = False

addFractions :: Expression -> [Expression]
addFractions expr@(Multi Add exprs) = Multi Mul [numerator, denominator'] : rest
    where
        rest                        = defaultSolution addFractions expr
        mapSnd f (x, y)             = (x, map f y)
        splitNumDenom (Multi Mul x) = mapSnd (\(Unary Div x) -> x) $ partition (not . isDiv) x
        splitNumDenom (Unary Div x) = swap $ splitNumDenom x
        splitNumDenom x             = ([x], [])
        (nums, denoms)              = unzip $ map splitNumDenom exprs
        factors                     = nub $ concat $ denoms
        factorCounts                = map (\xs -> map (length . flip elemIndices xs) factors) denoms
        maxFactorCounts             = map maximum $ transpose factorCounts
        numerator                   = Multi Add $ zipWith extend nums factorCounts
        denominator                 = concat $ zipWith replicate maxFactorCounts factors
        denominator'                = Unary Div $ Multi Mul $ denominator
        extend num factorCount
            | null extendedNum      = Value 1
            | otherwise             = Multi Mul extendedNum
            where
                extensionCounts     = zipWith (-) maxFactorCounts factorCount
                extensions          = concat $ zipWith replicate extensionCounts factors
                extendedNum         = num ++ extensions
addFractions expr                   = defaultSolution addFractions expr

splitDivs :: Expression -> Expression
splitDivs (Unary Div (Multi Mul exprs)) = Multi Mul (map (Unary Div) exprs)
splitDivs x = defaultSolution' splitDivs x
