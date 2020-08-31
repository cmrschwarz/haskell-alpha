module Solver.Expression.Factor (groupFactors, ungroupFactors, factorOut, factorIn) where

import Solver
import Solver.Expression.Common
import Data.List
import Data.Maybe

splitScale x                = case x of
    Multi Mul exprs         -> case partition isValue exprs of
        ([], factor)        -> ([Value 1], factor)
        split               -> split
    _                       -> ([Value 1], [x])

groupFactors :: Expression -> Expression
groupFactors (Multi Add exprs)      = Multi Add $ groupFactors' splitted
    where
        splitted                    = map splitScale exprs
        groupFactors' []            = []
        groupFactors' ((scale,factor):xs)
            | null withX            = groupFactors alone : groupFactors' xs
            | otherwise             = (Multi Mul $ scales : factor) : groupFactors' rest
            where
                alone               = if scale == [(Value 1)]
                    then Multi Mul factor
                    else Multi Mul $ scale ++ factor
                (withX, rest)       = partition ((listsEqual factor) . snd) xs
                scales              = Multi Add $ concat $ scale : map fst withX

groupFactors (Binary op x y)        = Binary op (groupFactors x) (groupFactors y)
groupFactors (Unary op expr)        = Unary op (groupFactors expr)
groupFactors expr                   = expr

ungroupFactors :: Expression -> [Expression]
ungroupFactors expr@(Multi Mul exprs) = if abs scaleConst > 1
    then ungrouped : innerUngrouped
    else innerUngrouped
    where
        innerUngrouped              = defaultSolution ungroupFactors expr
        (scale, factor)             = splitScale expr
        scaleConst                  = foldl (\x (Value y) -> x * y) 1 scale
        ungrouped                   = case scaleConst of
            2                       -> Multi Add [Multi Mul factor, Multi Mul factor]
            -2                      -> Multi Add [Multi Mul (Value (-1) : factor), Multi Mul (Value (-1) : factor)]
            _                       -> if scaleConst > 0
                then Multi Add [Multi Mul factor, Multi Mul (Value (scaleConst - 1) : factor)]
                else Multi Add [Multi Mul factor, Multi Mul (Value (scaleConst + 1) : factor)]
ungroupFactors expr                 = defaultSolution ungroupFactors expr

anyCountPermutations :: [a] -> [a] -> [[a]]
anyCountPermutations stack []       = [stack]
anyCountPermutations stack xs       = stack : rest
    where
        removeFromXs index          = (\(x, y) -> x ++ tail y) $ splitAt index xs
        anyCountPermut (i, x)       = anyCountPermutations (x : stack) (removeFromXs i)
        rest                        = concat $ map anyCountPermut $ zip [0..] xs

factorOut :: Expression -> [Expression]
factorOut expr@(Multi Add _)        = results ++ innerFactored
    where
        innerFactored               = defaultSolution factorOut expr
        multipleOccurences          = (>1) . length . flip elemIndices factorCandidates
        factorCandidates            = candidates expr
        factorCandidates'           = nub $ filter multipleOccurences factorCandidates
        results                     = concat $ map (flip factorOut' expr) factorCandidates'
        candidates expr             = case expr of
            Multi Add exprs         -> concat $ map candidates exprs
            Multi Mul exprs         -> exprs
            Binary Exp x (Value n)  -> [x]
            _                       -> [expr]
        factors expr                = case expr of
            Multi Mul exprs         -> exprs
            Binary Exp x (Value n)  -> if n > 1
                then [x, expr]
                else [expr]
            _                       -> [expr]
        factorOut' factor expr      = case expr of
            Multi Add exprs         -> map (factorOut'' factor) combinations''
                where
                    (with, without) = partition ((factor`elem`) . factors) exprs
                    iCombinations   = filter ((>1) . length) $ anyCountPermutations [] [0 .. length with - 1]
                    combinations    = map (\indices -> partition ((`elem` indices) . fst) $ zip [0..] with) iCombinations
                    combinations'   = map (\(x, y) -> (map snd x, map snd y)) combinations
                    combinations''  = map (\(with', without') -> (with', without' ++ without)) combinations'
            Multi Mul exprs         -> [Multi Mul $ (Unary Div factor) : exprs]
            _                       -> [Multi Mul [Unary Div factor, expr]]
        factorOut'' factor (with, without)
            | null without          = factored
            | length with == 1      = undefined
            | otherwise             = Multi Add (factored : without)
            where
                factored            = Multi Mul [factor, Multi Add $ map divFactor with]
                inverse             = Unary Div factor
                divFactor expr      = case expr of
                    Multi Mul exprs -> Multi Mul (inverse : exprs)
                    _               -> Multi Mul [inverse, expr]
factorOut expr                      = defaultSolution factorOut expr

factorIn :: Expression -> [Expression]
factorIn expr@(Multi Mul [a, b])
    | a == b                    = (factorInto a b) ++ innerFactored
    | otherwise                 = (factorInto a b) ++ (factorInto b a) ++ innerFactored
    where
        innerFactored           = defaultSolution factorIn expr
        factorInto a b          = case a of
            Multi Add exprs     -> [Multi Add $ map (factorInto' b) exprs]
            _                   -> []
        factorInto' factor expr = case expr of
            Multi Mul exprs     -> Multi Mul (factor : exprs)
            _                   -> Multi Mul [factor, expr]
factorIn expr                   = defaultSolution factorIn expr
