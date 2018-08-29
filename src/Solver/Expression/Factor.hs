module Solver.Expression.Factor (factorOut, factorIn) where

import Solver
import Data.List
import Data.Maybe

factor f (Multi op exprs)       = concat $ map factorPart exprs
    where
        factorPart part         = map ((Multi op) . (:exprs')) $ f part
            where
                index           = fromJust $ elemIndex part exprs
                (before, after) = splitAt index exprs
                exprs'          = before ++ tail after
factor f (Binary op left right) = left' ++ right'
    where
        right'                  = map (Binary op left) $ f right
        left'                   = map (flip (Binary op) right) $ f left
factor f (Unary op expr)        = map (Unary op) $ f expr
factor _ _                      = []

factorOut :: Expression -> [Expression]
factorOut expr@(Multi Add _)    = results ++ innerFactored
    where
        innerFactored           = factor factorOut expr
        factors                 = nub $ possibleFactors expr
        results                 = map (flip factorOut' expr) factors
        possibleFactors expr    = case expr of
            Multi Add exprs     -> concat $ map possibleFactors exprs
            Multi Mul exprs     -> exprs
            _                   -> [expr]
        factorOut' factor expr  = case expr of
            Multi Add exprs     -> Multi Mul [factor, Multi Add $ map (factorOut' factor) exprs]
            Multi Mul exprs     -> Multi Mul $ (Unary Div factor) : exprs
            _                   -> Multi Mul [Unary Div factor, expr]
factorOut expr                  = factor factorOut expr

factorIn :: Expression -> [Expression]
factorIn expr@(Multi Mul [a, b])
    | a == b                    = (factorInto a b) ++ innerFactored
    | otherwise                 = (factorInto a b) ++ (factorInto b a) ++ innerFactored
    where
        innerFactored           = factor factorIn expr
        factorInto a b          = case a of
            Multi Add exprs     -> [Multi Add $ map (factorInto' b) exprs]
            _                   -> []
        factorInto' factor expr = case expr of
            Multi Mul exprs     -> Multi Mul (factor : exprs)
            _                   -> Multi Mul [factor, expr]
factorIn expr                   = factor factorIn expr
