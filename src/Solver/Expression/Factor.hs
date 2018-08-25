module Solver.Expression.Factor where

import Solver
import Data.List

factorOut :: Expression -> [Expression]
factorOut expr@(Multi Add _)    = map (flip factorOut' expr) factors
    where
        factors                 = nub $ possibleFactors expr --TODO sort factors by frequency
        possibleFactors expr    = case expr of
            Multi Add exprs     -> concat $ map possibleFactors exprs
            Multi Mul exprs     -> exprs
            _                   -> [expr]
        factorOut' factor expr  = case expr of
            Multi Add exprs     -> Multi Mul [factor, Multi Add $ map (factorOut' factor) exprs]
            Multi Mul exprs     -> Multi Mul $ (Unary Div factor) : exprs
            _                   -> Multi Mul [Unary Div factor, expr]
factorOut _                     = []

factorIn :: Expression -> [Expression]
factorIn expr                   = case expr of
    Multi Mul [a, b]            -> (factorInto a b) ++ (factorInto b a)
    _                           -> []
    where
        factorInto a b          = case a of
            Multi Add exprs     -> [Multi Add $ map (factorInto' b) exprs]
            _                   -> []
        factorInto' factor expr = case expr of
            Multi Mul exprs     -> Multi Mul (factor : exprs)
            _                   -> Multi Mul [factor, expr]
