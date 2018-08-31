module Solver.Expression.Factor (groupFactors, ungroupFactors, factorOut, factorIn) where

import Solver
import Data.List
import Data.Maybe

replaceFirst :: Eq a => [a] -> a -> [a] -> [a]
replaceFirst [] _ _         = []
replaceFirst (x:xs) old new = if old == x
    then new ++ xs
    else x : replaceFirst xs old new

splitScale x                = case x of
    Multi Mul exprs         -> case partition isValue exprs of
        ([], factor)        -> ([Value 1], factor)
        split               -> split
    _                       -> ([Value 1], [x])

groupFactors :: Expression -> Expression
groupFactors (Multi Add exprs)      = Multi Add $ groupFactors' splitted
    where
        splitted                    = map splitScale exprs

        factorsEqual xs ys
            | anyNotIn xs ys        = False
            | anyNotIn ys xs        = False
            | otherwise             = True
            where
                anyNotIn x y        = any (not . (`elem`x)) y

        groupFactors' []            = []
        groupFactors' ((scale,factor):xs)
            | null withX            = groupFactors alone : groupFactors' xs
            | otherwise             = (Multi Mul $ scales : factor) : groupFactors' rest
            where
                alone               = if scale == [(Value 1)]
                    then Multi Mul factor
                    else Multi Mul $ scale ++ factor
                (withX, rest)       = partition ((factorsEqual factor) . snd) xs
                scales              = Multi Add $ concat $ scale : map fst withX

groupFactors (Binary op x y)        = Binary op (groupFactors x) (groupFactors y)
groupFactors (Unary op expr)        = Unary op (groupFactors expr)
groupFactors expr                   = expr

ungroupFactors :: Expression -> [Expression]
ungroupFactors expr@(Multi Add exprs) = results ++ innerUngrouped
    where
        results                     = mapMaybe ungroupFactors' exprs
        innerUngrouped              = factor ungroupFactors expr
        ungroup :: Rational -> [Expression] -> [Expression]
        ungroup 2 factor            = [Multi Mul factor, Multi Mul factor]
        ungroup scale factor        = [Multi Mul factor, Multi Mul (Value (scale - 1) : factor)]
        ungroupFactors' x           = if canUngroup
            then Just $ Multi Add $ replaceFirst exprs x $ ungroup scale' factor
            else Nothing
            where
                (scale, factor)     = splitScale x
                [Value scale']      = scale
                canUngroup          = length scale == 1 && scale' /= 1
ungroupFactors expr                 = factor ungroupFactors expr

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

--implements recursive handling (default for all non optimizable expressions)
factor f (Multi op exprs)       = concat $ map factorPart exprs
    where
        factorPart part         = map (Multi op . replaceFirst exprs part . return) (f part)
factor f (Binary op left right) = left' ++ right'
    where
        right'                  = map (Binary op left) $ f right
        left'                   = map (flip (Binary op) right) $ f left
factor f (Unary op expr)        = map (Unary op) $ f expr
factor _ _                      = []
