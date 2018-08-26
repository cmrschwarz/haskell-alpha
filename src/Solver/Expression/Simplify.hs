module Solver.Expression.Simplify where

import Solver
import Data.List
import Debug.Trace


simplify :: Expression -> Expression
simplify (Multi op exprs)           = case simplify' exprs of
        [single]                    -> single
        exprs'                      -> Multi op exprs'
    where
        simplify' []                = []
        simplify' (x:xs)            = case elemIndex (inverseUnary inverseOp x) xs of
            Just n                  -> simplify' $ take n xs ++ drop (n + 1) xs
            Nothing                 -> simplify x : simplify' xs
        inverseOp                   = case op of
            Add                     -> Minus
            Mul                     -> Div
simplify (Unary Minus (Unary Minus expr)) = simplify expr
simplify (Unary Div (Unary Div expr)) = simplify expr
simplify (Unary op expr)            = Unary op (simplify expr)
simplify (Binary op x y)            = Binary op (simplify x) (simplify y)
simplify expr                       = expr

constFold :: Expression -> Expression
constFold (Multi op exprs)
    | null rest'                    = Value folded
    | null consts                   = Multi op rest'
    | folded == start               = Multi op rest'
    | otherwise                     = Multi op ((Value folded) : rest')
    where
        (consts, rest)              = partition isValue exprs
        rest'                       = map constFold rest
        add (Value x) curr          = curr + x
        mul (Value x) curr          = curr * x
        (folder, start)             = case op of
            Add                     -> (add, 0)
            Mul                     -> (mul, 1)
        folded                      = foldr folder start consts
constFold (Unary Minus (Value x))   = Value (-x)
constFold (Unary Div (Value x))     = Value (1/x)
constFold (Unary op expr)           = Unary op (constFold expr)
constFold (Binary op x y)           = Binary op (constFold x) (constFold y)
constFold expr                      = expr
