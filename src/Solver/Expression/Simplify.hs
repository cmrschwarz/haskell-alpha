module Solver.Expression.Simplify where

import Solver
import Data.List
import Debug.Trace

simplify = constFold . shorten

shorten :: Expression -> Expression
shorten (Multi op exprs)           = case shorten' exprs of
        [single]                    -> single
        exprs'                      -> Multi op exprs'
    where
        shorten' []                = []
        shorten' (x:xs)            = case elemIndex (inverseUnary inverseOp x) xs of
            Just n                  -> shorten' $ take n xs ++ drop (n + 1) xs
            Nothing                 -> shorten x : shorten' xs
        inverseOp                   = case op of
            Add                     -> Minus
            Mul                     -> Div
shorten (Unary Minus (Unary Minus expr)) = shorten expr
shorten (Unary Div (Unary Div expr)) = shorten expr
shorten (Unary op expr)            = Unary op (shorten expr)
shorten (Binary op x y)            = Binary op (shorten x) (shorten y)
shorten expr                       = expr

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
