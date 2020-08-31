module Solver.Expression.Simplify where

import Solver
import Solver.Expression.Common
import Data.List

simplify = constFold . shorten . normalizeSigns . mergeInner

shorten :: Expression -> Expression
shorten (Multi op exprs)            = case shorten' exprs of
        []                          -> Value (neutralElement op)
        [single]                    -> single
        exprs'                      -> Multi op exprs'
    where
        inverseOp                   = inverseOperator op
        shorten' []                 = []
        shorten' (x:xs)             = case elemIndex (inverseUnary inverseOp x) xs of
            Just n                  -> shorten' $ take n xs ++ drop (n + 1) xs
            Nothing                 -> shorten x : shorten' xs
shorten (Unary Minus (Unary Minus expr)) = shorten expr
shorten (Unary Div (Unary Div expr)) = shorten expr
shorten (Unary op expr)             = Unary op (shorten expr)
shorten (Binary Exp x (Value 0))    = Value 1
shorten (Binary Exp x (Value 1))    = x
shorten (Binary op x y)             = Binary op (shorten x) (shorten y)
shorten expr                        = expr

constFold :: Expression -> Expression
constFold (Multi op exprs)
    | null rest                     = Value folded
    | null consts                   = Multi op rest
    | folded == start               = Multi op rest
    | folded == 0 && op == Mul      = Value 0
    | otherwise                     = Multi op ((Value folded) : rest)
    where
        exprs'                      = map constFold exprs
        (consts, rest)              = partition isValue exprs'
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

mergeInner :: Expression -> Expression
mergeInner expr@(Multi op exprs)
    | null inner                            = Multi op (map mergeInner exprs)
    | otherwise                             = Multi op exprs'
    where
        canInline (Multi op' _)             = op' == op
        canInline (Unary uOp (Multi op' _)) = op == op' && uOp == inverseOperator op
        canInline _                         = False
        retrieveExprs (Multi _ x)           = x
        retrieveExprs (Unary u (Multi _ x)) = map (Unary u) x
        (inner, rest)                       = partition canInline exprs
        innerExprs                          = concat $ map retrieveExprs inner
        exprs'                              = map mergeInner (innerExprs ++ rest)
mergeInner expr                             = defaultSolution' mergeInner expr

normalizeSigns :: Expression -> Expression
normalizeSigns (Multi Mul exprs)            = result
    where
        handleElem x                        = case x of
            Unary Minus y                   -> (normalizeSigns y, -1)
            y                               -> (normalizeSigns y, 1)
        (exprs', signs)                     = unzip $ map handleElem exprs
        result                              = if (-1) == foldr (*) 1 signs
            then Multi Mul $ Value (-1) : exprs'
            else Multi Mul exprs'
normalizeSigns (Multi Add exprs)            = Multi Add $ map handleElem exprs
    where
        handleElem x                        = case x of
            Unary Minus (Multi Mul exprs)   -> Multi Mul $ Value (-1) : map normalizeSigns exprs 
            expr                            -> normalizeSigns expr
normalizeSigns (Unary Minus (Multi Mul exprs)) = normalizeSigns $ Multi Mul $ Value (-1) : exprs
normalizeSigns expr                         = defaultSolution' normalizeSigns expr
