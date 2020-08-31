module Solver.Expression.Exponentiation (groupBases, groupExponents, negativeExpToDiv, divToNegativeExp) where

import Solver
import Solver.Expression.Common
import Data.List
import Data.Maybe

getExp :: Expression -> Expression
getExp (Binary Exp base exp) = exp
getExp _ = Value 1

getBase :: Expression -> Expression
getBase (Binary Exp base exp) = base
getBase x = x

splitExp :: Expression -> (Expression, Expression)
splitExp (Binary Exp base exp) = (base, exp)
splitExp (Unary Div x) = (x, Value (-1))
splitExp x = (x, Value 1)

groupBases :: Expression -> Expression
groupBases (Multi Mul exprs)        = Multi Mul $ map groupBases $ groupBases' exprs
    where
        groupBases' []              = []
        groupBases' (x:xs)          = grouped : groupBases' others
            where
                (base, exp)         = splitExp x
                (sameBase, others)  = partition ((==base) . getBase) xs
                grouped             = case exp : map getExp sameBase of
                    [Value 1]       -> base
                    exps            -> Binary Exp base $ Multi Add exps
groupBases x                        = defaultSolution' groupBases x

groupExponents :: Expression -> Expression
groupExponents (Multi Mul exprs)    = Multi Mul $ groupExponents' exprs
    where
        groupExponents' []          = []
        groupExponents' (x:xs)      = grouped : groupExponents' others
            where
                (base, exp)         = splitExp x
                (sameExp, others)   = partition ((==exp) . getExp) xs
                grouped             = Binary Exp (Multi Mul $ base : map getBase sameExp) exp
groupExponents x                    = defaultSolution' groupExponents x

negativeExpToDiv :: Expression -> Expression
negativeExpToDiv (Binary Exp x (Unary Minus y)) = Unary Div (Binary Exp x y)
negativeExpToDiv x                              = defaultSolution' negativeExpToDiv x

divToNegativeExp :: Expression -> Expression
divToNegativeExp (Unary Div (Binary Exp x y))   = Binary Exp x (Unary Minus y)
divToNegativeExp x                              = defaultSolution' divToNegativeExp x
