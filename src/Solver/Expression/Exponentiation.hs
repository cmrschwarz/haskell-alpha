module Solver.Expression.Exponentiation (groupBases) where

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
splitExp x = (x, Value 1)


groupBases :: Expression -> Expression
groupBases (Multi Mul exprs) = Multi Mul $ map groupBases $ groupBases' exprs
    where
        groupBases' [] = []
        groupBases' (x:xs) = (Binary Exp base $ Multi Add $ exp : map getExp sameBase) : groupBases' others
            where
                (base, exp) = splitExp x
                (sameBase, others) = partition ((==base) . getBase) xs
groupBases x = defaultSolution' groupBases x 
