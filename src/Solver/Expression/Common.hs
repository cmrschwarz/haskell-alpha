module Solver.Expression.Common where

import Solver

replaceFirst :: Eq a => [a] -> a -> [a] -> [a]
replaceFirst [] _ _         = []
replaceFirst (x:xs) old new = if old == x
    then new ++ xs
    else x : replaceFirst xs old new

--implements recursive handling (default for all non optimizable expressions)
defaultSolution :: (Expression -> [Expression]) -> Expression -> [Expression]
defaultSolution f (Multi op exprs)  = concat $ map applyF exprs
    where
        insertResult old new        = Multi op $ replaceFirst exprs old new
        applyF part                 = map (insertResult part . return) $ f part
defaultSolution f (Binary op l r)   = left' ++ right'
    where
        right'                      = map (Binary op l) $ f r
        left'                       = map (flip (Binary op) r) $ f l
defaultSolution f (Unary op expr)   = map (Unary op) $ f expr
defaultSolution _ _                 = []

defaultSolution' :: (Expression -> Expression) -> Expression -> Expression
defaultSolution' f (Multi op exprs) = Multi op $ map f exprs
defaultSolution' f (Binary op l r)  = Binary op (f l) (f r)
defaultSolution' f (Unary op expr)  = Unary op $ f expr
defaultSolution' _ x                = x
