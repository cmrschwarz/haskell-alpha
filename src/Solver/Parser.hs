module Solver.Parser (parseExpression) where

import Solver

import Data.List
import Data.Char
import Data.List.Split

data Token = TUnaryOp Char
           | TBinaryOp Char
           | TVariable String
           | TValue Rational
           | TConstant String
           | TParenOpen
           | TParenClose 
    deriving(Show)

tokenize :: String -> Bool -> [Token] 
tokenize (s:xs) prev_op 
    | isSpace s                             = tokenize xs prev_op
    | s == '('                              = TParenOpen : (tokenize xs False)
    | s == ')'                              = TParenClose : (tokenize xs False)
    | prev_op && (s `elem` ['+', '-'])      = (TUnaryOp s) : (tokenize xs True)
    | not prev_op && (s `elem` ['+', '-'])  = (TBinaryOp s) : (tokenize xs True)
    | s `elem` ['*', '/', '^', '%']         = (TBinaryOp s) : (tokenize xs True)
    | isDigit s                             = (TValue $ toRational $ read a) : tokenize b False
    | otherwise                             = (TVariable c) : tokenize d False
    where
        (a, b) = keepWhile isDigit (s:xs) 
        (c, d) = keepWhile (not . isSpace) (s:xs) 
tokenize [] prev_op = []

keepWhile :: (a -> Bool) -> [a] -> ([a], [a]) 
keepWhile f l = (\a b -> (a, drop (length a) b)) (takeWhile f l) l 

prec :: Token -> Int
prec (TUnaryOp o)
    | o == '-' = 10
    | o == '+' = 10
prec (TBinaryOp o)
    | o == '-' = 1
    | o == '+' = 1
    | o == '*' = 2
    | o == '/' = 2
    | o == '^' = 4
prec TParenOpen = 0


parseExpression :: String -> Expression
parseExpression s = shuntingYard (tokenize s True) [] []
        
shuntingYard :: [Token] -> [Token] -> [Expression] -> Expression

shuntingYard ((TValue v):ts) ops expr = 
    shuntingYard ts ops (Value v : expr)

shuntingYard ((TVariable v):ts) ops expr = 
    shuntingYard ts ops (Variable v : expr)

shuntingYard (t@(TUnaryOp u):ts) ops expr = shuntingYard ts (t:ops') expr'
    where
        (ops', expr') = collapseWhile ((>(prec t)) . prec) ops expr

shuntingYard (t@(TBinaryOp b):ts) ops expr = shuntingYard ts (t:ops') expr'
    where
        (ops', expr') = collapseWhile ((>(prec t)) . prec) ops expr

shuntingYard (TParenOpen : ts) ops expr =
    shuntingYard ts (TParenOpen : ops) expr

shuntingYard (TParenClose : ts) ops expr =
    shuntingYard ts ops' expr'
        where
            ((_: ops'), expr') = collapseWhile (not . isParenOpen) ops expr
                where 
                    isParenOpen TParenOpen = True
                    isParenOpen _ = False

shuntingYard [] ops expr        = result 
    where
        [result]                = snd $ collapseWhile (const True) ops expr

shuntingYard x y z = error (show (x, y, z))


collapseWhile :: (Token -> Bool) -> [Token] -> [Expression] -> ([Token], [Expression])
collapseWhile f (o:os) expr
    | f o                         = collapseWhile f os (makeExpr o expr)
    | otherwise                   = ((o:os), expr)
collapseWhile f [] expr = ([], expr)

makeExpr :: Token -> [Expression] -> [Expression]
makeExpr (TUnaryOp '+') (x:xs)    = x: xs
makeExpr (TUnaryOp '-') (x:xs)    = (Unary Minus x): xs
makeExpr (TBinaryOp '+') (y:x:xs) = (Multi Add [x, y]): xs
makeExpr (TBinaryOp '-') (y:x:xs) = (Multi Add [x, Unary Minus y]): xs
makeExpr (TBinaryOp '*') (y:x:xs) = (Multi Mul [x, y]) : xs
makeExpr (TBinaryOp '/') (y:x:xs) = (Multi Mul [x, Unary Div y]): xs
makeExpr (TBinaryOp '^') (y:x:xs) = (Binary Exp x y): xs

