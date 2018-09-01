module Solver where

import Data.Ratio
import Data.List

type Variable = String
data Constant = Pi | E
    deriving(Show, Eq)
data UnaryOperator = Minus | Div | Factorial | Sin | Cos | Tan
    deriving(Show, Eq)
data BinaryOperator = Mod | Exp | Log
    deriving(Show, Eq)
data MultiOperator = Add | Mul
    deriving(Show, Eq)
data Expression = Value Rational
                | Variable Variable
                | Constant Constant
                | Unary UnaryOperator Expression
                | Binary BinaryOperator Expression Expression
                | Multi MultiOperator [Expression]
data Equation = Equation Expression Expression
    deriving(Eq)

instance Show Expression where
    show (Value val)
        | denom == 1                = show numer
        | otherwise                 = show numer ++ "/" ++ show denom
        where
            numer                   = numerator val
            denom                   = denominator val
    show (Variable name)            = name
    show (Constant Pi)              = "π"
    show (Constant E)               = "e"
    show (Unary Minus expr)         = '-' : (show expr)
    show (Unary Factorial expr)     = (show expr) ++ "!"
    show (Unary op expr)            = (show op) ++ "(" ++ (show expr) ++ ")"
    --show (Binary Exp a (Value val))  = --TODO pretty print using "¹²³⁴⁵⁶⁷⁸⁹"
    show (Binary op a b)            = "(" ++ (show a) ++ " " ++ (show op) ++ " " ++ (show b) ++ ")"
    show (Multi Add exprs)          = "(" ++ (intercalate " + " $ map show exprs) ++ ")"
    --TODO pretty print Div's in Mul
    show (Multi Mul exprs)          = "(" ++ (intercalate " * " $ map show exprs) ++ ")"

instance Eq Expression where
    (==) (Value x) (Value y)                    = x == y
    (==) (Variable x) (Variable y)              = x == y
    (==) (Constant x) (Constant y)              = x == y
    (==) (Unary opX x) (Unary opY y)            = opX == opY && x == y
    (==) (Binary opX xl xr) (Binary opY yl yr)  = opX == opY && xl == yl && xr == yr
    (==) (Multi opX xs) (Multi opY ys)          = opX == opY && listsEqual xs ys
    (==) _ _                                    = False

instance Show Equation where
    show (Equation a b)             = show a ++ " = " ++ show b

--TODO: think about improving this [O(n^2)]
listsEqual :: Eq a => [a] -> [a] -> Bool
listsEqual xs ys
    | length xs /= length ys    = False
    | differentCounts xs ys     = False
    | differentCounts ys xs     = False
    | otherwise                 = True
    where
        count x xs              = length $ elemIndices x xs
        differentCounts xs ys   = any (\x -> count x xs /= count x ys) xs

inverseUnary :: UnaryOperator -> Expression -> Expression
inverseUnary Minus (Unary Minus expr)   = expr
inverseUnary Minus expr                 = Unary Minus expr
inverseUnary Div (Unary Div expr)       = expr
inverseUnary Div expr                   = Unary Div expr
--for now we don't need to inverse Factorial or Trigonometric functions

inverseOperator :: MultiOperator -> UnaryOperator
inverseOperator Add = Minus
inverseOperator Mul = Div

neutralElement :: MultiOperator -> Rational
neutralElement Add = 0
neutralElement Mul = 1

isValue :: Expression -> Bool
isValue (Value _)               = True
isValue _                       = False

isVariable :: Expression -> Bool
isVariable (Variable _)         = True
isVariable _                    = False

contains :: Variable -> Expression -> Bool
contains var (Multi _ exprs)    = any (contains var) exprs
contains var (Binary _ l r)     = contains var l || contains var r
contains var (Unary _ expr)     = contains var expr
contains var (Variable name)    = var == name
contains _ _                    = False
