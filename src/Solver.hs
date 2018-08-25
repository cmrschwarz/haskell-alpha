module Solver where

import Data.Ratio
import Data.List

data Constant = Pi | E
    deriving(Show, Eq)
data UnaryOperator = Minus | Div | Factorial | Sin | Cos | Tan
    deriving(Show, Eq)
data BinaryOperator = Mod | Exp | Log
    deriving(Show, Eq)
data MultiOperator = Add | Mul
    deriving(Show, Eq)
data Expression = Value Rational
                | Variable String
                | Constant Constant
                | Unary UnaryOperator Expression
                | Binary BinaryOperator Expression Expression
                | Multi MultiOperator [Expression]
    deriving(Eq)
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

instance Show Equation where
    show (Equation a b)             = show a ++ " = " ++ show b
