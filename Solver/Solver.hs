import Data.Ratio

data Constant = Pi | E
    deriving(Show, Eq)
data UnaryOperator = Minus | Factorial | Sin | Cos | Tan
    deriving(Show, Eq)
data BinaryOperator = Add | Sub | Mul | Div | Mod | Exp | Log
    deriving(Show, Eq)
data Expression = Value Rational
                | Variable String
                | Constant Constant
                | Unary UnaryOperator Expression
                | Binary BinaryOperator Expression Expression
    deriving(Eq)
data Equation = Equation Expression Expression

instance Show Expression where
    show (Value val)                 = show $ fromRational val
    show (Variable name)             = name
    show (Constant Pi)               = "π"
    show (Constant E)                = "e"
    show (Unary Factorial expr)      = (show expr) ++ "!"
    show (Unary Minus expr)          = '-' : (show expr)
    show (Unary op expr)             = (show op) ++ "(" ++ (show expr) ++ ")"
    --show (Binary Exp a (Value val))  = --TODO pretty print using "¹²³⁴⁵⁶⁷⁸⁹"
    show (Binary op a b)             = "(" ++ (show a) ++ " " ++ (show op) ++ " " ++ (show b) ++ ")"
