module Solver.Equation.MoveOperands (moveOperands) where
import Solver
import Data.List

moveOperands:: Equation -> [Equation]
moveOperands (Equation left right) =  
    case (left, right) of
        (Multi Add a, Multi Add b) -> moveOperands' Add a b
        (Multi Add a, b) -> moveOperands' Add a [b]
        (a, Multi Add b) -> moveOperands' Add [a] b
        _ -> []
    ++
    case (left, right) of
        (Multi Mul a, Multi Mul b) -> moveOperands' Mul a b
        (Multi Mul a, b) -> moveOperands' Mul a [b]
        (a, Multi Mul b) -> moveOperands' Mul [a] b
        _ -> []
    where
        moveOperands' op l r = 
            case (op, l, r) of
                (Mul, [Value 0], _) -> [Equation (Value 0) (Multi Mul $take n r) | n <- [1..length r]]
                (Mul, _, [Value 0]) -> [Equation (Multi Mul $take n l) (Value 0)| n <- [1..length l]]
                _->
                    map (\x -> case x of
                            ([], r') -> (Equation (Value $neutralElement op) (Multi op r'))
                            (l', []) -> (Equation (Multi op l') (Value $neutralElement op))
                            ([v], r') -> (Equation v (Multi op r'))
                            (l', [v]) -> (Equation (Multi op l') v)
                            (l', r') -> (Equation (Multi op l') (Multi op r'))
                        )
                        (moveListElements l r (inverseUnary (inverseOperator op)))
                   

moveListElements :: [a] -> [a] -> (a->a) -> [([a], [a])]
moveListElements left right switcher = 
    [ml | n <- [1..length right], ml <- move_n_left n (length right) right left []] ++
    [mr | n <- [1..length left - 1], mr <- move_n_right n (length left) left [] right]
    where
        move_n_left n lr remaining left right =
            if (n == 0) then
                [(left, right ++ remaining)]
            else
                if(n < lr) then
                    (move_n_left (n - 1) (lr - 1) (tail remaining) (left ++ [switcher $ head remaining]) right) ++
                    (move_n_left n (lr - 1) (tail remaining) left (right ++ [head remaining]))
                else
                    if (n == lr) then
                        [(left ++ map switcher remaining, right)]
                    else
                        []
        move_n_right n lr remaining left right =
            if (n == 0) then
                [(left ++ remaining, right)]
            else
                if(n < lr) then
                    (move_n_right (n-1) (lr - 1) (tail remaining) left (right ++ [switcher $ head remaining])) ++
                    (move_n_right n (lr - 1) (tail remaining) (left ++ [head remaining]) right)
                else
                    if (n == lr) then
                        [(left, right ++ map switcher remaining)]
                    else
                        []
