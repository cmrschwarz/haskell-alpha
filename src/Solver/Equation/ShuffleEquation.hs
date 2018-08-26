module Solver.Equation.ShuffleEquation where
import Solver
import Data.List

--TODO: think about problems like:
--dividing by x
--squaring creates false solutions
--switching comparator sign in inequations
--roots creating plus minus 
--saving applied moves
shuffleEquation:: Equation -> [Equation]
shuffleEquation equation =
    case equation of 
        Equation left right ->
            case (left, right) of
                (Multi Add a, Multi Add b) -> shuffleMultiAdd a b
                (Multi Add a, b) -> shuffleMultiAdd a [b]
                (a, Multi Add b) -> shuffleMultiAdd [a] b
                (Multi Mul a, Multi Mul b) -> shuffleMultiMul a b
                (Multi Mul a, b) -> shuffleMultiMul a [b]
                (a, Multi Mul b) -> shuffleMultiMul [a] b
                   
shuffleMultiAdd :: [Expression] -> [Expression] -> [Equation]
shuffleMultiAdd left right = 
    map (\x -> case x of
            ([], r) -> (Equation (Value 0) (Multi Add r))
            (l, []) -> (Equation (Multi Add l) (Value 0))
            ([v], r) -> (Equation v (Multi Add r))
            (l, [v]) -> (Equation (Multi Add l) v)
            (l, r) -> (Equation (Multi Add l) (Multi Add r))
        )
        (separations left right (\s -> case s of
                                    Unary Minus f -> f
                                    f -> Unary Minus f
                                )
        )

shuffleMultiMul :: [Expression] -> [Expression] -> [Equation]
shuffleMultiMul left right = 
    map (\x -> case x of
            ([], r) -> (Equation (Value 1) (Multi Mul r))
            (l, []) -> (Equation (Multi Mul l) (Value 1))
            ([v], r) -> (Equation v (Multi Mul r))
            (l, [v]) -> (Equation (Multi Mul l) v)
            (l, r) -> (Equation (Multi Mul l) (Multi Mul r))
        )
        (separations left right (\s -> case s of
                                    Unary Div f -> f
                                    f -> Unary Div f
                                )
        )                

--this could be greatly optimized, e.g.  
--by taking the remaining list length as parameter to stop recursion when (len list) < m
--but we will obey Knuth in this case
separations :: [a] -> [a] -> (a->a) -> [([a], [a])]
separations a b switcher = 
    [x |  n<-[0..length elements], x<-(separate_n elements n [] [])]    
    where
        elements = (map switcher a) ++ b
        separate_n [last_remaining] m left right = 
            case m of  
                0 -> [(left, right ++ [last_remaining])]
                1 -> [(left ++ [switcher last_remaining], right)]
                _ -> []
        separate_n remaining m left right = 
            (separate_n (tail remaining) (m-1) (left ++ [switcher $ head remaining]) right) ++
            (separate_n (tail remaining) m left (right ++ [head remaining]))
