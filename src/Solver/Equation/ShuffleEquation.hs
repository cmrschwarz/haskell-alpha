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
shuffleEquation (Equation left right) =  
    case (left, right) of
        (Multi Add a, Multi Add b) -> shuffleMulti Add a b
        (Multi Add a, b) -> shuffleMulti Add a [b]
        (a, Multi Add b) -> shuffleMulti Add [a] b
        _ -> []
    ++
    case (left, right) of
        (Multi Mul a, Multi Mul b) -> shuffleMulti Mul a b
        (Multi Mul a, b) -> shuffleMulti Mul a [b]
        (a, Multi Mul b) -> shuffleMulti Mul [a] b
        _ -> []
                   
shuffleMulti :: MultiOperator -> [Expression] -> [Expression] -> [Equation]
shuffleMulti op left right = 
    map (\x -> case x of
            ([], r) -> (Equation (Value 0) (Multi op r))
            (l, []) -> (Equation (Multi op l) (Value 0))
            ([v], r) -> (Equation v (Multi op r))
            (l, [v]) -> (Equation (Multi op l) v)
            (l, r) -> (Equation (Multi op l) (Multi op r))
        )
        (separations left right (inverseUnary (inverseOperator op)))


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
