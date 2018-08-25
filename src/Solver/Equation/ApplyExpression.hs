module Solver.Equation.ApplyExpression where
import Solver
import Data.List

switchSummand :: Expression -> Expression
switchSummand e = Unary Minus e
--TODO: purge double minus


applyExpression:: Equation -> [Equation]
applyExpression equation =
    case equation of
        Equation left right ->
            case (left, right) of
                (Multi Add a, Multi Add b) ->
                    (Equation
                        (Value 0)
                        (Multi Add (b ++ (map switchSummand a)))
                    ) :
                    (Equation
                        (Multi Add (a ++ (map switchSummand b)))
                        (Value 0)
                    ) :
                    map (\x -> case x of
                            ([v], r) -> (Equation v (Multi Add r))
                            (l, [v]) -> (Equation (Multi Add l) v)
                            (l, r) -> (Equation (Multi Add l) (Multi Add r))
                        )
                        (shuffle (a, b) switchSummand)


--TODO: make this really generate all possibilitys
shuffle :: ([a], [a]) -> (a -> a)-> [([a] , [a])]
shuffle (a, b) switcher =
   [(a ++ (map switcher $ take n b), (drop n b))| n <- [1..(length b) - 1]] ++
   [(drop n a, (b ++ (map switcher $ take n a)))| n <- [1..(length a) - 1]]
