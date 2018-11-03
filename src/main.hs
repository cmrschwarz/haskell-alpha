import Solver
import Solver.Parser
import Solver.Backtracker

import System.Environment
import Data.List

main = do
    args <- getArgs
    let expr        = parseExpression $ intercalate " " args
        solution    = solveExpression 5 expr
        solution'   = map head $ group $ reverse solution

    putStrLn $ intercalate "\n" $ map show solution'
