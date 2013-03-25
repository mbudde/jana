module Jana.Test.Fib where

import Prelude hiding (GT, LT, EQ)
import Jana.Ast

fibMain = ProcMain [ Scalar Int "x1", Scalar Int "x2", Scalar Int "n" ]
                 [ Assign AddEq (Var "n") (Number 4)
                 , Call "fib" ["x1", "x2", "n"] ]

fib = Proc { procname = "fib"
           , params   = [ (Int, "x1"), (Int, "x2"), (Int, "n") ]
           , body     = [ If (BinOp EQ (LV $ Var "n")
                                       (Number 0))
                             [ Assign AddEq (Var "x1") (Number 1)
                             , Assign AddEq (Var "x2") (Number 1) ]
                             [ Assign SubEq (Var "n") (Number 1)
                             , Call "fib" [ "x1", "x2", "n" ]
                             , Assign AddEq (Var "x1") (LV $ Var "x2")
                             , Swap "x1" "x2" ]
                             (BinOp EQ (LV $ Var "x1")
                                       (LV $ Var "x2"))
                        ]
           }

fibProg = (fibMain, [fib])
