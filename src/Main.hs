
import Jana.Ast

fib :: Program
fib = ( ProcMain [] []
      , [ Proc { procname = "fib"
               , params = [ (Int, "x1"), (Int, "x2"), (Int, "n") ]
               , body = [ If (BinOp Eq (LV $ Var "n")
                                       (Number 0))
                             [ Assign AddEq (Var "x1") (Number 1)
                             , Assign AddEq (Var "x2") (Number 1) ]
                             [ Call "fib" [ "x1", "x2", "n" ]
                             , Assign AddEq (Var "x1") (LV $ Var "x2")
                             , Swap "x1" "x2" ]
                             (BinOp Eq (LV $ Var "x1")
                                       (LV $ Var "x2")) ] }

        , Proc { procname = "fib_fwd"
               , params = [ (Int, "x1"), (Int, "x2"), (Int, "n") ]
               , body = [ Assign AddEq (Var "n") (Number 4)
                        , Call "fib" [ "x1", "x2", "n" ] ] }
        , Proc { procname = "fib_bwd"
               , params = [ (Int, "x1"), (Int, "x2"), (Int, "n") ]
               , body = [ Assign AddEq (Var "x1") (Number 5)
                        , Assign AddEq (Var "x2") (Number 8)
                        , Uncall "fib" [ "x1", "x2", "n" ] ] }
        ]
      )

main :: IO ()
main = print fib
