
import Jana.Ast

fib = [ Proc { procname = "fib"
             , params = [ (Int, "x1"), (Int, "x2"), (Int, "n") ]
             , body = [ IfElse (BinOp Equal
                                      (LV $ Scalar "n")
                                      (NumConst 0))
                               [ Assign PlusEq (Scalar "x1") (NumConst 1)
                               , Assign PlusEq (Scalar "x2") (NumConst 1) ]
                               [ Call "fib" [ "x1", "x2", "n" ]
                               , Assign PlusEq (Scalar "x1") (LV $ Scalar "x2")
                               , Swap "x1" "x2" ]
                               (BinOp Equal
                                      (LV $ Scalar "x1")
                                      (LV $ Scalar "x2")) ] }

      ] :: Program

main :: IO ()
main = print fib
