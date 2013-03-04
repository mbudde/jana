
import Jana.Ast

fib = [ Proc { procname = "fib"
             , params = [ (Int, "x1"), (Int, "x2"), (Int, "n") ]
             , body = [ If (BinOp Eq (LV $ Scalar "n")
                                     (Number 0))
                           [ Assign AddEq (Scalar "x1") (Number 1)
                           , Assign AddEq (Scalar "x2") (Number 1) ]
                           [ Call "fib" [ "x1", "x2", "n" ]
                           , Assign AddEq (Scalar "x1") (LV $ Scalar "x2")
                           , Swap "x1" "x2" ]
                           (BinOp Eq (LV $ Scalar "x1")
                                     (LV $ Scalar "x2")) ] }

      , Proc { procname = "fib_fwd"
             , params = [ (Int, "x1"), (Int, "x2"), (Int, "n") ]
             , body = [ Assign AddEq (Scalar "n") (Number 4)
                      , Call "fib" [ "x1", "x2", "n" ] ] }
      , Proc { procname = "fib_bwd"
             , params = [ (Int, "x1"), (Int, "x2"), (Int, "n") ]
             , body = [ Assign AddEq (Scalar "x1") (Number 5)
                      , Assign AddEq (Scalar "x2") (Number 8)
                      , Uncall "fib" [ "x1", "x2", "n" ] ] }
      ] :: Program

main :: IO ()
main = print fib
