module Jana.Eval where


import Prelude hiding (GT, LT, EQ)
import Control.Monad
import Control.Monad.State
import Control.Monad.Error

import Jana.Ast
import Jana.Types


unpackInt :: Value -> Eval Integer
unpackInt (JInt x) = return x
unpackInt val = throwError $ TypeMismatch "int" (showValueType val)

unpackArray :: Value -> Eval Array
unpackArray (JArray x) = return x
unpackArray val = throwError $ TypeMismatch "array" (showValueType val)

unpackStack :: Value -> Eval Stack
unpackStack (JStack x) = return x
unpackStack val = throwError $ TypeMismatch "stack" (showValueType val)


arrayLookup :: Array -> Integer -> Eval Value
arrayLookup arr idx =
  if idx < 0 || idx' >= length arr
    then throwError OutOfBounds
    else return $ JInt $ arr !! idx'
  where idx' = fromInteger idx


evalLval :: Lval -> Eval Value
evalLval (Var id) = getVar id
evalLval (Lookup id e) =
  do idx <- unpackInt =<< evalExpr e  -- FIXME: error
     arr <- unpackArray =<< getVar id
     arrayLookup arr idx


evalExpr :: Expr -> Eval Value
evalExpr (Number x) = return $ JInt x
evalExpr Nil        = return nil
evalExpr (LV lval)  = evalLval lval
evalExpr (BinOp op e1 e2) =
  do x <- evalExpr e1
     y <- evalExpr e2
     performOperation op x y
evalExpr (Top id)    =
  do stack <- unpackStack =<< getVar id
     case stack of
       (x:xs) -> return $ JInt x
       []     -> return nil
evalExpr (Empty id)  =
  do stack <- unpackStack =<< getVar id
     case stack of
       [] -> return $ JInt 1
       _  -> return $ JInt 0


