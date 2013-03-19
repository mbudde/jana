{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jana.Eval where


import Prelude hiding (GT, LT, EQ)
import Data.Bits
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error

import Jana.Ast
import Jana.Types


newtype Eval a = E { runE :: StateT Store (ReaderT ProcEnv (Either JError)) a }
               deriving (Monad, MonadError JError, MonadReader ProcEnv, MonadState Store)

runEval :: Eval a -> Store -> ProcEnv -> Either JError (a, Store)
runEval eval store procs = runReaderT (runStateT (runE eval) store) procs

-- XXX: Implement monad instances manually?
{- instance Monad Eval Store where -}
    {- return = E . return -}
    {- e >>= f = S -}


unpackInt :: Value -> Eval Integer
unpackInt (JInt x) = return x
unpackInt (JArray _) = throwError $ TypeMismatch "int" "array"
unpackInt (JStack _) = throwError $ TypeMismatch "int" "stack"


boolToInt :: Num a => (a -> a -> Bool) -> a -> a -> a
boolToInt f x y = if f x y then 1 else 0


performOperation :: (Integral a, Bits a) => BinOp -> a -> a -> a
performOperation Add  = (+)
performOperation Sub  = (-)
performOperation Mul  = (*)
performOperation Div  = undefined  -- FIXME
performOperation Mod  = mod
performOperation And  = (.&.)
performOperation Or   = (.|.)
performOperation Xor  = xor
performOperation LAnd = undefined
performOperation LOr  = undefined
performOperation GT   = boolToInt (>)
performOperation LT   = boolToInt (<)
performOperation EQ   = boolToInt (==)
performOperation NEQ  = boolToInt (/=)
performOperation GE   = boolToInt (>=)
performOperation LE   = boolToInt (<=)


evalLval :: Lval -> Eval Value
evalLval (Var id) =
  do storeEnv <- get
     return $ lookupIdent id storeEnv
evalLval (Lookup id e) =
  do storeEnv <- get
     JInt idx <- evalExpr e  -- FIXME: error
     let JArray arr = lookupIdent id storeEnv in
       if toInteger (length arr) <= idx || idx < 0
         then error "out of bounds"
         else return $ JInt (arr !! fromInteger idx)



evalExpr :: Expr -> Eval Value
evalExpr (Number x) = return $ JInt x
evalExpr Nil        = return $ nil
evalExpr (LV lval)  = evalLval lval
evalExpr (BinOp op e1 e2) =
  do x <- unpackInt =<< evalExpr e1
     y <- unpackInt =<< evalExpr e2
     return $ JInt $ performOperation op x y
evalExpr (Top _)    = undefined
evalExpr (Empty _)  = undefined


