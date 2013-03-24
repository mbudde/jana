module Jana.Eval (
  evalLval,
  evalExpr,
  runEval,
  evalString,
  ) where


import Prelude hiding (GT, LT, EQ)
import Data.Map (fromList)
import Control.Monad
import Control.Monad.State
import Control.Monad.Error

import Jana.Ast
import Jana.Types
import Jana.Parser (parseExprString, parseStmtsString)


unpackInt :: Value -> Eval Integer
unpackInt (JInt x) = return x
unpackInt val = throwError $ TypeMismatch "int" (showValueType val)

unpackArray :: Value -> Eval Array
unpackArray (JArray x) = return x
unpackArray val = throwError $ TypeMismatch "array" (showValueType val)

unpackStack :: Value -> Eval Stack
unpackStack (JStack x) = return x
unpackStack val = throwError $ TypeMismatch "stack" (showValueType val)

assert :: Bool -> Expr -> Eval ()
assert bool expr =
  do val1 <- evalExpr expr
     if truthy val1 == bool
       then return ()
       else throwError $ AssertionFail $ "should be " ++ show bool

assertTrue = assert True
assertFalse = assert False


arrayLookup :: Array -> Integer -> Eval Value
arrayLookup arr idx =
  if idx < 0 || idx' >= length arr
    then throwError OutOfBounds
    else return $ JInt $ arr !! idx'
  where idx' = fromInteger idx

evalStmts :: [Stmt] -> Eval ()
evalStmts = mapM_ evalStmt

evalStmt :: Stmt -> Eval ()
evalStmt (If e1 s1 s2 e2) =
  do val1 <- evalExpr e1 -- XXX: int required?
     if truthy val1
       then do evalStmts s1
               assertTrue e2
       else do evalStmts s2
               assertFalse e2
evalStmt (From e1 s1 s2 e2) =
  do assertTrue e1
     evalStmts s1
     loop
  where loop = do val <- evalExpr e2
                  if truthy val
                    then return ()
                    else loopRec
        loopRec = do evalStmts s2
                     assertFalse e1
                     evalStmts s1
                     loop
evalStmt (Swap id1 id2) =
  do val1 <- getVar id1
     val2 <- getVar id2
     if typesMatch val1 val2
       then setVar id2 val1 >> setVar id1 val2
       else throwError $ TypeError ("cannot swap '" ++ showValueType val1 ++
                                    "' and '" ++ showValueType val2 ++
                                    "' variables")
evalStmt Skip = return ()

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

evalString :: String -> IO ()
evalString str =
  case runEval (evalStmts e) store emptyProcEnv of
    Right (res, s) -> print res >> print s
    Left e         -> print e
  where e = parseStmtsString str
        store = fromList [("x", JInt 42)
                         ,("a", JArray [1,2,3,4,5])
                         ,("s", JStack [6,7,8,9,10])]

