module Jana.Eval (
  runProgram,
  evalLval,
  evalExpr,
  runEval,
  evalString,
  ) where


import Prelude hiding (GT, LT, EQ)
import Data.Map (fromList)
import Data.List (genericSplitAt, genericReplicate)
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Text.Printf (printf)

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

checkType :: Type -> Value -> Eval ()
checkType Int   (JInt _)   = return ()
checkType Stack (JStack _) = return ()
checkType typ val = throwError $ TypeMismatch
  (if typ == Int then "int" else "stack") (showValueType val)


arrayLookup :: Array -> Integer -> Eval Value
arrayLookup arr idx =
  if idx < 0 || idx' >= length arr
    then throwError OutOfBounds
    else return $ JInt $ arr !! idx'
  where idx' = fromInteger idx

arrayModify :: Array -> Integer -> Integer -> Array
arrayModify arr idx val = xs ++ val : ys
  where (xs, _:ys) = genericSplitAt idx arr



runProgram :: Program -> IO ()
runProgram (main, procs) =
  case runEval (evalMain main) emptyStore (procEnvFromList procs) of
    Right (_, s) -> putStrLn $ showStore s
    Left err     -> print err


evalMain :: ProcMain -> Eval ()
evalMain (ProcMain vdecls body) =
  do mapM_ initBinding vdecls
     evalStmts body
  where initBinding (Scalar Int id)   = bindVar id $ JInt 0
        initBinding (Scalar Stack id) = bindVar id nil
        initBinding (Array id size)   = bindVar id $ initArr size
        initArr size = JArray $ genericReplicate size 0

evalProc :: Proc -> [Ident] -> Eval ()
evalProc proc args =
  do values <- mapM getVar args
     oldStoreEnv <- get
     put emptyStore
     bindArgs (params proc) values
     evalStmts $ body proc
     newValues <- mapM getVar $ map snd (params proc)
     put oldStoreEnv
     mapM_ (uncurry setVar) (zip args newValues)
  where bindArg :: (Type, Ident) -> Value -> Eval ()
        bindArg (typ, id) val = checkType typ val >> setVar id val
        bindArgs params values =
          do if expArgs /= gotArgs
               then throwError $ ArgumentError (procname proc)
                                               expArgs gotArgs
               else mapM_ (uncurry bindArg) (zip params values)
        expArgs = length (params proc)
        gotArgs = length args


assignLval :: ModOp -> Lval -> Expr -> Eval ()
assignLval modOp (Var id) expr =
  do exprVal <- evalExpr expr
     varVal  <- getVar id
     performModOperation modOp varVal exprVal >>= setVar id
assignLval modOp (Lookup id idxExpr) expr =
  do idx    <- unpackInt =<< evalExpr idxExpr
     arr    <- unpackArray =<< getVar id
     val    <- evalExpr expr
     oldval <- arrayLookup arr idx
     newval <- unpackInt =<< performModOperation modOp oldval val
     setVar id $ JArray $ arrayModify arr idx newval

evalStmts :: [Stmt] -> Eval ()
evalStmts = mapM_ evalStmt

evalStmt :: Stmt -> Eval ()
evalStmt (Assign modOp lval expr) = assignLval modOp lval expr
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
evalStmt (Push id1 id2) =
  do head <- unpackInt   =<< getVar id1
     tail <- unpackStack =<< getVar id2
     setVar id2 $ JStack $ head : tail
     setVar id1 $ JInt 0
evalStmt (Pop id1 id2) =
  do head <- unpackInt   =<< getVar id1
     tail <- unpackStack =<< getVar id2
     if head /= 0
       then throwError $ AssertionFail $
              "cannot pop to non-zero variable '" ++ id1 ++ "'"
       else case tail of
         (x:xs) -> setVar id1 (JInt x) >> setVar id2 (JStack xs)
         []     -> throwError EmptyStack
evalStmt (Local assign1 stmts assign2) =
  do createBinding assign1
     evalStmts stmts
     assertBinding assign2
  where createBinding (typ, id, expr) =
          do val <- evalExpr expr
             checkType typ val
             bindVar id val
        assertBinding (_, id, expr) =   -- XXX: check type?
          do val <- evalExpr expr
             val' <- getVar id
             if val == val'
               then return ()
               else throwError $ AssertionFail $
                      printf "'%s' has the value '%s' not '%s'"
                             id (show val') (show val)
evalStmt (Call funId args) =
  do proc <- getProc funId
     evalProc proc args
evalStmt (Uncall funId args) =
  undefined
evalStmt (Swap id1 id2) =
  do val1 <- getVar id1
     val2 <- getVar id2
     if typesMatch val1 val2
       then setVar id2 val1 >> setVar id1 val2
       else throwError $ TypeError $
              printf "cannot swap '%s' and '%s' variables"
                     (showValueType val1) (showValueType val2)
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
  case runEval (evalStmts e) store procs of
    Right (res, s) -> print res >> print s
    Left e         -> print e
  where e = parseStmtsString str
        store = fromList [("x", JInt 42)
                         ,("y", JInt 66)
                         ,("z", JInt 0)
                         ,("a", JArray [1,2,3,4,5])
                         ,("s", JStack [6,7,8,9,10])]
        procs = fromList [("foo", Proc { procname = "foo"
                                       , params = [(Int, "x")]
                                       , body = [Assign AddEq (Var "x") (Number 5)] }) ]

