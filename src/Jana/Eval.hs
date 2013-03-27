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

import Text.Parsec.Pos

import Jana.Ast
import Jana.Types
import Jana.Invert
import Jana.Parser (parseExprString, parseStmtsString)


unpackInt :: SourcePos -> Value -> Eval Integer
unpackInt _ (JInt x) = return x
unpackInt pos val = throwError $ TypeMismatch "int" (showValueType val) pos

unpackArray :: SourcePos -> Value -> Eval Array
unpackArray _ (JArray x) = return x
unpackArray pos val = throwError $ TypeMismatch "array" (showValueType val) pos

unpackStack :: SourcePos -> Value -> Eval Stack
unpackStack _ (JStack x) = return x
unpackStack pos val = throwError $ TypeMismatch "stack" (showValueType val) pos

assert :: Bool -> Expr -> Eval ()
assert bool expr =
  do val1 <- evalExpr expr
     unless (truthy val1 == bool) $
       throwError $ AssertionFail ("should be " ++ show bool) $ getExprPos expr

assertTrue = assert True
assertFalse = assert False

checkType :: Type -> Value -> Eval ()
checkType (Int pos)   (JInt _)   = return ()
checkType (Int pos)   (JArray _) = return ()
checkType (Stack pos) (JStack _) = return ()
checkType (Int pos)   val = throwError $ TypeMismatch "int" (showValueType val) pos
checkType (Stack pos) val = throwError $ TypeMismatch "stack" (showValueType val) pos


arrayLookup :: Array -> Integer -> SourcePos -> Eval Value
arrayLookup arr idx pos =
  if idx < 0 || idx' >= length arr
    then throwError $ OutOfBounds pos
    else return $ JInt $ arr !! idx'
  where idx' = fromInteger idx

arrayModify :: Array -> Integer -> Integer -> Array
arrayModify arr idx val = xs ++ val : ys
  where (xs, _:ys) = genericSplitAt idx arr


getExprPos :: Expr -> SourcePos
getExprPos (Number _ pos) = pos
getExprPos (LV _ pos)     = pos
getExprPos (BinOp _ e1 _) = getExprPos e1
getExprPos (Empty _ pos)  = pos
getExprPos (Top _ pos)    = pos
getExprPos (Nil pos)      = pos


runProgram :: Program -> IO ()
runProgram (main, procs) =
  case runEval (evalMain main) emptyStore (procEnvFromList procs) of
    Right (_, s) -> putStrLn $ showStore s
    Left err     -> print err


evalMain :: ProcMain -> Eval ()
evalMain (ProcMain vdecls body pos) =
  do mapM_ initBinding vdecls
     evalStmts body
  where initBinding (Scalar (Int _) id _)   = bindVar id $ JInt 0
        initBinding (Scalar (Stack _) id _) = bindVar id nil
        initBinding (Array id size pos)     = if size < 1
                                                then throwError $ ArraySize pos
                                                else bindVar id $ initArr size
        initArr size = JArray $ genericReplicate size 0

evalProc :: Proc -> [Ident] -> Eval ()
evalProc proc args =
  do values <- mapM getVar args
     oldStoreEnv <- get
     put emptyStore
     bindArgs (params proc) values 
     evalStmts $ body proc
     newValues <- mapM (getVar . snd) (params proc)
     put oldStoreEnv
     mapM_ (uncurry setVar) (zip args newValues)
  where bindArg :: (Type, Ident) -> Value -> Eval ()
        bindArg (typ, id) val = checkType typ val >> setVar id val
        bindArgs params values pos =
          if expArgs /= gotArgs
            then throwError $ ArgumentError (ident $ procname proc)
                                            expArgs gotArgs pos
            else mapM_ (uncurry bindArg) (zip params values)
        expArgs = length (params proc)
        gotArgs = length args


assignLval :: ModOp -> Lval -> Expr -> SourcePos -> Eval ()
assignLval modOp (Var id) expr pos =
  do exprVal <- evalExpr expr
     varVal  <- getVar id
     performModOperation modOp varVal exprVal pos >>= setVar id
assignLval modOp (Lookup id idxExpr) expr pos =
  do idx    <- unpackInt =<< evalExpr idxExpr
     arr    <- unpackArray =<< getVar id
     val    <- evalExpr expr
     oldval <- arrayLookup arr idx (getExprPos idxExpr)
     newval <- unpackInt =<< performModOperation modOp oldval val pos
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
                  unless (truthy val) loopRec
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
             unless (val == val') $
               throwError $ AssertionFail $
                 printf "'%s' has the value '%s' not '%s'"
                        id (show val') (show val)
evalStmt (Call funId args) =
  do proc <- getProc funId
     evalProc proc args
evalStmt (Uncall funId args) =
  do proc <- getProc funId
     evalProc (invertProc proc) args
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
evalExpr (BinOp LAnd e1 e2) =
  do x <- evalExpr e1
     if truthy x
       then do y <- evalExpr e2
               if truthy y then return $ JInt 1
                           else return $ JInt 0
       else return $ JInt 0
evalExpr (BinOp LOr e1 e2) =
  do x <- evalExpr e1
     if truthy x
       then return $ JInt 1
       else do y <- evalExpr e2
               if truthy y then return $ JInt 1
                           else return $ JInt 0
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
