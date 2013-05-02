module Jana.Eval (
  runProgram,
  evalLval,
  evalExpr,
  runEval,
  ) where


import Prelude hiding (GT, LT, EQ, userError)
import System.Exit
import Data.Char (toLower)
import Data.Map (fromList)
import Data.List (genericSplitAt, genericReplicate)
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Text.Printf (printf)

import Text.Parsec.Pos

import Jana.Ast
import Jana.Types
import Jana.Invert
import Jana.Error
import Jana.ErrorMessages
import Jana.Parser (parseExprString, parseStmtsString)

inArgument :: String -> String -> Eval a -> Eval a
inArgument funid argid monad = catchError monad $
  throwError . addErrorMessage (InArgument funid argid)

inExpression :: Expr -> Eval a -> Eval a
inExpression expr monad = catchError monad $
  throwError . addOnceErrorMessage (InExpression expr)

inStatement :: Stmt -> Eval a -> Eval a
inStatement stmt monad = catchError monad $
  \err -> do store <- get
             throwError $ addErrorMessage (InStatement stmt (showStore store)) err

inProcedure :: Proc -> Eval a -> Eval a
inProcedure proc monad = catchError monad $
  throwError . addErrorMessage (InProcedure $ ident proc)


unpackInt :: SourcePos -> Value -> Eval Integer
unpackInt _ (JInt x) = return x
unpackInt pos val = pos <!!> typeMismatch ["int"] (showValueType val)

unpackArray :: SourcePos -> Value -> Eval Array
unpackArray _ (JArray x) = return x
unpackArray pos val = pos <!!> typeMismatch ["array"] (showValueType val)

unpackStack :: SourcePos -> Value -> Eval Stack
unpackStack _ (JStack x) = return x
unpackStack pos val = pos <!!> typeMismatch ["stack"] (showValueType val)

unpackBool :: SourcePos -> Value -> Eval Bool
unpackBool _ (JBool x) = return x
unpackBool pos val = pos <!!> typeMismatch ["bool"] (showValueType val)

assert :: Bool -> Expr -> Eval ()
assert bool expr =
  do val1 <- unpackBool (getExprPos expr) =<< evalModularExpr expr
     unless (val1 == bool) $
       getExprPos expr <!!> assertionFail ("should be " ++ map toLower (show bool))

assertTrue = assert True
assertFalse = assert False

checkType :: Type -> Value -> Eval ()
checkType (Int pos)   (JInt _)   = return ()
checkType (Int pos)   (JArray _) = return ()
checkType (Stack pos) (JStack _) = return ()
checkType (Int pos)   val = pos <!!> typeMismatch ["int"] (showValueType val)
checkType (Stack pos) val = pos <!!> typeMismatch ["stack"] (showValueType val)

checkVdecl :: Vdecl -> Value -> Eval ()
checkVdecl (Scalar Int {}   _ _)  (JInt _)     = return ()
checkVdecl (Scalar Stack {} _ _)  (JStack _)   = return ()
checkVdecl (Array _ Nothing  _)   (JArray _)   = return ()
checkVdecl (Array _ (Just x) pos) (JArray arr) =
  unless (x == arrLen) $ pos <!!> arraySizeMismatch x arrLen
  where arrLen = toInteger (length arr)
checkVdecl vdecl val =
  vdeclPos vdecl <!!> typeMismatch [vdeclType vdecl] (showValueType val)
  where vdeclPos (Scalar _ _ pos) = pos
        vdeclPos (Array _ _ pos)  = pos
        vdeclType (Scalar Int{} _ _)   = "int"
        vdeclType (Scalar Stack{} _ _) = "stack"
        vdeclType (Array{})            = "array"


arrayLookup :: Array -> Integer -> SourcePos -> Eval Value
arrayLookup arr idx pos =
  if idx < 0 || idx' >= length arr
    then pos <!!> outOfBounds idx (toInteger $ length arr)
    else return $ JInt $ arr !! idx'
  where idx' = fromInteger idx

arrayModify :: Array -> Integer -> Integer -> Array
arrayModify arr idx val = xs ++ val : ys
  where (xs, _:ys) = genericSplitAt idx arr


getExprPos :: Expr -> SourcePos
getExprPos (Number _ pos)  = pos
getExprPos (Boolean _ pos) = pos
getExprPos (LV _ pos)      = pos
getExprPos (BinOp _ e1 _)  = getExprPos e1
getExprPos (Empty _ pos)   = pos
getExprPos (Top _ pos)     = pos
getExprPos (Nil pos)       = pos


runProgram :: String -> Program -> EvalOptions -> IO ()
runProgram _ (Program [main] procs) evalOptions =
  case procEnvFromList procs of
    Left err -> print err
    Right procEnv ->
      case runEval (evalMain main) emptyStore EE { procEnv = procEnv, evalOptions = evalOptions } of
        Right (_, s) -> putStrLn $ showStore s
        Left err     -> print err >> (exitWith $ ExitFailure 1)
runProgram filename (Program [] _) _ =
  print $ newFileError filename noMainProc
runProgram filename (Program _ _) _ =
  print $ newFileError filename multipleMainProcs


evalMain :: ProcMain -> Eval ()
evalMain proc@(ProcMain vdecls body pos) =
  do mapM_ initBinding vdecls
     evalStmts body
  where initBinding (Scalar (Int _) id _)   = bindVar id $ JInt 0
        initBinding (Scalar (Stack _) id _) = bindVar id nil
        initBinding (Array id Nothing pos)  = pos <!!> arraySizeMissing id
        initBinding (Array id (Just size) pos) =
          if size < 1
            then pos <!!> arraySize
            else bindVar id $ initArr size
        initArr size = JArray $ genericReplicate size 0

evalProc :: Proc -> [Ident] -> Eval ()
evalProc proc args =
  do values <- mapM getVar args
     oldStoreEnv <- get
     put emptyStore
     bindArgs (params proc) values (procPos proc)
     evalStmts $ body proc
     newValues <- mapM (getVar . getVdeclIdent) (params proc)
     put oldStoreEnv
     mapM_ (uncurry setVar) (zip args newValues)
  where bindArg :: Vdecl -> Value -> Eval ()
        bindArg vdecl val = inArgument (ident proc) (ident $ getVdeclIdent vdecl) $
          checkVdecl vdecl val >> setVar (getVdeclIdent vdecl) val
        bindArgs params values pos =
          if expArgs /= gotArgs
            then pos <!!> argumentError proc expArgs gotArgs
            else mapM_ (uncurry bindArg) (zip params values)
        expArgs = length (params proc)
        gotArgs = length args
        procPos Proc { procname = Ident _ pos } = pos
        getVdeclIdent (Scalar _ id _) = id
        getVdeclIdent (Array id _ _)  = id


assignLval :: ModOp -> Lval -> Expr -> SourcePos -> Eval ()
assignLval modOp (Var id) expr _ =
  do exprVal <- evalModularExpr expr
     varVal  <- getVar id
     performModOperation modOp varVal exprVal exprPos exprPos >>= setVar id
  where exprPos = getExprPos expr
assignLval modOp (Lookup id idxExpr) expr pos =
  do idx    <- unpackInt exprPos =<< evalModularExpr idxExpr
     arr    <- unpackArray pos =<< getVar id
     val    <- evalModularExpr expr
     oldval <- arrayLookup arr idx (getExprPos idxExpr)
     newval <- unpackInt pos =<< performModOperation modOp oldval val exprPos exprPos
     setVar id $ JArray $ arrayModify arr idx newval
  where exprPos = getExprPos expr

evalStmts :: [Stmt] -> Eval ()
evalStmts = mapM_ (\stmt -> inStatement stmt $ evalStmt stmt)

evalStmt :: Stmt -> Eval ()
evalStmt (Assign modOp lval expr pos) = assignLval modOp lval expr pos
evalStmt (If e1 s1 s2 e2 _) =
  do val1 <- unpackBool (getExprPos e1) =<< evalModularExpr e1 -- XXX: int required?
     if val1
       then do evalStmts s1
               assertTrue e2
       else do evalStmts s2
               assertFalse e2
evalStmt (From e1 s1 s2 e2 _) =
  do assertTrue e1
     evalStmts s1
     loop
  where loop = do val <- unpackBool (getExprPos e2) =<< evalModularExpr e2
                  unless val loopRec
        loopRec = do evalStmts s2
                     assertFalse e1
                     evalStmts s1
                     loop
evalStmt (Push id1 id2 pos) =
  do head <- unpackInt pos   =<< getVar id1
     tail <- unpackStack pos =<< getVar id2
     setVar id2 $ JStack $ head : tail
     setVar id1 $ JInt 0
evalStmt stmt@(Pop id1 id2 pos) =
  do head <- unpackInt pos   =<< getVar id1
     tail <- unpackStack pos =<< getVar id2
     if head /= 0
       then pos <!!> popToNonZero id1
       else case tail of
         (x:xs) -> setVar id1 (JInt x) >> setVar id2 (JStack xs)
         []     -> pos <!!> emptyStack
evalStmt (Local assign1 stmts assign2@(_, Ident _ pos, _) _) =
  do checkIdentAndType assign1 assign2
     createBinding assign1
     evalStmts stmts
     assertBinding assign2
  where createBinding (typ, id, expr) =
          do val <- evalModularExpr expr
             checkType typ val
             bindVar id val
        assertBinding (_, id, expr) =
          do val <- evalModularExpr expr
             val' <- getVar id
             unless (val == val') $
               pos <!!> wrongDelocalValue id (show val) (show val')
             unbindVar id
        checkIdentAndType (typ1, id1, _) (typ2, id2, _) =
          do unless (id1 == id2) $
               pos <!!> delocalNameMismatch id1 id2
             unless (typ1 == typ2) $
               pos <!!> delocalTypeMismatch id1 (show typ1) (show typ2)
evalStmt stmt@(Call funId args _) =
  do proc <- getProc funId
     evalProc proc args
evalStmt (Uncall funId args _) =
  do proc <- getProc funId
     evalProc (invertProc proc) args
evalStmt (Swap id1 id2 pos) =
  do val1 <- getVar id1
     val2 <- getVar id2
     if typesMatch val1 val2
       then setVar id2 val1 >> setVar id1 val2
       else pos <!!> swapTypeError (showValueType val1) (showValueType val2)
evalStmt (UserError msg pos) =
  pos <!!> userError msg

evalStmt (Skip _) = return ()

evalLval :: Lval -> Eval Value
evalLval (Var id) = getVar id
evalLval (Lookup id@(Ident _ pos) e) =
  do idx <- unpackInt (getExprPos e) =<< evalModularExpr e  -- FIXME: error
     arr <- unpackArray pos =<< getVar id
     arrayLookup arr idx pos

numberToModular :: Value -> Eval Value
numberToModular (JInt x) =
  do flag <- asks (modInt . evalOptions)
     return $ JInt $ if flag then ((x + 2^31) `mod` 2^32) - 2^31 else x
numberToModular val = return val

evalModularExpr :: Expr -> Eval Value
evalModularExpr expr = evalExpr expr >>= numberToModular

evalExpr :: Expr -> Eval Value
evalExpr (Number x _)       = return $ JInt x
evalExpr (Boolean b _)      = return $ JBool b
evalExpr (Nil _)            = return nil
evalExpr expr@(LV lval _)   = inExpression expr $ evalLval lval
evalExpr expr@(UnaryOp Not e) = inExpression expr $
  do x <- unpackBool (getExprPos e) =<< evalExpr e
     return $ JBool $ not x
evalExpr expr@(BinOp LAnd e1 e2) = inExpression expr $
  do x <- unpackBool (getExprPos e1) =<< evalExpr e1
     if x
       then liftM JBool (unpackBool (getExprPos e2) =<< evalExpr e2)
       else return $ JBool False
evalExpr expr@(BinOp LOr e1 e2) = inExpression expr $
  do x <- unpackBool (getExprPos e1) =<< evalExpr e1
     if x
       then return $ JBool True
       else liftM JBool $ unpackBool (getExprPos e2) =<< evalExpr e2
evalExpr expr@(BinOp op e1 e2) = inExpression expr $
  do x <- evalExpr e1
     y <- evalExpr e2
     performOperation op x y (getExprPos e1) (getExprPos e2)
evalExpr expr@(Top id pos) = inArgument "top" (ident id) $
  do stack <- unpackStack pos =<< getVar id
     case stack of
       (x:xs) -> return $ JInt x
       []     -> return nil
evalExpr expr@(Empty id pos) = inArgument "empty" (ident id) $
  do stack <- unpackStack pos =<< getVar id
     case stack of
       [] -> return $ JInt 1
       _  -> return $ JInt 0
evalExpr expr@(Size id@(Ident _ pos) _) = inArgument "size" (ident id) $
  do boxedVal <- getVar id
     case boxedVal of
       JArray xs -> return $ JInt (toInteger $ length xs)
       JStack xs -> return $ JInt (toInteger $ length xs)
       val       -> pos <!!> typeMismatch ["array", "stack"] (showValueType val)
