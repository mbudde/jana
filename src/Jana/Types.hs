{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jana.Types (
    Array, Stack,
    Value(..), nil, performOperation, performModOperation,
    showValueType, typesMatch, truthy,
    Store, showStore, emptyStore, bindVar, unbindVar, setVar, getVar,
    ProcEnv, emptyProcEnv, procEnvFromList, getProc,
    Eval, runEval, (<!!>)
    ) where

import Prelude hiding (GT, LT, EQ)
import Data.Bits
import Data.List (intercalate)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Text.Printf (printf)
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

import Text.Parsec.Pos

import Jana.Ast
import Jana.Error
import Jana.ErrorMessages

type Array = [Integer]
type Stack = [Integer]

-- Types of values an expression can evaluate to.
data Value
  = JInt Integer
  | JArray Array
  | JStack Stack
  deriving (Eq)

instance Show Value where
  show (JInt x)    = show x
  show (JArray xs) = "{" ++ intercalate ", " (map show xs) ++ "}"
  show (JStack []) = "nil"
  show (JStack xs) = "<" ++ intercalate ", " (map show xs) ++ "]"

showValueType :: Value -> String
showValueType (JInt _) = "int"
showValueType (JStack _) = "stack"
showValueType (JArray _) = "array"

typesMatch :: Value -> Value -> Bool
typesMatch (JInt _) (JInt _) = True
typesMatch (JArray _) (JArray _) = True
typesMatch (JStack _) (JStack _) = True
typesMatch _ _ = False

nil = JStack []

truthy :: Value -> Bool
truthy (JInt 0)    = False
truthy (JStack []) = False
truthy _           = True


boolToInt :: Num a => (a -> a -> Bool) -> a -> a -> a
boolToInt f x y = if f x y then 1 else 0


opFunc :: (Integral a, Bits a) => BinOp -> a -> a -> a
opFunc Add  = (+)
opFunc Sub  = (-)
opFunc Mul  = (*)
opFunc Div  = div
opFunc Mod  = mod
opFunc And  = (.&.)
opFunc Or   = (.|.)
opFunc Xor  = xor
opFunc LAnd = undefined -- handled by evalExpr
opFunc LOr  = undefined -- handled by evalExpr
opFunc GT   = boolToInt (>)
opFunc LT   = boolToInt (<)
opFunc EQ   = boolToInt (==)
opFunc NEQ  = boolToInt (/=)
opFunc GE   = boolToInt (>=)
opFunc LE   = boolToInt (<=)

performOperation :: BinOp -> Value -> Value -> SourcePos -> SourcePos -> Eval Value
performOperation Div (JInt _) (JInt 0) _ pos =
  pos <!!> divisionByZero
performOperation op (JInt x) (JInt y) _ _ =
  return $ JInt $ opFunc op x y
performOperation _ (JInt _) val _ pos =
  pos <!!> typeMismatch "int" (showValueType val)
performOperation _ val _ pos _ =
  pos <!!> typeMismatch "int" (showValueType val)

performModOperation :: ModOp -> Value -> Value -> SourcePos -> SourcePos -> Eval Value
performModOperation modOp = performOperation $ modOpToBinOp modOp
  where modOpToBinOp AddEq = Add
        modOpToBinOp SubEq = Sub
        modOpToBinOp XorEq = Xor

--
-- Environment
--

type Store = Map.Map String Value

showStore :: Store -> String
showStore store = intercalate "\n" (map printVdecl (Map.toList store))
  where printVdecl (name, val@(JArray xs)) = printf "%s[%d] = %s" name (length xs) (show val)
        printVdecl (name, val) = printf "%s = %s" name (show val)

emptyStore = Map.empty

envFromList :: [(String, Value)] -> Store
envFromList = Map.fromList

bindVar :: Ident -> Value -> Eval ()
bindVar (Ident name pos) val =
  do storeEnv <- get
     case Map.lookup name storeEnv of
       Nothing  -> put $ Map.insert name val storeEnv
       Just _ -> pos <!!> alreadyBound name

unbindVar :: Ident -> Eval ()
unbindVar = modify . Map.delete . ident


setVar :: Ident -> Value -> Eval ()
setVar (Ident name _) val = modify $ Map.insert name val


getVar :: Ident -> Eval Value
getVar (Ident name pos) =
  do storeEnv <- get
     case Map.lookup name storeEnv of
       Just val -> return val
       Nothing  -> pos <!!> unboundVar name


type ProcEnv = Map.Map String Proc

emptyProcEnv = Map.empty

procEnvFromList :: [Proc] -> Either JanaError ProcEnv
procEnvFromList = foldM insertProc emptyProcEnv
  where insertProc env p = if Map.notMember (ident p) env
                             then if checkDuplicateArgs (makeIdentList p)
                                    then Right (Map.insert (ident p) p env)
                                    else Left $ newErrorMessage (ppos p) (procDuplicateArgs p)
                             else Left  $ newErrorMessage (ppos p) (procDefined p)
        ppos  Proc { procname = (Ident _ pos) } = pos

makeIdentList :: Proc -> [Ident]
makeIdentList (Proc {params = params}) = map paramToIdent params
  where
    paramToIdent (_, ident) = ident

checkDuplicateArgs :: [Ident] -> Bool
checkDuplicateArgs []         = True
checkDuplicateArgs ([arg])    = True
checkDuplicateArgs (arg:args) =
  not (elem arg args) && checkDuplicateArgs args

getProc :: Ident -> Eval Proc
getProc (Ident funName pos) =      -- FIXME: calling main?
  do procEnv <- ask
     case Map.lookup funName procEnv of
       Just proc -> return proc
       Nothing   -> pos <!!> undefProc funName


--
-- Evaluation
--

newtype Eval a = E { runE :: StateT Store (ReaderT ProcEnv (Either JanaError)) a }
               deriving (Monad, MonadError JanaError, MonadReader ProcEnv, MonadState Store)

runEval :: Eval a -> Store -> ProcEnv -> Either JanaError (a, Store)
runEval eval store procs = runReaderT (runStateT (runE eval) store) procs

-- XXX: Implement monad instances manually?
{- instance Monad Eval Store where -}
    {- return = E . return -}
    {- e >>= f = S -}

{- throwJanaError :: (MonadError e m) => SourcePos -> Message -> m a -}
throwJanaError pos msg = throwError $ newErrorMessage pos msg

infixr 1 <!!>
pos <!!> msg = throwJanaError pos msg
