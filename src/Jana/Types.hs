{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jana.Types (
    Array, Stack,
    Value(..), nil, performOperation, showValueType,
    JError(..),
    Store, emptyStore, setVar, getVar,
    ProcEnv, emptyProcEnv, bindProc, lookupProc,
    Eval, runEval,
    ) where

import Prelude hiding (GT, LT, EQ)
import Data.Bits
import Data.List (intercalate)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

import Jana.Ast

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
  show (JArray xs) = show xs --"<array: " ++ show xs ++ ">"
  show (JStack []) = "nil"
  show (JStack xs) = "<" ++ intercalate "," (map show xs) ++ "]"

showValueType :: Value -> String
showValueType (JInt _) = "int"
showValueType (JStack _) = "stack"
showValueType (JArray _) = "array"

nil = JStack []


boolToInt :: Num a => (a -> a -> Bool) -> a -> a -> a
boolToInt f x y = if f x y then 1 else 0


getOperatorFunc :: (Integral a, Bits a) => BinOp -> a -> a -> a
getOperatorFunc Add  = (+)
getOperatorFunc Sub  = (-)
getOperatorFunc Mul  = (*)
getOperatorFunc Div  = undefined  -- FIXME
getOperatorFunc Mod  = mod
getOperatorFunc And  = (.&.)
getOperatorFunc Or   = (.|.)
getOperatorFunc Xor  = xor
getOperatorFunc LAnd = undefined
getOperatorFunc LOr  = undefined
getOperatorFunc GT   = boolToInt (>)
getOperatorFunc LT   = boolToInt (<)
getOperatorFunc EQ   = boolToInt (==)
getOperatorFunc NEQ  = boolToInt (/=)
getOperatorFunc GE   = boolToInt (>=)
getOperatorFunc LE   = boolToInt (<=)


performOperation :: BinOp -> Value -> Value -> Eval Value
performOperation op (JInt x) (JInt y) =
  return $ JInt $ getOperatorFunc op x y
performOperation _ (JStack _) val =
  throwError $ TypeMismatch "int" (showValueType val)
performOperation _ val _ =
  throwError $ TypeMismatch "int" (showValueType val)


--
-- Environment
--

type Store = Map.Map Ident Value

emptyStore = Map.empty

setVar :: Ident -> Value -> Eval ()
setVar id val = modify $ Map.insert id val

getVar :: Ident -> Eval Value
getVar id =
  do storeEnv <- get
     case Map.lookup id storeEnv of
       Just val -> return val
       Nothing  -> throwError $ UnboundVar id


type ProcEnv = Map.Map Ident Proc

emptyProcEnv = Map.empty

bindProc :: Ident -> Proc -> ProcEnv -> ProcEnv
bindProc name proc env
  = if Map.notMember name env
      then Map.insert name proc env
      else error "function already defined"

lookupProc :: Ident -> ProcEnv -> Maybe Proc
lookupProc = Map.lookup


--
-- Evaluation
--

newtype Eval a = E { runE :: StateT Store (ReaderT ProcEnv (Either JError)) a }
               deriving (Monad, MonadError JError, MonadReader ProcEnv, MonadState Store)

runEval :: Eval a -> Store -> ProcEnv -> Either JError (a, Store)
runEval eval store procs = runReaderT (runStateT (runE eval) store) procs

-- XXX: Implement monad instances manually?
{- instance Monad Eval Store where -}
    {- return = E . return -}
    {- e >>= f = S -}


data JError       -- FIXME: Add source pos
  = UnboundVar   String           -- ident
  | TypeMismatch String String    -- expected-type found-type
  | OutOfBounds
  | UndefProc    String           -- ident
  | Unknown      String           -- message

instance Show JError where
  show (UnboundVar name) = "variable not declared: " ++ name
  show (TypeMismatch expType foundType) = "expected type " ++ expType ++
                                          " but got " ++ foundType
  show (OutOfBounds) = "array lookup was out of bounds"
  show (UndefProc name) = "function not defined: " ++ name
  show (Unknown s) = "unknown error: " ++ s

instance Error JError where
  noMsg  = Unknown "Unknown error"
  strMsg = Unknown
