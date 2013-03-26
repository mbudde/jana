{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jana.Types (
    Array, Stack,
    Value(..), nil, performOperation, performModOperation,
    showValueType, typesMatch, truthy,
    JError(..),
    Store, showStore, emptyStore, bindVar, setVar, getVar,
    ProcEnv, emptyProcEnv, procEnvFromList, bindProc, getProc,
    Eval, runEval,
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

performOperation :: BinOp -> Value -> Value -> Eval Value
performOperation op (JInt x) (JInt y) =
  return $ JInt $ opFunc op x y
performOperation _ (JInt _) val =
  throwError $ TypeMismatch "int" (showValueType val)
performOperation _ val _ =
  throwError $ TypeMismatch "int" (showValueType val)

performModOperation :: ModOp -> Value -> Value -> Eval Value
performModOperation modOp = performOperation $ modOpToBinOp modOp
  where modOpToBinOp AddEq = Add
        modOpToBinOp SubEq = Sub
        modOpToBinOp XorEq = Xor

--
-- Environment
--

type Store = Map.Map Ident Value

showStore :: Store -> String
showStore store = intercalate "\n" (map printVdecl (Map.toList store))
  where printVdecl (Ident name _, val@(JArray xs)) = printf "%s[%d] = %s" name (length xs) (show val)
        printVdecl (Ident name _, val) = printf "%s = %s" name (show val)

emptyStore = Map.empty

envFromList :: [(Ident, Value)] -> Store
envFromList = Map.fromList

bindVar :: Ident -> Value -> Eval ()
bindVar id val =
  do storeEnv <- get
     case Map.lookup id storeEnv of
       Nothing  -> put $ Map.insert id val storeEnv
       Just val -> throwError $ AlreadyBound id

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

procEnvFromList :: [Proc] -> ProcEnv
procEnvFromList = Map.fromList . map (\p -> (procname p, p))

bindProc :: Ident -> Proc -> ProcEnv -> ProcEnv
bindProc name proc env
  = if Map.notMember name env
      then Map.insert name proc env
      else error "function already defined"

getProc :: Ident -> Eval Proc
getProc funId =      -- FIXME: calling main?
  do procEnv <- ask
     case Map.lookup funId procEnv of
       Just proc -> return proc
       Nothing   -> throwError $ UndefProc funId


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


data JError
  = UnboundVar    String SourcePos          -- ident
  | AlreadyBound  String SourcePos          -- ident
  | TypeError     String SourcePos          -- message
  | TypeMismatch  String String SourcePos   -- expected-type found-type
  | OutOfBounds   SourcePos
  | EmptyStack    SourcePos
  | AssertionFail String SourcePos
  | UndefProc     String SourcePos          -- ident
  | ArgumentError String Int Int SourcePos  -- proc-name expected got
  | ArraySize     SourcePos
  | Unknown       String SourcePos          -- message

instance Show JError where
  show (UnboundVar name pos) =
    jErrorMsg pos $ "variable not declared: " ++ name ++ "line: "
  show (AlreadyBound name pos) =
    jErrorMsg pos $ "variable name is already bound: " ++ name
  show (TypeError s pos)     = s
  show (TypeMismatch expType foundType pos) =
    jErrorMsg pos $ "expected type " ++ expType ++ " but got " ++ foundType
  show (OutOfBounds pos)     = jErrorMsg pos $ "array lookup was out of bounds"
  show (EmptyStack pos)      = jErrorMsg pos $ "cannot pop from empty stack"
  show (AssertionFail s pos) = jErrorMsg pos $ "assertion failed: " ++ s
  show (UndefProc name pos)  = jErrorMsg pos $ printf "procedure '%s' is not defined" name
  show (ArgumentError name exp got pos) =
   jErrorMsg pos $  printf "procedure '%s' expects %d argument(s) but got %d"
                           name exp got
  show (ArraySize pos) = jErrorMsg pos $ "array size must be greater than or equal to one"
  show (Unknown s pos) = jErrorMsg pos $ "unknown error: " ++ s

jErrorMsg :: SourcePos -> String -> String
jErrorMsg pos str = 
  printf "File \"%s\" in line %d, column %d:\n  %s" (sourceName pos) (sourceLine pos) (sourceColumn pos) str

instance Error JError where
  noMsg  = Unknown "Unknown error"
  strMsg = Unknown
