{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jana.Types (
    Array, Stack,
    Value(..), nil, performOperation, performModOperation,
    showValueType, typesMatch, truthy,
    Store, printVdecl, showStore, emptyStore, bindVar, unbindVar, setVar, getVar,
    EvalEnv(..),
    EvalOptions(..), defaultOptions,
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

import Jana.Aliases
import Jana.Ast
import Jana.Error
import Jana.ErrorMessages

type Array = [Integer]
type Stack = [Integer]

-- Types of values an expression can evaluate to.
data Value
  = JInt Integer
  | JBool Bool
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
showValueType (JBool _)  = "bool"

typesMatch :: Value -> Value -> Bool
typesMatch (JInt _) (JInt _) = True
typesMatch (JArray _) (JArray _) = True
typesMatch (JStack _) (JStack _) = True
typesMatch (JBool _) (JBool _)   = True
typesMatch _ _ = False

nil = JStack []

truthy :: Value -> Bool
truthy (JInt 0)    = False
truthy (JStack []) = False
truthy _           = True


boolToInt :: Num a => (a -> a -> Bool) -> a -> a -> a
boolToInt f x y = if f x y then 1 else 0

wrap :: (a -> Value) -> (Integer -> Integer -> a) -> Integer -> Integer -> Value
wrap m f x y = m $ f x y


opFunc :: BinOp -> Integer -> Integer -> Value
opFunc Add  = wrap JInt (+)
opFunc Sub  = wrap JInt (-)
opFunc Mul  = wrap JInt (*)
opFunc Div  = wrap JInt div
opFunc Mod  = wrap JInt mod
opFunc And  = wrap JInt (.&.)
opFunc Or   = wrap JInt (.|.)
opFunc Xor  = wrap JInt xor
opFunc LAnd = undefined -- handled by evalExpr
opFunc LOr  = undefined -- handled by evalExpr
opFunc GT   = wrap JBool (>)
opFunc LT   = wrap JBool (<)
opFunc EQ   = wrap JBool (==)
opFunc NEQ  = wrap JBool (/=)
opFunc GE   = wrap JBool (>=)
opFunc LE   = wrap JBool (<=)

performOperation :: BinOp -> Value -> Value -> SourcePos -> SourcePos -> Eval Value
performOperation Div (JInt _) (JInt 0) _ pos =
  pos <!!> divisionByZero
performOperation op (JInt x) (JInt y) _ _ =
  return $ opFunc op x y
performOperation _ (JInt _) val _ pos =
  pos <!!> typeMismatch ["int"] (showValueType val)
performOperation _ val _ pos _ =
  pos <!!> typeMismatch ["int"] (showValueType val)

performModOperation :: ModOp -> Value -> Value -> SourcePos -> SourcePos -> Eval Value
performModOperation modOp = performOperation $ modOpToBinOp modOp
  where modOpToBinOp AddEq = Add
        modOpToBinOp SubEq = Sub
        modOpToBinOp XorEq = Xor

--
-- Environment
--

type Store = Map.Map String Value

printVdecl :: String -> Value -> String
printVdecl name val@(JArray xs) = printf "%s[%d] = %s" name (length xs) (show val)
printVdecl name val = printf "%s = %s" name (show val)

showStore :: Store -> String
showStore store = intercalate "\n" (map (uncurry printVdecl) (Map.toList store))

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

data EvalEnv = EE { evalOptions :: EvalOptions
                  , procEnv :: ProcEnv
                  , aliases :: AliasSet }

data EvalOptions = EvalOptions { modInt :: Bool, runReverse :: Bool }
defaultOptions   = EvalOptions { modInt = False, runReverse = False }

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
makeIdentList (Proc {params = params}) = map getVdeclIdent params
  where
    getVdeclIdent (Scalar _ id _) = id
    getVdeclIdent (Array id _ _)  = id

checkDuplicateArgs :: [Ident] -> Bool
checkDuplicateArgs []         = True
checkDuplicateArgs ([arg])    = True
checkDuplicateArgs (arg:args) =
  arg `notElem` args && checkDuplicateArgs args

getProc :: Ident -> Eval Proc
getProc (Ident funName pos) =      -- FIXME: calling main?
  do when (funName == "main") $ pos <!!> callingMainError
     procEnv <- asks procEnv
     case Map.lookup funName procEnv of
       Just proc -> return proc
       Nothing   -> pos <!!> undefProc funName


--
-- Evaluation
--

newtype Eval a = E { runE :: StateT Store (ReaderT EvalEnv (ErrorT JanaError IO)) a }
               deriving (Monad, MonadIO, MonadError JanaError, MonadReader EvalEnv, MonadState Store)

runEval :: Eval a -> Store -> EvalEnv -> IO (Either JanaError (a, Store))
runEval eval store procs = runErrorT (runReaderT (runStateT (runE eval) store) procs)

-- XXX: Implement monad instances manually?
{- instance Monad Eval Store where -}
    {- return = E . return -}
    {- e >>= f = S -}

{- throwJanaError :: (MonadError e m) => SourcePos -> Message -> m a -}
throwJanaError pos msg = throwError $ newErrorMessage pos msg

infixr 1 <!!>
pos <!!> msg = throwJanaError pos msg
