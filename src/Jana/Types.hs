module Jana.Types (
    Value(..), nil,
    JError(..),
    Store, emptyStore, bindIdent, lookupIdent,
    ProcEnv, emptyProcEnv, bindProc, lookupProc,
    ) where

import Data.List (intercalate)
import Control.Monad.Error
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

import Jana.Ast (Ident, Proc)


-- Types of values an expression can evaluate to.
data Value
    = JInt Integer
    | JArray [Integer]
    | JStack [Integer]
    deriving (Eq)

instance Show Value where
  show (JInt x)    = show x
  show (JArray xs) = show xs --"<array: " ++ show xs ++ ">"
  show (JStack []) = "nil"
  show (JStack xs) = "<" ++ intercalate "," (map show xs) ++ "]"

nil = JStack []

data JError
    = UnboundVar String       -- FIXME: Add source pos
    | TypeMismatch String String
    | UndefProc String
    | Unknown String

instance Show JError where
  show (UnboundVar name) = "variable not declared: " ++ name
  show (TypeMismatch expType foundType) = "expected type " ++ expType ++
                                          " but got " ++ foundType
  show (UndefProc name) = "function not defined: " ++ name
  show (Unknown s) = "unknown error: " ++ s

instance Error JError where
  noMsg  = Unknown "Unknown error"
  strMsg = Unknown


type Store = Map.Map Ident Value

emptyStore = Map.empty

bindIdent :: Ident -> Value -> Store -> Store
bindIdent id val store
    = if Map.notMember id store
        then Map.insert id val store
        else error $ "variable already declared"

lookupIdent :: Ident -> Store -> Value
lookupIdent id store
    = Maybe.fromMaybe (error $ "identifier not found")
                      (Map.lookup id store)


type ProcEnv = Map.Map Ident Proc

emptyProcEnv = Map.empty

bindProc :: Ident -> Proc -> ProcEnv -> ProcEnv
bindProc name proc env
    = if Map.notMember name env
        then Map.insert name proc env
        else error "function already defined"

lookupProc :: Ident -> ProcEnv -> Maybe Proc
lookupProc = Map.lookup
