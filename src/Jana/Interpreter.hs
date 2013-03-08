module Jana.Interpreter where

import Prelude hiding (GT, LT, EQ)
import Data.Bits
import Control.Monad.State
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

import Jana.Ast


data Value
    = VInt Int
    | VArray [Int]
    | VNil
    deriving (Eq)

instance Show Value where
    show (VInt x)    = show x
    show (VArray xs) = show xs
    show VNil        = "nil"

{- data JanaError -}


type Store = Map.Map Ident Value

bindIdent :: Store -> Ident -> Value -> Store
bindIdent store id val
    = if Map.notMember id store
        then Map.insert id val store
        else error $ "variable already declared"

lookupIdent :: Store -> Ident -> Value
lookupIdent store id
    = Maybe.fromMaybe (error $ "identifier not found")
                      (Map.lookup id store)


type ProcEnv = Map.Map Ident Proc

bindProc :: ProcEnv -> Ident -> Proc -> ProcEnv
bindProc env name proc
    = if Map.notMember name env
        then Map.insert name proc env
        else error "function already defined"


data EvalEnv
    = EvalEnv
        { store :: Store
        , procs :: ProcEnv
        , main  :: ProcMain }
    deriving (Eq, Show)

initEvalEnv =
    EvalEnv { store = Map.empty
            , procs = Map.empty
            , main  = ProcMain [] [] }

type EvalComp a = State EvalEnv a


boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0


performOperation :: BinOp -> Int -> Int -> Int
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
performOperation GT   = \x y -> boolToInt $ x > y
performOperation LT   = \x y -> boolToInt $ x < y
performOperation EQ   = \x y -> boolToInt $ x == y
performOperation NEQ  = \x y -> boolToInt $ x /= y
performOperation GE   = \x y -> boolToInt $ x >= y
performOperation LE   = \x y -> boolToInt $ x <= y


evalLval :: Lval -> EvalComp Value
evalLval (Var id) = do
    storeEnv <- gets store
    return $ lookupIdent storeEnv id
evalLval (Lookup id e) = do
    storeEnv <- gets store
    VInt idx <- evalExpr e  -- FIXME: error
    let VArray arr = lookupIdent storeEnv id in
        if length arr <= idx || idx < 0
            then error "out of bounds"
            else return $ VInt (arr !! idx)


evalExpr :: Expr -> EvalComp Value
evalExpr (Number x) = return $ VInt x
evalExpr (LV lval) = evalLval lval
evalExpr (BinOp op e1 e2) = do
    VInt val1 <- evalExpr e1  -- FIXME: error when non-int
    VInt val2 <- evalExpr e2
    return $ VInt $ performOperation op val1 val2
evalExpr Nil = return VNil
