
module Jana.Ast where

-- Data types
data Type
    = Int
    | Stack
    deriving (Eq, Show)

-- Identifier
type Ident = String

-- Left-value
data Lval
    = Scalar Ident
    | Array  Ident Int
    deriving (Eq, Show)

-- Modification operators used in assignment
data ModOp
    = AddEq -- +=
    | SubEq -- -=
    | XorEq -- ^=
    deriving (Eq, Show)

-- Binary operators
data BinOp
    = Add | Sub | Mul | Div | Mod     -- Arithmetic (+ - * / %)
    | And | Or | Xor                  -- Binary (& | ^)
    | LAnd | LOr                      -- Logical (&& ||)
    | Gt | Lt | Eq | GE | LE          -- Relational (> < = >= <=)
    deriving (Eq, Show)

-- Statement
data Stat
    = Assign   ModOp Lval Expr
    | If       Expr [Stat] [Stat] Expr
    | From     Expr [Stat] [Stat] Expr
    | Push     Ident Ident
    | Pop      Ident Ident
    | Local    (Type, Ident, Expr) [Stat] (Type, Ident, Expr)
    | Call     Ident [Ident]
    | Uncall   Ident [Ident]
    | Swap     Ident Ident
    | Skip
    deriving (Eq, Show)

-- Expression
data Expr
    = Number   Int
    | LV       Lval
    | BinOp    BinOp Expr Expr
    | Empty    Ident
    | Top      Ident
    | Nil
    deriving (Eq, Show)

-- Procedure
data Proc
    = Proc { procname  :: Ident
           , params    :: [(Type, Ident)]   -- Zero or more
           , body      :: [Stat] }
    deriving (Eq, Show)

type Program = [Proc]
