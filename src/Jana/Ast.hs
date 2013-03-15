
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
    = Var    Ident
    | Lookup Ident Expr
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
    | GT | LT | EQ | NEQ | GE | LE    -- Relational (> < = != >= <=)
    deriving (Eq, Show)

-- Statement
data Stmt
    = Assign   ModOp Lval Expr
    | If       Expr [Stmt] [Stmt] Expr
    | From     Expr [Stmt] [Stmt] Expr
    | Push     Ident Ident
    | Pop      Ident Ident
    | Local    (Type, Ident, Expr) [Stmt] (Type, Ident, Expr)
    | Call     Ident [Ident]
    | Uncall   Ident [Ident]
    | Swap     Ident Ident
    | Skip
    deriving (Eq, Show)

-- Expression
data Expr
    = Number   Integer
    | LV       Lval
    | BinOp    BinOp Expr Expr
    | Empty    Ident
    | Top      Ident
    | Nil
    deriving (Eq, Show)

-- Declaration
data Vdecl
    = Scalar Type Ident
    | Array  Ident Integer
    deriving (Eq, Show)

-- Main procedure
data ProcMain
    = ProcMain [Vdecl] [Stmt]
    deriving (Eq, Show)

-- Procedure definition
data Proc
    = Proc { procname  :: Ident
           , params    :: [(Type, Ident)]   -- Zero or more
           , body      :: [Stmt] }
    deriving (Eq, Show)

type Program = (ProcMain, [Proc])
