module Jana.Ast where

import Text.Parsec.Pos

-- Data types
data Type
    = Int SourcePos
    | Stack SourcePos
    deriving (Eq, Show)

-- Identifier
data Ident =
  Ident String SourcePos
  deriving (Eq, Show)

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
    = Assign   ModOp Lval Expr SourcePos
    | If       Expr [Stmt] [Stmt] Expr SourcePos
    | From     Expr [Stmt] [Stmt] Expr SourcePos
    | Push     Ident Ident SourcePos
    | Pop      Ident Ident SourcePos
    | Local    (Type, Ident, Expr) [Stmt] (Type, Ident, Expr) SourcePos
    | Call     Ident [Ident] SourcePos
    | Uncall   Ident [Ident] SourcePos
    | Swap     Ident Ident SourcePos
    | Skip SourcePos
    deriving (Eq, Show)

-- Expression
data Expr
    = Number   Integer SourcePos
    | LV       Lval SourcePos
    | BinOp    BinOp Expr Expr
    | Empty    Ident SourcePos
    | Top      Ident SourcePos
    | Nil SourcePos
    deriving (Eq, Show)

-- Declaration
data Vdecl
    = Scalar Type Ident SourcePos
    | Array  Ident Integer SourcePos
    deriving (Eq, Show)

-- Main procedure
data ProcMain
    = ProcMain [Vdecl] [Stmt] SourcePos
    deriving (Eq, Show)

-- Procedure definition
data Proc
    = Proc { procname  :: Ident
           , params    :: [(Type, Ident)]   -- Zero or more
           , body      :: [Stmt]
           }
    deriving (Eq, Show)

type Program = (ProcMain, [Proc])


class Identifiable a where
  ident :: a -> String

instance Identifiable Ident where
  ident (Ident id _) = id

instance Identifiable Lval where
  ident (Var id) = ident id
  ident (Lookup id _) = ident id

instance Identifiable ProcMain where
  ident _ = "main"

instance Identifiable Proc where
  ident proc = ident $ procname proc
