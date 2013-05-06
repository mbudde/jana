module Jana.Ast where

import Text.Parsec.Pos

-- Data types
data Type
    = Int SourcePos
    | Stack SourcePos
    | BoolT SourcePos

instance Eq Type where
  (Int _)   == (Int _)   = True
  (Stack _) == (Stack _) = True
  (BoolT _) == (BoolT _) = True
  _         == _         = False

-- Identifier
data Ident =
  Ident String SourcePos

instance Eq Ident where
  (Ident name1 _) == (Ident name2 _) = name1 == name2

-- Left-value
data Lval
    = Var    Ident
    | Lookup Ident Expr
    deriving (Eq)

-- Modification operators used in assignment
data ModOp
    = AddEq -- +=
    | SubEq -- -=
    | XorEq -- ^=
    deriving (Eq, Show)

data UnaryOp
    = Not
    deriving (Eq, Ord)

-- Binary operators
data BinOp
    = Add | Sub | Mul | Div | Mod     -- Arithmetic (+ - * / %)
    | And | Or | Xor                  -- Binary (& | ^)
    | LAnd | LOr                      -- Logical (&& ||)
    | GT | LT | EQ | NEQ | GE | LE    -- Relational (> < = != >= <=)
    deriving (Eq, Ord, Show)

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
    | UserError String SourcePos
    | Swap     Ident Ident SourcePos
    | Prints   Prints SourcePos
    | Skip SourcePos
    deriving (Eq)

-- Expression
data Expr
    = Number   Integer SourcePos
    | Boolean  Bool SourcePos
    | LV       Lval SourcePos
    | UnaryOp  UnaryOp Expr
    | BinOp    BinOp Expr Expr
    | Empty    Ident SourcePos
    | Top      Ident SourcePos
    | Size     Ident SourcePos
    | Nil      SourcePos
    deriving (Eq)

-- Declaration
data Vdecl
    = Scalar Type Ident SourcePos
    | Array  Ident (Maybe Integer) SourcePos
    deriving (Eq)

data Prints
    = Print String
    | Printf String [Ident]
    | Show [Ident]
    deriving (Eq)

-- Main procedure
data ProcMain
    = ProcMain [Vdecl] [Stmt] SourcePos
    deriving (Eq)

-- Procedure definition
data Proc
    = Proc { procname  :: Ident
           , params    :: [Vdecl]   -- Zero or more
           , body      :: [Stmt]
           }
    deriving (Eq)

data Program = Program [ProcMain] [Proc]


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
