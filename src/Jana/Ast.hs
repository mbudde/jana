
module Jana.Ast where

-- Data types
data Type = Int
          | Stack
          deriving (Eq, Show, Read)

-- Identifier
type Ident = String

-- Left-value
data Lval = Scalar Ident
          | Array  Ident Int
          deriving (Eq, Show, Read)

-- Modification operators used in assignment
data ModOp = PlusEq
           | MinusEq
           | XorEq
           deriving (Eq, Show, Read)

-- Operator
data Op = Plus          -- Arithmetic
        | Minus
        | Times
        | Divide
        | Modulo
        | And           -- Binary
        | Or
        | Xor
        | LAnd          -- Logical
        | LOr
        | GreaterThan   -- Relational
        | LessThan
        | Equal
        | GreaterEqual
        | LessEqual
        deriving (Eq, Show, Read)

data Statement = Assign   ModOp Lval Expr
               | IfElse   Expr [Statement] [Statement] Expr
               | FromLoop Expr [Statement] [Statement] Expr
               | Push     Ident Ident
               | Pop      Ident Ident
               | Local    (Type, Ident, Expr) [Statement] (Type, Ident, Expr)
               | Call     Ident [Ident]
               | Uncall   Ident [Ident]
               | Swap     Ident Ident
               | Skip
               deriving (Eq, Show, Read)

data Expr = NumConst Int
          | LV       Lval
          | BinOp    Op Expr Expr
          | Empty    Ident
          | Top      Ident
          | Nil
          deriving (Eq, Show, Read)

data Proc = Proc { procname  :: Ident
                 , params    :: [(Type, Ident)]   -- Zero or more
                 , body      :: [Statement] }
          deriving (Eq, Show, Read)

type Program = [Proc]
