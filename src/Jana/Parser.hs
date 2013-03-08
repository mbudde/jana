module Jana.Parser where

import System.IO
import Control.Monad
import Control.Applicative((<*))
import Jana.Ast
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Combinator as Combinator
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

janaDef = T.LanguageDef {
                T.commentStart     = "(*"
              , T.commentEnd       = "*)"
              , T.commentLine      = "#"
              , T.nestedComments   = False
              , T.identStart       = letter
              , T.identLetter      = alphaNum
              , T.opStart          = oneOf "+-^*/%&|<>=!"
              , T.opLetter         = oneOf "&|="
              , T.reservedOpNames  = [ "+"
                                   , "-"
                                   , "^"
                                   , "*"
                                   , "/"
                                   , "%"
                                   , "&"
                                   , "|"
                                   , "&&"
                                   , "||"
                                   , "<"
                                   , ">"
                                   , "="
                                   , "!="
                                   , "<="
                                   , ">="
                                   ]
              , T.reservedNames    = [ "procedure"
                                   , "int"
                                   , "stack"
                                   , "if"
                                   , "then"
                                   , "else"
                                   , "fi"
                                   , "from"
                                   , "do"
                                   , "loop"
                                   , "until"
                                   , "push"
                                   , "pop"
                                   , "local"
                                   , "delocal"
                                   , "call"
                                   , "uncall"
                                   , "skip"
                                   , "empty"
                                   , "top"
                                   , "nil"
                                   ]
              , T.caseSensitive    = True
  }

lexer = T.makeTokenParser janaDef

identifier = T.identifier lexer -- parses an identifier
reserved   = T.reserved   lexer -- parses a reserved name
reservedOp = T.reservedOp lexer -- parses an operator
parens     = T.parens     lexer -- parses surrounding parenthesis:
                                -- parens p
                                -- takes care of the parenthesis and
                                -- uses p to parse what's inside them
brackets   = T.brackets   lexer -- parses brackets
integer    = T.integer    lexer -- parses an integer
semi       = T.semi       lexer -- parses a semicolon
comma      = T.comma      lexer -- parses a comma
whiteSpace = T.whiteSpace lexer -- parses whitespace

{- janaParser :: Parser Proc -}
{- janaParser = whiteSpace >> procedure -}

{- procedure :: Parser Proc -}
{- procedure =  -}
  {- do reserved "procedure" -}
     {- name   <- identifier -}
     {- params <- sepBy parameter comma -- TODO: Write parameter function -}
     {- stats  <- many statement        -- TODO: Write statement function -}
     {- return $ Proc { procname = name, params = params, body = stats } -}

statement :: Parser Stmt
statement =   assignStmt
          <|> ifStmt
          <|> fromStmt
          <|> pushStmt
          <|> popStmt
          <|> localStmt
          <|> callStmt
          <|> uncallStmt
          <|> swapStmt
          <|> skipStmt

assignStmt = undefined
ifStmt     = undefined
fromStmt   = undefined
pushStmt   = undefined
popStmt    = undefined
localStmt  = undefined
callStmt   = undefined
uncallStmt = undefined
swapStmt   = undefined
skipStmt   = undefined

expression :: Parser Expr
expression =  buildExpressionParser binOperators term

term :: Parser Expr
term =   parens expression
     <|> numberExpr
     <|> lvalExpr
     <|> emptyExpr
     <|> topExpr
     <|> return Nil

numberExpr :: Parser Expr
numberExpr = liftM Number integer

lvalExpr ::  Parser Expr
lvalExpr = liftM LV lval

lval ::  Parser Lval
lval =   try lookUp
     <|> liftM Var identifier

lookUp :: Parser Lval
lookUp =
  do ident <- identifier
     expr  <- brackets expression
     return $ Lookup ident expr

emptyExpr :: Parser Expr
emptyExpr = reserved "empty" >>
            parens identifier >>= return . Jana.Ast.Empty

topExpr :: Parser Expr
topExpr =  reserved "top" >>
           parens identifier >>= return . Jana.Ast.Top

binOperators = [ [Infix (reservedOp "+"   >> return (BinOp Add )) AssocLeft]
               , [Infix (reservedOp "-"   >> return (BinOp Sub )) AssocLeft]
               , [Infix (reservedOp "*"   >> return (BinOp Mul )) AssocLeft]
               , [Infix (reservedOp "/"   >> return (BinOp Div )) AssocLeft]
               , [Infix (reservedOp "%"   >> return (BinOp Mod )) AssocLeft]
               , [Infix (reservedOp "&"   >> return (BinOp And )) AssocLeft]
               , [Infix (reservedOp "|"   >> return (BinOp Or  )) AssocLeft]
               , [Infix (reservedOp "^"   >> return (BinOp Xor )) AssocLeft]
               , [Infix (reservedOp "&&"  >> return (BinOp LAnd)) AssocLeft]
               , [Infix (reservedOp "||"  >> return (BinOp LOr )) AssocLeft]
               , [Infix (reservedOp ">"   >> return (BinOp Jana.Ast.GT)) AssocLeft]
               , [Infix (reservedOp "<"   >> return (BinOp Jana.Ast.LT)) AssocLeft]
               , [Infix (reservedOp "="   >> return (BinOp Jana.Ast.EQ)) AssocLeft]
               , [Infix (reservedOp "!="  >> return (BinOp NEQ )) AssocLeft]
               , [Infix (reservedOp ">="  >> return (BinOp GE  )) AssocLeft]
               , [Infix (reservedOp "<="  >> return (BinOp LE  )) AssocLeft]
               ]


parseString :: String -> Expr
parseString str =
  case parse expression "" str of
    Left e  -> error $ show e
    Right r -> r
