module Jana.Parser where

import Prelude hiding (GT, LT, EQ)
import System.IO
import Control.Monad
import Control.Applicative((<*))

import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Combinator as Combinator
import qualified Text.Parsec.Token as Token

import Jana.Ast

janaDef = Token.LanguageDef {
                Token.commentStart     = "(*"
              , Token.commentEnd       = "*)"
              , Token.commentLine      = "#"
              , Token.nestedComments   = False
              , Token.identStart       = letter
              , Token.identLetter      = alphaNum
              , Token.opStart          = oneOf "+-^*/%&|<>=!"
              , Token.opLetter         = oneOf "&|="
              , Token.reservedOpNames  = [ "+"
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
              , Token.reservedNames    = [ "procedure"
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
              , Token.caseSensitive    = True
  }

lexer = Token.makeTokenParser janaDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                -- parens p
                                -- takes care of the parenthesis and
                                -- uses p to parse what's inside them
brackets   = Token.brackets   lexer -- parses brackets
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
comma      = Token.comma      lexer -- parses a comma
whiteSpace = Token.whiteSpace lexer -- parses whitespace

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
lookUp = liftM2 Lookup identifier (brackets expression)

emptyExpr :: Parser Expr
emptyExpr = reserved "empty" >> liftM Empty (parens identifier)

topExpr :: Parser Expr
topExpr =  reserved "top" >> liftM Top (parens identifier)

binOperators = [ [ Infix (reservedOp "*"   >> return (BinOp Mul )) AssocLeft
                 , Infix (reservedOp "/"   >> return (BinOp Div )) AssocLeft
                 , Infix (reservedOp "%"   >> return (BinOp Mod )) AssocLeft ]
               , [ Infix (reservedOp "+"   >> return (BinOp Add )) AssocLeft
                 , Infix (reservedOp "-"   >> return (BinOp Sub )) AssocLeft ]
               , [ Infix (reservedOp "<"   >> return (BinOp LT  )) AssocLeft
                 , Infix (reservedOp "<="  >> return (BinOp LE  )) AssocLeft
                 , Infix (reservedOp ">"   >> return (BinOp GT  )) AssocLeft
                 , Infix (reservedOp ">="  >> return (BinOp GE  )) AssocLeft
                 , Infix (reservedOp "="   >> return (BinOp EQ  )) AssocLeft
                 , Infix (reservedOp "!="  >> return (BinOp NEQ )) AssocLeft ]
               , [ Infix (reservedOp "&"   >> return (BinOp And )) AssocLeft
                 , Infix (reservedOp "^"   >> return (BinOp Xor )) AssocLeft
                 , Infix (reservedOp "|"   >> return (BinOp Or  )) AssocLeft ]
               , [ Infix (reservedOp "&&"  >> return (BinOp LAnd)) AssocLeft
                 , Infix (reservedOp "||"  >> return (BinOp LOr )) AssocLeft ]
               ]


parseString :: String -> Expr
parseString str =
  case parse expression "" str of
    Left e  -> error $ show e
    Right r -> r
