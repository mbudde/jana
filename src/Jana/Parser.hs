module Jana.Parser where

import Prelude hiding (GT, LT, EQ)
import System.IO
import Control.Monad
import Control.Applicative((<*))
import Data.Either
import Data.Maybe

import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token      as Token

import Jana.Ast

janaDef = Token.LanguageDef {
                Token.commentStart     = "(*"
              , Token.commentEnd       = "*)"
              , Token.commentLine      = "#"
              , Token.nestedComments   = False
              , Token.identStart       = letter
              , Token.identLetter      = alphaNum
              , Token.opStart          = oneOf "" -- oneOf "+-^*/%&|<>=!"
              , Token.opLetter         = oneOf "" -- oneOf "&|="
              , Token.reservedOpNames  = [{- "+"
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
                                         , ">=" -}
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

program :: Parser Program
program =
  do whiteSpace
     ([main], procs) <- liftM partitionEithers (many genProcedure)
     return $ (main, procs)

genProcedure :: Parser (Either ProcMain Proc)
genProcedure =  try (mainProcedure >>= return . Left)
            <|> (procedure     >>= return . Right)

mainProcedure :: Parser ProcMain
mainProcedure =
  do reserved "procedure"
     reserved "main"
     parens whiteSpace
     vdecls <- many vdecl
     stats  <- many statement
     return $ ProcMain vdecls stats
     
vdecl :: Parser Vdecl
vdecl =
  do mytype <- atype
     ident  <- identifier
     if mytype == Int then
       (brackets integer >>= return . (Array ident))
       <|> (return $ Scalar mytype ident)
      else
        return $ Scalar mytype ident
     
procedure :: Parser Proc
procedure = 
  do reserved "procedure"
     name   <- identifier
     params <- parens $ sepBy parameter comma
     stats  <- many statement
     return $ Proc { procname = name, params = params, body = stats }

parameter :: Parser (Type, Ident)
parameter =
  do mytype <- atype
     ident  <- identifier
     return (mytype, ident)

statement :: Parser Stmt
statement =   try assignStmt
          <|> ifStmt
          <|> fromStmt
          <|> pushStmt
          <|> popStmt
          <|> localStmt
          <|> callStmt
          <|> uncallStmt
          <|> swapStmt
          <|> skipStmt
          <?> "statement"

assignStmt :: Parser Stmt
assignStmt =
  do lval  <- lval
     modop <- modOp
     expr  <- expression
     return $ Assign modop lval expr

modOp :: Parser ModOp
modOp =   (reservedOp "+=" >> return AddEq)
      <|> (reservedOp "-=" >> return SubEq)
      <|> (reservedOp "^=" >> return XorEq)

ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     entrycond  <- expression
     reserved "then"
     stats1     <- many statement
     stats2     <- option [] $ reserved "else" >> many1 statement
     reserved "fi"
     exitcond   <- expression
     return $ If entrycond stats1 stats2 exitcond

fromStmt :: Parser Stmt
fromStmt =
  do reserved "from"
     entrycond <- expression
     stats1    <- option [] $ reserved "do"   >> many1 statement
     stats2    <- option [] $ reserved "loop" >> many1 statement
     reserved "until"
     exitcond  <- expression
     return $ From entrycond stats1 stats2 exitcond

pushStmt :: Parser Stmt
pushStmt =
  do reserved "push"
     (x,y) <- parens $ do x <- identifier
                          comma
                          y <- identifier
                          return (x,y)
     return $ Push x y

popStmt :: Parser Stmt
popStmt =
  do reserved "pop"
     (x,y) <- parens $ do x <- identifier
                          comma
                          y <- identifier
                          return (x,y)
     return $ Pop x y

localStmt :: Parser Stmt
localStmt =
  do reserved "local"
     type1  <- atype
     ident1 <- identifier
     reservedOp "="
     expr1  <- expression
     stats  <- many statement
     reserved "delocal"
     type2  <- atype
     ident2 <- identifier
     reservedOp "="
     expr2  <- expression
     return $ Local (type1, ident1, expr1) stats (type2, ident2, expr2)

atype :: Parser Type
atype =   (reserved "int"   >> return Int)
      <|> (reserved "stack" >> return Stack)

callStmt :: Parser Stmt
callStmt =
  do reserved "call"
     procname <- identifier
     args     <- parens $ sepBy identifier comma
     return $ Call procname args

uncallStmt :: Parser Stmt
uncallStmt =
  do reserved "uncall"
     procname <- identifier
     args     <- parens $ sepBy identifier comma
     return $ Uncall procname args

swapStmt :: Parser Stmt
swapStmt =
  do ident1 <- identifier
     reservedOp "<=>"
     ident2 <- identifier
     return $ Swap ident1 ident2

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

expression :: Parser Expr
expression =  buildExpressionParser binOperators term

term :: Parser Expr
term =   parens expression
     <|> numberExpr
     <|> lvalExpr
     <|> emptyExpr
     <|> topExpr
     <?> "expression"

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


parseString :: String -> [Stmt]
parseString str =
  case parse (many statement) "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Program
parseFile file =
  do str <- readFile file
     case parse program "" str of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
