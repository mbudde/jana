module Jana.Parser (
    parseExprString, parseStmtsString, parseString, parseFile,
    ) where

import Prelude hiding (GT, LT, EQ)
import Control.Monad (liftM, liftM2)
import Data.Either (partitionEithers)

import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Pos
import qualified Text.Parsec.Token as Token

import Jana.Ast

janaDef = Token.LanguageDef {
                Token.commentStart     = "/*"
              , Token.commentEnd       = "*/"
              , Token.commentLine      = "//"
              , Token.nestedComments   = False
              , Token.identStart       = letter
              , Token.identLetter      = alphaNum
              , Token.opStart          = oneOf ""
              , Token.opLetter         = oneOf ""
              , Token.reservedOpNames  = []
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

t_identifier = Token.identifier lexer -- parses an identifier
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

identifier :: Parser Ident
identifier =
  do pos   <- getPosition
     ident <- t_identifier
     return $ Ident ident pos

program :: Parser Program
program =
  do whiteSpace
     ([main], procs) <- liftM partitionEithers (many genProcedure)
     return $ Program main procs

genProcedure :: Parser (Either ProcMain Proc)
genProcedure =   try (liftM Left mainProcedure)
             <|> liftM Right procedure

mainProcedure :: Parser ProcMain
mainProcedure =
  do reserved "procedure"
     pos <- getPosition
     reserved "main"
     parens whiteSpace
     vdecls <- many vdecl
     stats  <- many1 statement
     return $ ProcMain vdecls stats pos

vdecl :: Parser Vdecl
vdecl =
  do mytype <- atype
     ident  <- identifier
     pos    <- getPosition
     case mytype of
       (Int _) ->     liftM2 (Array ident) (brackets integer) getPosition
                  <|> return (Scalar mytype ident pos)
       _       -> return $ Scalar mytype ident pos

procedure :: Parser Proc
procedure =
  do reserved "procedure"
     name   <- identifier
     params <- parens $ sepBy parameter comma
     stats  <- many1 statement
     return Proc { procname = name, params = params, body = stats }

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
          <|> try swapStmt
          <|> skipStmt
          <?> "statement"

assignStmt :: Parser Stmt
assignStmt =
  do lval  <- lval
     pos   <- getPosition
     modop <- modOp
     expr  <- expression
     return $ Assign modop lval expr pos

modOp :: Parser ModOp
modOp =   (reservedOp "+=" >> return AddEq)
      <|> (reservedOp "-=" >> return SubEq)
      <|> (reservedOp "^=" >> return XorEq)

ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     pos   <- getPosition
     entrycond <- expression
     reserved "then"
     stats1    <- many1 statement
     stats2    <- option [] $ reserved "else" >> many1 statement
     reserved "fi"
     exitcond  <- expression
     return $ If entrycond stats1 stats2 exitcond pos

fromStmt :: Parser Stmt
fromStmt =
  do reserved "from"
     pos   <- getPosition
     entrycond <- expression
     stats1    <- option [] $ reserved "do"   >> many1 statement
     stats2    <- option [] $ reserved "loop" >> many1 statement
     reserved "until"
     exitcond  <- expression
     return $ From entrycond stats1 stats2 exitcond pos

pushStmt :: Parser Stmt
pushStmt =
  do reserved "push"
     pos   <- getPosition
     (x,y) <- parens twoArgs
     return $ Push x y pos

popStmt :: Parser Stmt
popStmt =
  do reserved "pop"
     pos   <- getPosition
     (x,y) <- parens twoArgs
     return $ Pop x y pos

twoArgs :: Parser (Ident, Ident)
twoArgs =
  do x <- identifier
     comma
     y <- identifier
     return (x,y)


localStmt :: Parser Stmt
localStmt =
  do reserved "local"
     pos   <- getPosition
     type1  <- atype
     ident1 <- identifier
     reservedOp "="
     expr1  <- expression
     stats  <- many1 statement
     reserved "delocal"
     type2  <- atype
     ident2 <- identifier
     reservedOp "="
     expr2  <- expression
     return $ Local (type1, ident1, expr1) stats (type2, ident2, expr2) pos

atype :: Parser Type
atype =   (reserved "int"   >> liftM Int getPosition)
      <|> (reserved "stack" >> liftM Stack getPosition)

callStmt :: Parser Stmt
callStmt =
  do reserved "call"
     pos   <- getPosition
     procname <- identifier
     args     <- parens $ sepBy identifier comma
     return $ Call procname args pos

uncallStmt :: Parser Stmt
uncallStmt =
  do reserved "uncall"
     pos   <- getPosition
     procname <- identifier
     args     <- parens $ sepBy identifier comma
     return $ Uncall procname args pos

swapStmt :: Parser Stmt
swapStmt =
  do ident1 <- identifier
     pos   <- getPosition
     reservedOp "<=>"
     ident2 <- identifier
     return $ Swap ident1 ident2 pos

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> liftM Skip getPosition

expression :: Parser Expr
expression = buildExpressionParser binOperators term

addPos :: Parser (SourcePos -> a) -> Parser a
addPos parser = do pos <- getPosition
                   t   <- parser
                   return $ t pos

term :: Parser Expr
term =   parens expression
     <|> addPos numberExpr
     <|> addPos lvalExpr
     <|> addPos emptyExpr
     <|> addPos topExpr
     <|> addPos nilExpr
     <?> "expression"

numberExpr :: Parser (SourcePos -> Expr)
numberExpr = liftM Number integer

lvalExpr :: Parser (SourcePos -> Expr)
lvalExpr = liftM LV lval

lval ::  Parser Lval
lval =   try lookUp
     <|> liftM Var identifier

nilExpr :: Parser (SourcePos -> Expr)
nilExpr = reserved "nil" >> (return Nil)

lookUp :: Parser Lval
lookUp = liftM2 Lookup identifier (brackets expression)

emptyExpr :: Parser (SourcePos -> Expr)
emptyExpr =
  do reserved "empty"
     ident <- (parens identifier)
     return $ Empty ident

topExpr :: Parser (SourcePos -> Expr)
topExpr =
  do reserved "top"
     ident <- (parens identifier)
     return $ Top ident

binOperators = [ [ Infix (reservedOp "*"   >> return (BinOp Mul )) AssocLeft
                 , Infix (reservedOp "/"   >> return (BinOp Div )) AssocLeft
                 , Infix (reservedOp "%"   >> return (BinOp Mod )) AssocLeft ]
               , [ Infix (reservedOp "+"   >> return (BinOp Add )) AssocLeft
                 , Infix (reservedOp "-"   >> return (BinOp Sub )) AssocLeft ]
               , [ Infix (reservedOp "<="  >> return (BinOp LE  )) AssocLeft
                 , Infix (reservedOp "<"   >> return (BinOp LT  )) AssocLeft
                 , Infix (reservedOp ">="  >> return (BinOp GE  )) AssocLeft
                 , Infix (reservedOp ">"   >> return (BinOp GT  )) AssocLeft
                 , Infix (reservedOp "="   >> return (BinOp EQ  )) AssocLeft
                 , Infix (reservedOp "!="  >> return (BinOp NEQ )) AssocLeft ]
               , [ Infix (try $ reservedOp "&" >> lookAhead (noneOf "&") >> return (BinOp And)) AssocLeft
                 , Infix (try $ reservedOp "|" >> lookAhead (noneOf "|") >> return (BinOp Or )) AssocLeft
                 , Infix (reservedOp "^"   >> return (BinOp Xor )) AssocLeft ]
               , [ Infix (reservedOp "&&"  >> return (BinOp LAnd)) AssocLeft
                 , Infix (reservedOp "||"  >> return (BinOp LOr )) AssocLeft ]
               ]

parseString :: Parser a -> String -> a
parseString parser str =
  case parse parser "" str of
    Left e  -> error $ show e
    Right r -> r

parseExprString :: String -> Expr
parseExprString = parseString expression

parseStmtsString :: String -> [Stmt]
parseStmtsString = parseString (many1 statement)

parseFile :: String -> IO Program
parseFile file =
  do str <- readFile file
     case parse program file str of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
