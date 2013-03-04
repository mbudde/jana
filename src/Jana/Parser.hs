module Jana.Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as T

import Jana.Ast


janusDef :: T.LanguageDef st
janusDef = T.LanguageDef
    { T.commentStart    = ""
    , T.commentEnd      = ""
    , T.commentLine     = "#"
    , T.nestedComments  = True
    , T.identStart      = letter <|> char '_'
    , T.identLetter     = alphaNum <|> oneOf "_-"
    , T.opStart         = T.opLetter janusDef
    , T.opLetter        = oneOf "+-*/%&|<=>^"
    , T.reservedOpNames = []
    , T.reservedNames   = []
    , T.caseSensitive   = True
    }

varType :: Parser Type
varType =
        (reserved "int" >> return Int)
    <|> (reserved "stack" >> return Stack)

param :: Parser (Type, Ident)
param = do
    parType <- varType
    parName <- ident
    return (parType, parName)

paramList :: Parser [(Type, Ident)]
paramList = commaSep param

proc :: Parser Proc
proc = do
    reserved "procedure"
    name <- ident
    ps   <- parens paramList
    return Proc { procname = name, params = ps, body = [] }

parserTest =
    case (parse proc "" "procedure foo(int x)") of
        Left err -> error $ show err
        Right p  -> print p


-- Lexer
lexer = T.makeTokenParser janusDef

ident    = T.identifier lexer
reserved = T.reserved lexer
parens   = T.parens lexer
commaSep = T.commaSep lexer
