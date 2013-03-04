module Jana.Parser where

import Jana.Ast
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

def = emptyDef{
    commentStart     = "(*",
    commentEnd       = "*)",
    identStart       = letter,
    identLetter      = alphaNum,
    opStart          = oneOf "+-^=",
    opLetter         = oneOf "+-^=",
    reservedOpNames  = ["+", "-", "^", "="],
    reservedNames    = ["procedure",
                        "int", "stack",
                        "if", "then", "else", "fi",
                        "from", "do", "loop", "until",
                        "push", "pop",
                        "local", "delocal",
                        "call", "uncall",
                        "skip",
                        "empty", "top",
                        "nil"]
  }

TokenParser{ parens     = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved   = m_reserved
           , semiSep1   = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def


main :: IO ()
main = play("hey")
