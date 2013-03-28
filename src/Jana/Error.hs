module Jana.Error where

import Control.Monad.Error
import Data.List (intercalate)
import Text.Printf (printf)
import Text.Parsec.Pos

import Jana.Ast


joinlines = intercalate "\n"

indent :: String -> String
indent = joinlines . map ("    " ++) . lines


data Message = Message String
             | InArgument String Ident
             | InExpression Expr
             | InStatement Stmt
             | InProcedure String
             | StoreState String

instance Show Message where
  show (Message s) = s
  show (InArgument funid argid) =
    printf "In an argument of `%s', namely `%s'"
           funid (ident argid)
  show (InExpression expr) =
    "In expression:\n" ++ (indent $ show expr)
  show (InStatement stmt) =
    "In statement:\n" ++ (indent $ show stmt)
  show (InProcedure proc) =
    printf "In procedure `%s'" proc
  show (StoreState store) =
    "State when error occured:\n" ++ (indent store)


data JanaError = JanaError SourcePos [Message]

instance Error JanaError where
  noMsg  = newErrorUnknown (newPos "" 0 0)
  strMsg = newErrorMessage (newPos "" 0 0) . Message

errorPos :: JanaError -> SourcePos
errorPos (JanaError pos _)
  = pos

errorMessages :: JanaError -> [Message]
errorMessages (JanaError _ msgs)
  = msgs

errorIsUnknown :: JanaError -> Bool
errorIsUnknown (JanaError _ msgs)
  = null msgs

newErrorUnknown :: SourcePos -> JanaError
newErrorUnknown pos
  = JanaError pos []

newErrorMessage :: SourcePos -> Message -> JanaError
newErrorMessage pos msg
  = JanaError pos [msg]

addErrorMessage :: Message -> JanaError -> JanaError
addErrorMessage msg (JanaError pos msgs)
  = JanaError pos (msgs ++ [msg])

setErrorPos :: JanaError -> SourcePos -> JanaError
setErrorPos (JanaError _ msgs) pos
  = JanaError pos msgs

{-
setErrorMessage :: Message -> JanaError -> JanaError
setErrorMessage msg (JanaError pos msgs)
    = JanaError pos (msg : filter (msg /=) msgs)
-}

instance Show JanaError where
  show (JanaError pos msgs) =
    printf "File \"%s\" in line %d, column %d:\n%s"
           (sourceName pos) (sourceLine pos) (sourceColumn pos)
           (showMsgs msgs)
    where showMsgs []   = indent "Unknown error occured"
          showMsgs msgs = joinlines $ map (indent . show) msgs
