module Jana.Error where

import Control.Monad.Error
import Data.List (intercalate, sort)
import Text.Printf (printf)
import Text.Parsec.Pos

import Jana.Ast
import Jana.Format


joinlines = intercalate "\n"

indent :: String -> String
indent = joinlines . map ("    " ++) . lines


data Message = Message String
             | InArgument String String
             | InExpression Expr
             | InStatement Stmt String
             | InProcedure String

instance Enum Message where
    fromEnum (Message       _) = 0
    fromEnum (InArgument  _ _) = 1
    fromEnum (InExpression  _) = 2
    fromEnum (InStatement _ _) = 3
    fromEnum (InProcedure   _) = 4
    toEnum _ = error "toEnum is undefined for Message"

instance Eq Message where
    m1 == m2 = fromEnum m1 == fromEnum m2

instance Ord Message where
    compare msg1 msg2 = compare (fromEnum msg1) (fromEnum msg2)

instance Show Message where
  show (Message s) = s
  show (InArgument funid argid) =
    printf "In an argument of `%s', namely `%s'"
           funid argid
  show (InExpression expr) =
    "In expression:\n" ++ (indent $ show expr)
  show (InStatement stmt@(Local {}) store) =
    printf "In statement: %s\n%s" (show $ formatDelocal stmt) (indent store)
  show (InStatement stmt store) =
    printf "In statement: %s\n%s" (show stmt) (indent store)
  show (InProcedure proc) =
    printf "In procedure `%s'" proc


data JanaError = JanaError SourcePos [Message]

instance Error JanaError where
  noMsg  = newErrorUnknown (newPos "" 0 0)
  strMsg = newErrorMessage (newPos "" 0 0) . Message

errorPos :: JanaError -> SourcePos
errorPos (JanaError pos _)
  = pos

errorMessages :: JanaError -> [Message]
errorMessages (JanaError _ msgs)
  = sort $ reverse msgs

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
  = JanaError pos (msg : msgs)

setErrorPos :: JanaError -> SourcePos -> JanaError
setErrorPos (JanaError _ msgs) pos
  = JanaError pos msgs

setErrorMessage :: Message -> JanaError -> JanaError
setErrorMessage msg (JanaError pos msgs)
    = JanaError pos (msg : filter (msg /=) msgs)

addOnceErrorMessage :: Message -> JanaError -> JanaError
addOnceErrorMessage msg err@(JanaError pos msgs)
    | msg `elem` msgs = err
    | otherwise = JanaError pos (msg : filter (msg /=) msgs)

instance Show JanaError where
  show err =
    printf "File \"%s\" in line %d, column %d:\n%s"
           (sourceName pos) (sourceLine pos) (sourceColumn pos)
           (showMsgs msgs)
    where pos = errorPos err
          msgs = errorMessages err
          showMsgs []   = indent "Unknown error occured"
          showMsgs msgs = joinlines $ map (indent . show) msgs
