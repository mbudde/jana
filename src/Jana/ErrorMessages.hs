module Jana.ErrorMessages where

import Control.Monad.Error
import Text.Printf
import Jana.Error

import Jana.Ast

{- throwJanaError :: (MonadError e m) => SourcePos -> Message -> m a -}
throwJanaError pos msg = throwError $ newErrorMessage pos msg

unboundVar :: String -> Message
unboundVar name = Message $
  printf "Variable `%s' has not been declared" name

alreadyBound :: String -> Message
alreadyBound name = Message $
  printf "Variable name `%s' is already bound" name

typeError :: String -> Message
typeError = Message

typeMismatch :: String -> String -> Message
typeMismatch expType actualType = Message $
  printf "Couldn't match expected type `%s'\n\
         \            with actual type `%s'" expType actualType

outOfBounds :: (PrintfArg a) => a -> a -> Message
outOfBounds index size = Message $
  printf "Array index `%d' was out of bounds (array size was 4)"
         index size

emptyStack :: Message
emptyStack = Message "Can't pop from empty stack"

assertionFail :: String -> Message
assertionFail s = Message $
  "Assertion failed: " ++ s

undefProc :: String -> Message
undefProc name = Message $
  printf "Procedure `%s' is not defined" name

procDefined :: (Identifiable a) => a -> Message
procDefined id = Message $
  printf "Procedure `%s' is already defined" (ident id)

argumentError :: (PrintfArg a) => Ident -> a -> a -> Message
argumentError procId expect actual = Message $
  printf "Procedure `%s' expects %d argument(s) but got %d"
         (ident procId) expect actual

arraySize :: Message
arraySize = Message "Array size must be greater than or equal to one"

