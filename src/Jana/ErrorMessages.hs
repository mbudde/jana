module Jana.ErrorMessages where

import Control.Monad.Error
import Text.Printf

import Jana.Error
import Jana.Ast



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

swapTypeError :: String -> String -> Message
swapTypeError typ1 typ2 = Message $
  printf "Can't swap variables of type `%s' and `%s'" typ1 typ2

outOfBounds :: (PrintfArg a) => a -> a -> Message
outOfBounds index size = Message $
  printf "Array index `%d' was out of bounds (array size was %d)"
         index size

emptyStack :: Message
emptyStack = Message "Can't pop from empty stack"

popToNonZero :: Ident -> Message
popToNonZero id = Message $
  printf "Can't pop to non-zero variable `%s'" (ident id)

assertionFail :: String -> Message
assertionFail s = Message $
  "Assertion failed: " ++ s

wrongDelocalValue :: Ident -> String -> String -> Message
wrongDelocalValue id expect actual = Message $
  printf "Expected value to be `%s' for local variable `%s'\n\
         \ but actual value is `%s'"
         expect (ident id) actual

undefProc :: String -> Message
undefProc name = Message $
  printf "Procedure `%s' is not defined" name

procDefined :: (Identifiable a) => a -> Message
procDefined id = Message $
  printf "Procedure `%s' is already defined" (ident id)

argumentError :: (Identifiable a, PrintfArg b) => a -> b -> b -> Message
argumentError id expect actual = Message $
  printf "Procedure `%s' expects %d argument(s) but got %d"
         (ident id) expect actual

arraySize :: Message
arraySize = Message "Array size must be greater than or equal to one"

divisionByZero :: Message
divisionByZero = Message "Division by zero"

noMainProc :: Message
noMainProc = Message "No main procedure has been defined"

multipleMainProcs :: Message
multipleMainProcs = Message "Multiple main procedures has been defined"

procDuplicateArgs :: Proc -> Message
procDuplicateArgs id = Message $
  printf "Procedure `%s' has duplicate arguments" (ident id)

