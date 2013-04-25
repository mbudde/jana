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

typeMismatch :: [String] -> String -> Message
typeMismatch expTypes actualType = Message $
  printf "Couldn't match expected type %s\n\
         \            with actual type `%s'" (join expTypes) actualType
  where join []     = ""
        join [x]    = quote x
        join [x, y] = quote x ++ " or " ++ quote y
        join (x:xs) = quote x ++ ", " ++ join xs
        quote s = "`" ++ s ++ "'"

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

delocalNameMismatch :: Ident -> Ident -> Message
delocalNameMismatch id1 id2 = Message $
  printf "Variable names does not match in local declaration:\n\
         \    `%s' in `local'\n\
         \    `%s' in `delocal'\n\
         \`delocal' statements must come in reverse order of the `local' statments"
         (ident id1) (ident id2)

delocalTypeMismatch :: Ident -> String -> String -> Message
delocalTypeMismatch id locType delocType = Message $
  printf "Type of variable `%s' does not match local declaration:\n\
         \    `%s' in `local'\n\
         \    `%s' in `delocal'"
         (ident id) locType delocType

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

callingMainError :: Message
callingMainError = Message "It is not allowed to call the `main' procedure"

argumentError :: (Identifiable a, PrintfArg b) => a -> b -> b -> Message
argumentError id expect actual = Message $
  printf "Procedure `%s' expects %d argument(s) but got %d"
         (ident id) expect actual

arraySize :: Message
arraySize = Message "Array size must be greater than or equal to one"

arraySizeMissing :: Ident -> Message
arraySizeMissing id = Message $
  printf "Array size missing for variable `%s'" (ident id)

arraySizeMismatch :: (PrintfArg a, PrintfArg b) => a -> b -> Message
arraySizeMismatch exp actual = Message $
  printf "Expecting array of size %d\n\
         \           but got size %d"
         exp actual

divisionByZero :: Message
divisionByZero = Message "Division by zero"

noMainProc :: Message
noMainProc = Message "No main procedure has been defined"

multipleMainProcs :: Message
multipleMainProcs = Message "Multiple main procedures has been defined"

procDuplicateArgs :: Proc -> Message
procDuplicateArgs id = Message $
  printf "Procedure `%s' has duplicate arguments" (ident id)

userError :: String -> Message
userError msg = Message $ "User error: " ++ msg

printfTypeMismatch :: String -> String -> Message
printfTypeMismatch expected given = Message $ printf "Type mismatch in printf statement: Expected %s, given %s" expected given

printfArgMismatch :: Message
printfArgMismatch = Message "Number of arguments does not correspond with number of %<type char>"

printfUnrecognizedType :: Char -> Message
printfUnrecognizedType char = Message $ printf "Unrecognized type character: %c" char
