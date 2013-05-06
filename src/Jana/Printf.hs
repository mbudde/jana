module Jana.Printf (printfRender) where

import Data.List (elemIndex, delete)

import Jana.Error
import Jana.ErrorMessages


printfRender :: [String] -> [(String, String)] -> Either Message String
printfRender str [] =
  case findPercent (last str) of
    Just _  -> Left printfNotEnoughArgs
    Nothing -> Right $ concat str
printfRender str vList@((var, varType):vars) =
  case percentIndex of
    Just idx | typeStr `elem` acceptedTypes ->
                 if typeStr == varType
                   then printfRender (init str ++ [insertVar var cutFirstStr, cutLastStr]) vars
                   else Left $ printfTypeMismatch typeChar typeStr varType
             | typeChar == '%' ->
                 printfRender (init str ++ [delete '%' cutFirstStr, cutLastStr]) vList
             | otherwise -> Left $ printfUnrecognizedType typeChar

      where lastStr = last str
            typeChar = last str !! (idx+1)
            acceptedTypes = ["int", "array", "bool", "stack"]
            typeStr = correspondingType typeChar
            cutFirstStr = take (idx + 2) lastStr
            cutLastStr = drop (idx + 2) lastStr

    Nothing  -> if null vList
                  then Right $ concat str
                  else Left printfTooManyArgs
  where percentIndex = findPercent (last str)
        insertVar :: String -> String -> String
        insertVar var str =
          take (length str - 2) str ++  var

correspondingType :: Char -> String
correspondingType typeChar =
  case typeChar of
    'd' -> "int"
    'a' -> "array"
    'b' -> "bool"
    't' -> "stack"
    _   -> ""

findPercent :: String -> Maybe Int
findPercent = elemIndex '%'
