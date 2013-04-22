
import System.Environment
import System.IO
import Control.Monad
import Data.List
import Jana.Parser
import Jana.Eval (runProgram)
import Jana.Types (defaultOptions, EvalOptions(..))


usage = "usage: jana [-m] <file>\n\
        \options:\n\
        \  -m           use 32-bit modular arithmetic"

parseArgs :: IO (Maybe ([String], EvalOptions))
parseArgs =
  do args <- getArgs
     (flags, files) <- return $ splitArgs args
     case checkFlags flags of
       Left err          -> putStrLn err >> return Nothing
       Right evalOptions -> return $ Just (files, evalOptions)

splitArgs :: [String] -> ([String], [String])
splitArgs = partition (\arg -> head arg == '-' && length arg > 1)

checkFlags :: [String] -> Either String EvalOptions
checkFlags = foldM addOption defaultOptions

addOption :: EvalOptions -> String -> Either String EvalOptions
addOption evalOptions "-m" =
  return $ evalOptions{ modInt = True }
addOption _ f = Left $ "invalid option: " ++ f

loadFile :: String -> IO String
loadFile "-" = hGetContents stdin
loadFile filename = readFile filename

parseAndRun :: String -> EvalOptions -> IO ()
parseAndRun filename evalOptions =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err
       Right prog -> runProgram filename prog evalOptions

main :: IO ()
main = do args <- parseArgs
          case args of
            Just ([file], flags) -> parseAndRun file flags
            _                    -> putStrLn usage
