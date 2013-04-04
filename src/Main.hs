
import System.Environment
import Control.Monad
import Data.List
import Jana.Parser
import Jana.Eval (runProgram)
import Jana.Types (defaultOptions, EvalOptions(..))


usage = "usage: jana [-m] <file>\n\
        \options:\n\
        \  -m           use 32-bit modular arithmetic"

parseArgs :: IO (Maybe (String, EvalOptions))
parseArgs =
  do args <- getArgs
     (flags, file) <- return $ splitArgs args
     case (checkFlags flags, checkFile file) of
       (Left err, _) -> putStrLn err >> return Nothing
       (_, Left err) -> putStrLn err >> return Nothing
       (Right evalOptions, Right file) -> return $ Just (file, evalOptions)

checkFile :: [String] -> Either String String
checkFile [file] = Right file
checkFile _ = Left "please provide a single file name"

splitArgs :: [String] -> ([String], [String])
splitArgs args =
  partition (\arg -> (head arg) == '-') args

checkFlags :: [String] -> Either String EvalOptions
checkFlags = foldM addOption defaultOptions

addOption :: EvalOptions -> String -> Either String EvalOptions
addOption evalOptions "-m" =
  return $ evalOptions{ modInt = True }
addOption _ f = Left $ "invalid option: " ++ f

parseAndRun :: String -> EvalOptions -> IO ()
parseAndRun filename evalOptions =
  do ast <- parseFile filename
     case ast of
       Left err   -> print err
       Right prog -> runProgram filename prog evalOptions

main :: IO ()
main = do args <- parseArgs
          case args of
            Nothing            -> putStrLn usage
            Just (file, flags) -> parseAndRun file flags
