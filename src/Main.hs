
import System.Environment
import System.Exit
import System.IO
import System.Timeout
import Control.Monad
import Data.List
import Jana.Parser
import Jana.Eval (runProgram)
import Jana.Types (defaultOptions, EvalOptions(..))
import Jana.Invert


data Options = Options
  { timeOut :: Int
  , invert :: Bool
  , evalOpts :: EvalOptions }

defaults = Options
  { timeOut = -1
  , invert = False
  , evalOpts = defaultOptions }

usage = "usage: jana [options] <file>\n\
        \options:\n\
        \  -m           use 32-bit modular arithmetic\n\
        \  -tN          timeout after N seconds\n\
        \  -i           print inverted program"

parseArgs :: IO (Maybe ([String], Options))
parseArgs =
  do args <- getArgs
     (flags, files) <- return $ splitArgs args
     case checkFlags flags of
       Left err   -> putStrLn err >> return Nothing
       Right opts -> return $ Just (files, opts)

splitArgs :: [String] -> ([String], [String])
splitArgs = partition (\arg -> head arg == '-' && length arg > 1)

checkFlags :: [String] -> Either String Options
checkFlags = foldM addOption defaults

addOption :: Options -> String -> Either String Options
addOption opts@(Options { evalOpts = evalOptions }) "-m" =
  return $ opts { evalOpts = evalOptions { modInt = True } }
addOption opts ('-':'t':time) =
  case reads time of
    [(timeVal, "")] -> return $ opts { timeOut = timeVal }
    _               -> Left "non-number given to -t option"
addOption opts "-i" = return opts { invert = True }
addOption _ f = Left $ "invalid option: " ++ f

loadFile :: String -> IO String
loadFile "-"      = getContents
loadFile filename = readFile filename

printInverted :: String -> IO ()
printInverted filename =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> print $ invertProgram prog

parseAndRun :: String -> EvalOptions -> IO ()
parseAndRun filename evalOptions =
  do text <- loadFile filename
     case parseProgram filename text of
       Left err   -> print err >> (exitWith $ ExitFailure 1)
       Right prog -> runProgram filename prog evalOptions

main :: IO ()
main = do args <- parseArgs
          case args of
            Just ([file], Options { invert = True }) -> printInverted file
            Just ([file], opts) ->
              do res <- timeout (timeOut opts * 1000000)
                                (parseAndRun file (evalOpts opts))
                 case res of
                   Nothing -> exitWith $ ExitFailure 124
                   _       -> return ()
            _ -> putStrLn usage
