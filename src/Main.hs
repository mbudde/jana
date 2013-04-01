
import System.Environment
import Jana.Parser
import Jana.Eval (runProgram)

getFilename :: IO (Maybe String)
getFilename =
  do args <- getArgs
     case args of
       [file] -> return $ Just file
       _      -> return Nothing

parseAndRun :: String -> IO ()
parseAndRun filename =
  do ast <- parseFile filename
     case ast of
       Left err   -> print err
       Right prog -> runProgram prog

main :: IO ()
main = do file <- getFilename
          case file of
            Nothing   -> print "usage: jana <file>"
            Just file -> parseAndRun file
