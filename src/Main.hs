
import System.Environment
import Jana.Parser
import Jana.Eval (runProgram)

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> parseFile (args !! 0) >>= runProgram
            _      -> putStrLn "usage: jana <file>"
