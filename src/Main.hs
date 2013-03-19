
import System.Environment
import Jana.Parser

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> parseFile (args !! 0) >>= print
            _      -> putStrLn "usage: jana <file>"
