import Mango
import System.Environment
import Text.Parsec.ByteString

main = do
        [file] <- getArgs
        Right prog <- parseFromFile program file
        runProgram prog

