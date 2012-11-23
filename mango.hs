{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.Lens
import Mango
import System.Environment
import System.IO
import Text.Parsec
import Text.Parsec.ByteString

decodeArgs :: [String] -> (String, [Tok] -> IO ())
decodeArgs [file] =  (file, runProgram)
decodeArgs ["-d",file] = (file, debugProgram)

main = do
        args <- getArgs
	let (f,run) = decodeArgs args
        Right prog <- parseFromFile program f
        run prog

oneLineStatus = do
	s <- access stack
	ip <- access ip
	io . hPutStrLn stderr $ case ip of
		[] -> "terminated."
	        (ni:_) -> "Instruction: " ++ show ni ++ "\n" ++
		          "Stack: " ++ show s
	

debugStep = do
       oneLineStatus
       io $ hPutStr stderr "debug> " 
       command <- io getLine
       case command of
	       "" -> step
               "s" -> step
	       "l" -> dumpLens ip
               "ds" -> dumpLens stack
               "da" -> dumpLens arry
               "dv" -> dumpLens vars
	       "q" -> ip !!= []
	       ('e':expr) -> case parse program "" expr of
		   Left err -> io (putStr "Parse error: " >> print err)
		   Right tok -> ip %= (tok ++) >> return ()
               _ -> io $ putStrLn "s - step\nl - list code\nds - dump stack\nda - dump array\ndv - dump vars\ne - evaluate expression"

debugProgram = runProgWith debugStep 
