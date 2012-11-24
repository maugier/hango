{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Applicative
import Control.Monad.Error
import Data.Lens
import Data.List
import Mango
import System.Environment
import System.IO
import Text.Parsec
import Text.Parsec.ByteString

warn msg = io . hPutStrLn stderr $ "Execution blocked: " ++ msg
safeStep = catchError (fetch >>= exec) (\msg -> warn msg >> dump)

onLeft _ (Right x) = Right x
onLeft f (Left x) = Left (f x)

wrapError = mapErrorT (fmap (onLeft show))

withErrors m = do
	r <- runErrorT m
	case r of
		Right _ -> return ()
		Left err -> fail err

decodeArgs [file]      = return (file, runProgram)
decodeArgs ["-d",file] = return (file, debugProgram)
decodeArgs _           = fail "Usage: hango [-d] file.mango"


main = withErrors $ do
        (file,run) <- io getArgs >>= decodeArgs
        prog <- wrapError . ErrorT $ parseFromFile program file
        run prog

oneLineStatus = do
	s <- access stack
	ip <- access ip
	io . hPutStrLn stderr $ case ip of
		[] -> "terminated."
	        (ni:_) -> "Instruction: " ++ show ni ++ '\t' : 
		          "Stack: " ++ show s
	
listTok (Label s) = s ++ ":"
listTok (Number n) = '\t' : show n
listTok (Keyword k) = '\t' : k

listCode l = intercalate "\n" (listTok <$> take 10 l)

debugStep = do
       oneLineStatus
       io $ hPutStr stderr "debug> " 
       command <- io getLine
       case command of
	       "" -> safeStep
               "s" -> safeStep
	       "l" -> access ip >>= io.putStr.listCode
               "ds" -> dumpLens stack
               "da" -> dumpLens arry
               "dv" -> dumpLens vars
	       "q" -> ip !!= []
	       ('e':expr) -> case parse program "" expr of
		   Left err -> io (putStr "Parse error: " >> print err)
		   Right tok -> void (ip %= (tok ++))
               _ -> io $ putStrLn "s - step\nl - list code\nds - dump stack\nda - dump array\ndv - dump vars\ne - evaluate expression"

debugProgram = runProgWith debugStep 
