{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Mango where

-- Mango parser

import Control.Applicative ((<$>), (<$), (<*>))
import Control.Category
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Array
import Data.Bits
import Data.Char (chr,ord)
import Data.Lens
import Data.Lens.Template
import Data.List (intersperse)
import Data.Map as M hiding (null)
import Data.Maybe
import Prelude hiding ((.))
import Text.Parsec hiding (label)
import Text.Parsec.Char

-- Utility code
(??) t _ True  = t
(??) _ f False = f

io = liftIO

a !!= b = (a != b) >> return ()

item :: Integer -> Lens (Array Int a) a
item idx = lens (Data.Array.! (fromInteger idx)) (\v a -> a // [(fromInteger idx,v)])


-- Parsing stuff

type ProgPtr = [Tok]
data Tok = Label String
         | Number Integer
         | Keyword String       

instance Show Tok where
	show (Label s) = s ++ ":"
	show (Number n) = "\t" ++ show n
	show (Keyword k) = "\t" ++ k
	showList l tail = concat (intersperse "\n" (show <$> take 10 l)) ++ tail

comment = string "/*" >> in_comment
voidChar = () <$ anyChar
in_comment =  (try (string "*/") >> return ())
          <|> ((comment <|> voidChar) >> in_comment) 
          <?> "end of comment"

whitespace = many1 (() <$ space  <|> comment )

tchar = oneOf $ '_' : ['a'..'z']

tok =  Number . read <$> many1 digit
   <|> do
	ident <- many1 tchar
	(char ':' >> return (Label ident)) 
                 <|> return (Keyword ident)
   <?> "token"

program = optional whitespace >> (tok `sepEndBy` whitespace)

-- Compiling

allLabels = M.fromList . allLabels' where
        allLabels' [] = []
        allLabels' (Label l : xs) = (l, PtrVal xs) : allLabels' xs
        allLabels' (_ : xs) = allLabels' xs

-- Evaluating
data Value = IntVal Integer | PtrVal ProgPtr
newtype Stack = Stack { unStack :: (Maybe String, Value) }

instance Show Stack where
	show (Stack (Nothing, v)) = show v
	show (Stack (Just var, v)) = "<" ++ var ++ ">" ++ show v

instance Show Value where
	show (IntVal n) = show n
	show (PtrVal _) = "<pointer>"

data ProgState = ProgState {
        _stack :: [Stack],
        _vars  :: Map String Value,
        _arry  :: Array Int Value,
        _ip    :: ProgPtr
} deriving Show

$( makeLenses [''ProgState] )

initProg p = ProgState [] (allLabels p) (listArray (0,1000) (repeat (IntVal 0)))  p

crash reason = (get >>= io.print) >> (ip !!= [])

push = (>> return ()) . (stack %=) . (:) . Stack 
pushi x = push (Nothing, IntVal x)

ucons l = (head l, tail l)

pop  = unStack <$> (stack %%= ucons)
popi = do { (_, IntVal v) <- pop; return v }
popp = do { (_, PtrVal p) <- pop; return p }

binop f = do { y <- popi; x <- popi; pushi (x `f` y) }

cjump test = do
	no <- popp
	yes <- popp
	x <- popi
	if test x
		then ip !!= yes
		else ip !!= no

step = (ip %%= ucons) >>= exec 

dumpLens l = access l >>= (io . print)

terminate = null <$> access ip

exec (Label _)  = return ()
exec (Number n) = pushi n
exec (Keyword "add") = binop (+)
exec (Keyword "call") = do
        tgt <- popp
        cur <- access ip
        push (Nothing, PtrVal cur)
        ip !!= tgt
exec (Keyword "dup") = do { x <- pop; push x; push x }
exec (Keyword "exit") = ip !!= []
exec (Keyword "ifz") = cjump (== 0)
exec (Keyword "ifg") = cjump (> 0)
exec (Keyword "jump") = popp >>= (ip !!=)
exec (Keyword "mod") = binop mod
exec (Keyword "print_byte") = popi >>= io.putChar.chr.fromInteger
exec (Keyword "print_num") = popi >>= io.putStr.show
exec (Keyword "read_num") = io readLn >>= pushi
exec (Keyword "read_byte") = io getChar >>= pushi.toInteger.ord

exec (Keyword "store") = do
        (Just addr, _) <- pop
        (_, n) <- pop
        vars %= insert addr n
        return ()
exec (Keyword "sub") = binop (-)
exec (Keyword "vload") = do
        i <- popi
        x <- access (item i . arry)
        push (Nothing,x)
        return ()
exec (Keyword "vstore") = do
        (_,x) <- pop
        idx <- popi
        (item idx . arry) !!= x
exec (Keyword "xor") = binop xor

exec (Keyword k) = do
        vs <- access vars
        push (Just k, fromMaybe (IntVal 0) (M.lookup k vs))



loop step = step >> (terminate >>= (return () ?? loop step))

runProgWith :: StateT ProgState IO () -> [Tok] -> IO ()
runProgWith step p = runStateT (loop step) (initProg p) >> return ()

runProgram = runProgWith step 

