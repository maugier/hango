{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Mango where

import Control.Applicative ((<$>), (<$), (<*>), (<*))
import Control.Category
import Control.Monad.Error
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Array
import Data.Bits
import Data.Char (chr,ord)
import Data.Lens
import Data.Lens.Template
import Data.Map as M hiding (null)
import Data.Maybe
import Prelude hiding ((.))
import Text.Parsec hiding (label)
import Text.Parsec.Char

---------------
-- Utility code

(??) t _ True  = t
(??) _ f False = f

io = liftIO

a !!= b = (a != b) >> return ()

-- Lens for array items
item :: Integer -> Lens (Array Int a) a
item idx = lens (Data.Array.! (fromInteger idx)) (\v a -> a // [(fromInteger idx,v)])


----------
-- PARSING

type ProgPtr = [Tok]
data Tok = Label String
         | Number Integer
         | Keyword String       
	 deriving Show


comment = (string "/*" >> in_comment) <?> "comment"
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

-- eof here chokes on trailing garbage, but prevents single-pass parsing
program = optional whitespace >> (tok `sepEndBy1` whitespace) <* eof

------------
-- Compiling

-- find all labels, will use them as variables later
allLabels = M.fromList . allLabels' where
        allLabels' [] = []
        allLabels' (Label l : xs) = (l, PtrVal xs) : allLabels' xs
        allLabels' (_ : xs) = allLabels' xs

------------
-- Execution

data Value = IntVal Integer | PtrVal ProgPtr

{- Stack values contain both a value and an optional variable symbol.
 - This is to get around a language quirk, the "magical" semantics
 - of 'store' which operates on the name of the last variable pushed
 - instead of on its content. This seemed a more robust approach
 - than looking ahead for a 'store' token in the parser to distinguish
 - symbols from values.
 -}
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

type MangoT m a = StateT ProgState (ErrorT String m) a

initProg p = ProgState
	[]               -- empty stack at startup
	(allLabels p)    -- first convert all labels as pointer variables 
	(listArray (0,1000) (repeat (IntVal 0))) -- empty array
	p                -- full program as initial IP

exit = (ip !!= [])
dump :: MangoT IO ()
dump = (get >>= io.print)
crash reason = (io.print) reason >> dump >> exit
m <!> r = catchError m (const $ throwError r)

push = (>> return ()) . (stack %=) . (:) . Stack 
pushi x = push (Nothing, IntVal x)

ucons = StateT $ ucons' where
	ucons' [] = fail "Empty stack"
	ucons' (h:t) = return (h,t)

pop  = unStack <$> focus stack ucons
popi = do { (_, IntVal v) <- pop; return v } <!> "Type error, int expected"
popp = do { (_, PtrVal p) <- pop; return p } <!> "Type error, ptr expected"
pops = do { (Just s, _) <- pop; return s } <!> "non-symbolic value on stack"

binop f = do { y <- popi; x <- popi; pushi (x `f` y) }

cjump test = do
	no <- popp
	yes <- popp
	x <- popi
	if test x
		then ip !!= yes
		else ip !!= no

-- Execution of a single operand

exec (Label _)  = return () 
exec (Number n) = pushi n
exec (Keyword "add") = binop (+)
exec (Keyword "call") = do
        tgt <- popp
        cur <- access ip
        push (Nothing, PtrVal cur)
        ip !!= tgt
exec (Keyword "dup") = do { x <- pop; push x; push x }
exec (Keyword "exit") = exit
exec (Keyword "ifz") = cjump (== 0)
exec (Keyword "ifg") = cjump (> 0)
exec (Keyword "jump") = popp >>= (ip !!=)
exec (Keyword "mod") = binop mod
exec (Keyword "print_byte") = popi >>= io.putChar.chr.fromInteger
exec (Keyword "print_num") = popi >>= io.putStr.show
exec (Keyword "read_num") = io readLn >>= pushi
exec (Keyword "read_byte") = io getChar >>= pushi.toInteger.ord

exec (Keyword "store") = do
        addr <- pops
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

fetch = focus ip ucons

step = catchError (fetch >>= exec) crash

dumpLens l = access l >>= (io . print)

terminated = null <$> access ip

loop step = step >> (terminated >>= (return () ?? loop step))

runProgWith step p = evalStateT (loop step) (initProg p)

runProgram = runProgWith step 

