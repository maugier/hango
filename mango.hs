{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Map as M hiding (null)
import Data.Maybe
import Prelude hiding ((.))
import Text.Parsec hiding (label)
import Text.Parsec.Char
import Text.Parsec.ByteString

-- Utility code
(??) t _ True  = t
(??) _ f False = f

a !!= b = (a != b) >> return ()

item :: Integer -> Lens (Array Int a) a
item idx = lens (Data.Array.! (fromInteger idx)) (\v a -> a // [(fromInteger idx,v)])

showState = get >>= (liftIO . print)

-- Parsing stuff

type ProgPtr = [Tok]
data Tok = Label String
         | Number Integer
         | Keyword String       
        deriving (Show, Eq, Ord)

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
data Value = IntVal Integer | PtrVal ProgPtr deriving Show
type Stack = (Maybe String, Value)

data ProgState = ProgState {
        _stack :: [Stack],
        _vars  :: Map String Value,
        _arry  :: Array Int Value,
        _ip    :: ProgPtr
} deriving Show

$( makeLenses [''ProgState] )


initProg p = ProgState [] (allLabels p) (listArray (0,1000) (repeat (IntVal 0)))  p

push = (>> return ()) . (stack %=) . (:) 
pushi x = push (Nothing, IntVal x)

ucons l = (head l, tail l)

pop  = stack %%= ucons
popi = do { (_, IntVal v) <- pop; return v }
popp = do { (_, PtrVal p) <- pop; return p }

binop f = do { x <- popi; y <- popi; pushi (x `f` y) }

cjump test = do
	yes <- popp
	no <- popp
	if test
		then ip !!= yes
		else ip !!= no

step = (ip %%= ucons) >>= exec 
debugStep = showState >> liftIO getLine >> step

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
exec (Keyword "ifz") = popi >>= cjump . (== 0)
exec (Keyword "ifg") = popi >>= cjump . (> 0)
exec (Keyword "jump") = popp >>= (ip !!=)
exec (Keyword "mod") = binop mod
exec (Keyword "print_byte") = popi >>= liftIO.putChar.chr.fromInteger
exec (Keyword "print_num") = popi >>= liftIO.print
exec (Keyword "read_num") = liftIO readLn >>= pushi
exec (Keyword "read_byte") = liftIO getChar >>= pushi.toInteger.ord

exec (Keyword "store") = do
        (_, n) <- pop
        (Just addr, _) <- pop
        vars %= insert addr n
        return ()
exec (Keyword "sub") = binop (-)
exec (Keyword "vload") = do
        i <- popi
        x <- access (item i . arry)
        push (Nothing,x)
        return ()
exec (Keyword "vstore") = do
        idx <- popi
        (_,x) <- pop
        (item idx . arry) !!= x
exec (Keyword "xor") = binop xor

exec (Keyword k) = do
        vs <- access vars
        push (Just k, fromMaybe (IntVal 0) (M.lookup k vs))

loop step = terminate >>= (return () ?? (step >> loop step))


runProgram p = runStateT (loop step) (initProg p)
debugProgram p = runStateT (loop debugStep) (initProg p)

