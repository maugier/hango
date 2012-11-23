{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

-- Mango parser

import Control.Applicative ((<$>), (<$))
import Data.Array
import Data.Lens
import Data.Lens.Template
import Data.Map
import Text.Parsec hiding (label)
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.ByteString

-- Parsing stuff

data Tok = Label String | Word String
	deriving (Show, Eq, Ord)

comment = string "/*" >> in_comment

in_comment =  (try (string "*/") >> return ())
          <|> (skipMany1 (noneOf "*") >> in_comment)
          <?> "Open comment"

whitespace = skipMany1 (() <$ space <|> comment)

tok = many1.oneOf $ ['a'..'z']++['A'..'Z']++['0'..'9']
word = Word <$> tok
label = char ':' >> Label <$> tok

program = (label <|> word) `sepEndBy` whitespace

-- Compiling

type ProgPtr = [Tok]
data Stack = Stack (Maybe String, Integer)
data Var   = VarPtr ProgPtr | VarNum Integer

lookupLabel l = tail . dropWhile (/= Label l)

-- Evaluating

data ProgState = ProgState {
	_stack :: [Stack],
	_vars  :: Map String Var,
	_arry  :: Array Int Integer,
	_prog  :: ProgPtr,
	_ip    :: ProgPtr
}

$( makeLenses [''ProgState] )

--init p = ProgState [] empty (newArray (0,1000) 0) p p

push = (stack %=) . (:)
pop  = stack %%= (\h -> (head h, tail h))


exec "dup" = stack %= \(x:xs) -> (x:x:xs)
exec "read_num" = do { n <- liftIO readLn; push Stack (Nothing,n) }
exec "store" = do
	Stack (_, n) <- pop
	Stack (Just addr, _) <- pop
	vars %= insert addr n

	


