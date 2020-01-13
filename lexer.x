{
module Lexer
	( Token(..)
	, AlexPosn(..)
	, alexScanTokens
	, tokPosn
	, Parser
	, satisfy
	, identifier
	, integer
	, reserved
	, reservedOp
	, string
	, symbol
	, parens
	, braces
	, brackets
	, commaSep
	, semi
	)
where

import Control.Monad.Identity
import Text.Parsec (setSourceLine, setSourceColumn)
import Text.Parsec.Prim
import Text.Parsec.Combinator
}

%wrapper "posn"

$white  = [\ \t\n]
$digit  = 0-9
$alpha  = [a-zA-Z]
$symbol = [\{\}\(\)\[\]\,\;]
$opSym  = [\+\-\*\/\%\<\>\=]

@reserved   = fn | while | if | else | true | false | return
@reservedOp = $opSym | "==" | ":=" | "<=" | ">=" | "||"
@string     = $alpha | $digit | ' '

tokens :-

	$white                     ; 
	$symbol                    { \p s -> Sym p (head s) }
	@reserved                  { \p s -> Reserved p s }
	@reservedOp                { \p s -> ReservedOp p s }
	\" @string* \"             { \p s -> String p (tail (init s)) }
	$alpha [$alpha $digit \_]* { \p s -> Ident p s }
	$digit+                    { \p s -> Int p (read s) }


{
data Token
	= Sym        AlexPosn Char
	| Reserved   AlexPosn String
	| ReservedOp AlexPosn String
	| String     AlexPosn String
	| Ident      AlexPosn String
	| Int        AlexPosn Int
	deriving (Show, Eq)

tokPosn :: Token -> AlexPosn
tokPosn (Sym p _)        = p
tokPosn (Reserved p _)   = p
tokPosn (ReservedOp p _) = p
tokPosn (String p _)     = p
tokPosn (Ident p _)      = p
tokPosn (Int p _)        = p


type Parser a = ParsecT [Token] () Identity a

-- parse tokens
satisfy :: (Token -> Bool) -> Parser Token
satisfy f =
	tokenPrim show nextPos tokEq
	where
		tokEq t = if f t then Just t else Nothing
		nextPos pos _ []     = pos
		nextPos pos _ (t:_)  =
			let (AlexPn _ line column) = tokPosn t
			in setSourceColumn (setSourceLine pos line) column

identifier = satisfy isIdent >>= (\(Ident _ s) -> return s)
	where
		isIdent (Ident _ _) = True
		isIdent _           = False

integer = satisfy isInt >>= (\(Int _ n) -> return n)
	where
		isInt (Int _ _) = True
		isInt _         = False

reserved s = satisfy isRes >> return ()
	where
		isRes (Reserved _ s') = if s' == s then True else False
		isRes _               = False

reservedOp o = satisfy isResOp >> return ()
	where
		isResOp (ReservedOp _ o') = if o' == o then True else False
		isResOp _                 = False

string = satisfy isString >>= (\(String _ s) -> return s)
	where
		isString (String _ _) = True
		isString _            = False

symbol c = satisfy isSym >>= (\(Sym _ c) -> return c)
	where
		isSym (Sym _ c') = if c' == c then True else False 
		isSym _          = False

parens p   = between (symbol '(') (symbol ')') p
braces p   = between (symbol '{') (symbol '}') p
brackets p = between (symbol '[') (symbol ']') p

commaSep p =
	try     (do {first <- p; rest <- many (symbol ',' >> p); return (first:rest)})
	<|> try (fmap (:[]) p)
	<|>     (return [])

semi = symbol ';'
}
