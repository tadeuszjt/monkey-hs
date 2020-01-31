{
module Lexer
	( Token(..)
	, AlexPosn(..)
	, alexScanTokens
	, tokPosn
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

@reserved   = let | fn | while | if | else | true | false | return | print
@reservedOp = $opSym | "==" | "<=" | ">=" | "||" | "&&"
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
}
