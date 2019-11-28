module Lexer where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Tok

lexer :: Tok.TokenParser()
lexer = Tok.makeTokenParser emptyDef {
	Tok.reservedOpNames = ["+", "-", "*", "/", "="],
	Tok.reservedNames = ["let", "return", "fn", "test"]
}

identifier = Tok.identifier lexer	
semi = Tok.semi lexer
integer = Tok.integer lexer
float = Tok.float lexer
whiteSpace = Tok.whiteSpace lexer
stringLiteral = Tok.stringLiteral lexer
colon = Tok.colon lexer
commaSep = Tok.commaSep lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
parens = Tok.parens lexer
braces = Tok.braces lexer
