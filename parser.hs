module Parser where

import Lexer
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Expr as Ex

data Op
	= Plus
	| Minus
	| Times
	| Divide
	deriving (Show, Eq)

data Expr
	= Ident String
	| Infix Op Expr Expr
	| Call Expr [Expr]
	| LitFunc [Expr] Statement
	| LitFloat Double
	deriving Show

data Statement
	= Let Expr Expr
	| Return Expr
	| Block [Statement]
	deriving (Show)

type Program = [Statement]

-- Expression Parsers

ident :: Parser Expr
ident =
	fmap Ident identifier

litFloat :: Parser Expr
litFloat =
	fmap LitFloat $
		try $ fmap fromInteger integer
		<|> float

litFunc :: Parser Expr
litFunc = do
	reserved "fn"
	args <- parens $ commaSep ident
	blck <- block
	return $ LitFunc args blck

call :: Parser Expr
call = do
	name <- ident
	args <- parens $ commaSep expression
	return $ Call name args

table = [
	[Ex.Infix (reservedOp "*" >> return (Infix Times)) Ex.AssocLeft,
	 Ex.Infix (reservedOp "/" >> return (Infix Divide)) Ex.AssocLeft],
	[Ex.Infix (reservedOp "+" >> return (Infix Plus)) Ex.AssocLeft,
	 Ex.Infix (reservedOp "-" >> return (Infix Minus)) Ex.AssocLeft]
	]

term =
	try call
	<|> try ident
	<|> try litFloat 
	<|> try litFunc
	<|> parens expression

expression :: Parser Expr
expression =
	Ex.buildExpressionParser table term <?> "expression"

-- Statement Parsers

ret :: Parser Statement
ret = do
	reserved "return"
	fmap Return expression

assign :: Parser Statement
assign = do
	reserved "let"
	name <- ident
	reservedOp "="
	fmap (Let name) expression

statement :: Parser Statement
statement = do
	s <- try assign
	     <|> ret
	semi
	return s

block :: Parser Statement
block =
	fmap Block $ braces (many statement)
	
program :: Parser Program
program = do
	s <- many statement
	eof
	return s

-- Parse Function

parseStr :: String -> IO Program
parseStr str = do
	return $ case parse program "" str of
		Left e -> error $ show e
		Right r -> r
