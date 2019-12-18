module Parser where

import Lexer
import AST
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Expr as Ex

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
	args <- parens $ commaSep identifier
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
	name <- ident
	reservedOp ":="
	fmap (Assign name) expression

statement :: Parser Statement
statement = do
	try block <|> do
		s <- try assign <|> ret
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
