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
	| LitInt Integer
	| LitBool Bool
	| LitFunc [Expr] Statement
	deriving Show

data Statement
	= Let Expr Expr
	| Return Expr
	| Block [Statement]
	deriving (Show)

type Program = [Statement]


expression :: Parser Expr
expression = Ex.buildExpressionParser table term <?> "expression"

table = [
	[Ex.Infix (reserved "*" >> return (Infix Times)) Ex.AssocLeft,
	 Ex.Infix (reserved "/" >> return (Infix Divide)) Ex.AssocLeft],
	[Ex.Infix (reserved "+" >> return (Infix Plus)) Ex.AssocLeft,
	 Ex.Infix (reserved "-" >> return (Infix Minus)) Ex.AssocLeft]
	]
        
term =
	parens expression
	<|> ident
	<|> fmap LitInt integer
	<|> litFunc
	<|> (reserved "true" >> (return $ LitBool True))
	<|> (reserved "false" >> (return $ LitBool False))
	
block :: Parser Statement
block =
	fmap Block $ braces (many statement)
	
	
litFunc :: Parser Expr
litFunc = do
	reserved "fn"
	args <- parens $ commaSep ident
	blck <- block
	return $ LitFunc args blck

ident :: Parser Expr
ident =
	fmap Ident identifier

assign :: Parser Statement
assign = do
	reserved "let"
	name <- ident
	reserved "="
	expr <- expression
	return $ Let name expr
	
ret :: Parser Statement
ret = do
	reserved "return"
	expr <- expression
	return $ Return expr
	
statement :: Parser Statement
statement = do
	s <- try assign <|> ret
	semi
	return s
	
program :: Parser Program
program = do
	s <- many statement
	eof
	return s

parseStr :: String -> IO Program
parseStr str = do
	return $ case parse program "" str of
		Left e -> error $ show e
		Right r -> r
