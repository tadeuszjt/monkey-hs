module Parser where

import Lexer
import AST
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Expr as Ex

-- Expression Parsers

ident :: Parser Expr
ident =
	fmap Ident identifier

litFlt :: Parser Expr
litFlt =
	fmap LitFlt $
		try $ fmap fromInteger integer
		<|> float

litBool :: Parser Expr
litBool =
	fmap LitBool $
		try (do {reserved "true"; return True})
		<|> (do {reserved "false"; return False})

litFunc :: Parser Expr
litFunc = do
	reserved "fn"
	args <- parens $ commaSep identifier
	blck <- block
	return $ LitFunc args blck

call :: Parser Expr
call = do
	(Ident name) <- ident
	args <- parens $ commaSep expr
	return $ Call name args

ifExpr :: Parser Expr
ifExpr = do
	reserved "if"
	cnd <- expr
	ex1 <- braces expr
	reserved "else"
	ex2 <- braces expr
	return $ IfExpr cnd ex1 ex2

table = [
	[Ex.Infix (reservedOp "*" >> return (Infix Times)) Ex.AssocLeft,
	 Ex.Infix (reservedOp "/" >> return (Infix Divide)) Ex.AssocLeft,
	 Ex.Infix (reservedOp "%" >> return (Infix Mod)) Ex.AssocLeft],
	[Ex.Infix (reservedOp "+" >> return (Infix Plus)) Ex.AssocLeft,
	 Ex.Infix (reservedOp "-" >> return (Infix Minus)) Ex.AssocLeft],
	[Ex.Infix (reservedOp "<" >> return (Infix LThan)) Ex.AssocLeft,
	 Ex.Infix (reservedOp ">" >> return (Infix GThan)) Ex.AssocLeft]
	]

term =
	try call
	<|> try ident
	<|> try ifExpr
	<|> try litFlt
	<|> try litBool
	<|> try litFunc
	<|> parens expr

expr :: Parser Expr
expr =
	Ex.buildExpressionParser table term <?> "expression"

-- Statement Parsers

ret :: Parser Stmt
ret = do
	reserved "return"
	fmap Return expr

assign :: Parser Stmt
assign = do
	(Ident name) <- ident
	reservedOp ":="
	fmap (Assign name) expr

set :: Parser Stmt
set = do
	(Ident name) <- ident
	reservedOp "="
	fmap (Set name) expr

ifStmt :: Parser Stmt
ifStmt = do
	reserved "if"
	cnd <- expr
	blk <- block
	return $ IfStmt cnd blk

while :: Parser Stmt
while = do
	reserved "while"
	cnd <- expr
	blk <- block
	return $ While cnd blk
	

statement :: Parser Stmt
statement =
	try block <|>
	try ifStmt <|>
	try exprStmt <|>
	try while <|> do
		s <- try assign
			<|> try ret
			<|> try set
		semi
		return s

exprStmt :: Parser Stmt
exprStmt = do
	exp <- expr
	semi
	return $ ExprStmt exp

block :: Parser Stmt
block =
	fmap Block . braces . many $ statement

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
