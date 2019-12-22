module Parser where

import Lexer
import AST
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Expr as Ex

-- Expression Parsers

ident :: Parser Expr
ident =
	fmap Ident identifier

litInt :: Parser Expr
litInt =
	return . EInt . fromInteger =<< integer

litBool :: Parser Expr
litBool =
	try (reserved "true" >> return (EBool True))
	<|> (reserved "false" >> return (EBool False))

litString :: Parser Expr
litString =
	return . EString =<< stringLiteral

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
	
array :: Parser Expr
array =
	fmap Array $ brackets (commaSep expr)


-- recursive left grammar postfix
recPostfix p = Ex.Postfix $ chainl1 p (return $ flip (.))
binary name op assoc = Ex.Infix (reservedOp name >> return (Infix op)) assoc

table = [
	[recPostfix (fmap (flip Subscript) $ brackets expr)],
	[binary "*" Times Ex.AssocLeft,
	 binary "/" Divide Ex.AssocLeft,
	 binary "%" Mod Ex.AssocLeft],
	[binary "+" Plus Ex.AssocLeft,
	 binary "-" Minus Ex.AssocLeft],
	[binary "<" LThan Ex.AssocLeft,
	 binary ">" GThan Ex.AssocLeft,
	 binary "==" EqEq Ex.AssocLeft,
	 binary "<=" LTEq Ex.AssocLeft,
	 binary ">=" GTEq Ex.AssocLeft],
	[binary "||" OrOr Ex.AssocLeft]
	]

term =
	try call
	<|> try ident
	<|> try litInt
	<|> try litBool
	<|> try litString
	<|> try litFunc
	<|> try array
	<|> parens expr

expr :: Parser Expr
expr =
	Ex.buildExpressionParser table term <?> "expression"

-- Statement Parsers

ret :: Parser Stmt
ret =
	reserved "return" >> fmap Return expr

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
	els <- optionMaybe $ reserved "else" >> (try ifStmt <|> block)
	return $ IfStmt cnd blk els

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
