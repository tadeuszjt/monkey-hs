module Parser where

import Lexer
import qualified AST as S
import Control.Monad.Identity
import Text.Parsec.Prim
import Text.Parsec.Combinator
import qualified Text.ParserCombinators.Parsec.Expr as Ex

ident = fmap S.Ident identifier
litInt = return . S.Int =<< integer
litString = return . S.String =<< string

litBool :: Parser S.Expr
litBool =
    try (reserved "true" >> return (S.Bool True))
    <|> (reserved "false" >> return (S.Bool False))

litFunc :: Parser S.Expr
litFunc = do
    reserved "fn"
    args <- parens $ commaSep identifier
    blck <- block
    return $ S.Func args blck
    
array = fmap S.Array $ brackets (commaSep expr)


-- recursive left grammar postfix
recPostfix p = Ex.Postfix $ chainl1 p (return $ flip (.))
binary name op assoc = Ex.Infix (reservedOp name >> return (S.Infix op)) assoc

table = [
	[recPostfix (fmap (flip S.Call) (parens (commaSep expr))),
     recPostfix (fmap (flip S.Subscript) (brackets expr))],
    [binary "*" S.Times Ex.AssocLeft,
     binary "/" S.Divide Ex.AssocLeft,
     binary "%" S.Mod Ex.AssocLeft],
    [binary "+" S.Plus Ex.AssocLeft,
     binary "-" S.Minus Ex.AssocLeft],
    [binary "<" S.LThan Ex.AssocLeft,
     binary ">" S.GThan Ex.AssocLeft,
     binary "==" S.EqEq Ex.AssocLeft,
     binary "<=" S.LTEq Ex.AssocLeft,
     binary ">=" S.GTEq Ex.AssocLeft],
    [binary "||" S.OrOr Ex.AssocLeft]
    ]

term =
    try ident
    <|> try litInt
    <|> try litBool
    <|> try litString
    <|> try litFunc
    <|> try array
    <|> parens expr

expr :: Parser S.Expr
expr =
    Ex.buildExpressionParser table term <?> "expression"

-- Statement Parsers

ret = reserved "return" >> fmap S.Return expr

assign :: Parser S.Stmt
assign = do
    (S.Ident id) <- ident
    reservedOp ":="
    fmap (S.Assign id) expr

set :: Parser S.Stmt
set = do
    (S.Ident id) <- ident
    reservedOp "="
    fmap (S.Set id) expr

ifStmt :: Parser S.Stmt
ifStmt = do
    reserved "if"
    cnd <- expr
    blk <- block
    els <- optionMaybe $ reserved "else" >> (try ifStmt <|> block)
    return $ S.If cnd blk els

while :: Parser S.Stmt
while = do
    reserved "while"
    cnd <- expr
    blk <- block
    return $ S.While cnd blk
    

statement :: Parser S.Stmt
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

exprStmt :: Parser S.Stmt
exprStmt = do
    exp <- expr
    semi
    return $ S.ExprStmt exp

block = fmap S.Block $ braces (many statement)

program :: Parser S.Program
program = do
    s <- many statement
    eof
    return s

-- Parse Function

parseTokens :: [Token] -> IO S.Program
parseTokens str = do
    return $ case parse program "" str of
        Left e -> error $ show e
        Right r -> r
