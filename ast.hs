module AST where

data Op
	= Plus
	| Minus
	| Times
	| Divide
	deriving (Show, Eq)

data Expr
	= LitFloat Double
	| Ident String
	| Call Expr [Expr]
	| LitFunc [Expr] Statement
	| Infix Op Expr Expr
	deriving Show

data Statement
	= Assign Expr Expr
	| Return Expr
	| Block [Statement]
	deriving (Show)

type Program = [Statement]
