module AST where

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
	= Assign Expr Expr
	| Return Expr
	| Block [Statement]
	deriving (Show)

type Program = [Statement]
