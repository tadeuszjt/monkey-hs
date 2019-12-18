module AST where

data Op
	= Plus
	| Minus
	| Times
	| Divide
	| LThan
	| GThan
	deriving (Show, Eq)

data Expr
	= LitFlt Double
	| LitBool Bool
	| Ident String
	| Call Expr [Expr]
	| LitFunc [String] Stmt
	| Infix Op Expr Expr
	| IfExpr Expr Expr Expr
	deriving Show

data Stmt
	= Assign Expr Expr
	| Set Expr Expr
	| IfStmt Expr Stmt
	| While Expr Stmt
	| Return Expr
	| Block [Stmt]
	| ExprStmt Expr
	deriving (Show)

type Program = [Stmt]
