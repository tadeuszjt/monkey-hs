module AST where

data Op
	= Plus
	| Minus
	| Times
	| Divide
	| LThan
	| GThan
	| Mod
	deriving (Show, Eq)

data Expr
	= LitFlt Double
	| LitBool Bool
	| Ident String
	| Call String [Expr]
	| LitFunc [String] Stmt
	| Infix Op Expr Expr
	| IfExpr Expr Expr Expr
	deriving Show

data Stmt
	= Assign String Expr
	| Set String Expr
	| IfStmt Expr Stmt
	| While Expr Stmt
	| Return Expr
	| Block [Stmt]
	| ExprStmt Expr
	deriving (Show)

type Program = [Stmt]
