module AST where

data Op
	= Times
	| Divide
	| Mod
	| Plus
	| Minus
	| LThan
	| GThan
	| EqEq
	deriving (Show, Eq)

data Expr
	= EInt Int
	| EBool Bool
	| EString String
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
