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
	| LTEq
	| GTEq
	deriving (Show, Eq)

data Expr
	= EInt Int
	| EBool Bool
	| EString String
	| Ident String
	| Call String [Expr]
	| LitFunc [String] Stmt
	| Infix Op Expr Expr
	deriving Show

data Stmt
	= Assign String Expr
	| Set String Expr
	| IfStmt Expr Stmt (Maybe Stmt)
	| While Expr Stmt
	| Return Expr
	| Block [Stmt]
	| ExprStmt Expr
	deriving (Show)

type Program = [Stmt]
