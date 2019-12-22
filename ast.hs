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
	| OrOr
	deriving (Show, Eq)

data Expr
	= EInt Int
	| EBool Bool
	| EString String
	| Func [String] Stmt
	| Ident String
	| Call String [Expr]
	| Infix Op Expr Expr
	| Array [Expr]
	| Subscript Expr Expr
	deriving (Show, Eq)

data Stmt
	= Assign String Expr
	| Set String Expr
	| IfStmt Expr Stmt (Maybe Stmt)
	| While Expr Stmt
	| Return Expr
	| Block [Stmt]
	| ExprStmt Expr
	deriving (Show, Eq)

type Program = [Stmt]
