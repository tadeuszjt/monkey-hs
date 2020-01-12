module AST where

type Name = String

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
    = Int Int
    | Bool Bool
    | String String
    | Func [Name] Stmt
    | Ident Name 
    | Call Expr [Expr]
    | Infix Op Expr Expr
    | Array [Expr]
    | Subscript Expr Expr
    deriving (Show, Eq)

data Stmt
    = Assign Name Expr
    | Set Name Expr
    | IfStmt Expr Stmt (Maybe Stmt)
    | While Expr Stmt
	| Block [Stmt]
    | Return Expr
    | ExprStmt Expr
    deriving (Show, Eq)

type Program = [Stmt]
