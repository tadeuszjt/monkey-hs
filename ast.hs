module AST where

import qualified Lexer as L

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
    = Int       L.AlexPosn Int
    | Bool      L.AlexPosn Bool
    | String    L.AlexPosn String
    | Func      L.AlexPosn [Name] Stmt
    | Ident     L.AlexPosn Name 
    | Call      L.AlexPosn Expr [Expr]
    | Infix     L.AlexPosn Op Expr Expr
    | Array     L.AlexPosn [Expr]
    | Subscript L.AlexPosn Expr Expr
    deriving (Show, Eq)

data Stmt
    = Assign    L.AlexPosn Name Expr
    | Set       L.AlexPosn Name Expr
    | If        L.AlexPosn Expr Stmt (Maybe Stmt)
    | While     L.AlexPosn Expr Stmt
	| Block     L.AlexPosn [Stmt]
	| BlockExpr L.AlexPosn Expr
    | Return    L.AlexPosn Expr
    | ExprStmt  Expr
    deriving (Show, Eq)

type Program = [Stmt]

exprPosn :: Expr -> L.AlexPosn
exprPosn exp = case exp of
	Int p _         -> p
	String p _      -> p
	Func p _ _      -> p
	Ident p _       -> p
	Call p _ _      -> p
	Infix p _ _ _   -> p
	Subscript p _ _ -> p

stmtPosn :: Stmt -> L.AlexPosn
stmtPosn stmt = case stmt of
	Assign p _ _  -> p
	Set p _ _     -> p
	If p _ _ _    -> p
	While p _ _   -> p
	Block p _     -> p
	BlockExpr p _ -> p
	Return p _    -> p
	ExprStmt e    -> exprPosn e
