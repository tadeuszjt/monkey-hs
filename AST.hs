module AST where

import qualified Lexer as L

type Name = String
type Posn = L.AlexPosn
type AST  = [Stmt]


data Op
    = Plus
    | Minus
    | Times
	| Divide
    | Mod
    | LT
    | GT
    | LTEq
    | GTEq
    | EqEq
    | OrOr
    deriving (Show, Eq, Ord)


data Expr
    = Int       Posn Int
    | Bool      Posn Bool
    | String    Posn String
    | Ident     Posn Name 
    | Func      Posn [Name] Stmt
    | Call      Posn Expr   [Expr]
    | Infix     Posn Op     Expr   Expr
    | Subscript Posn Expr   Expr
    | Array     Posn [Expr]
    deriving (Show, Eq)


data Stmt
    = Assign    Posn Name   Expr
    | Set       Posn Name   Expr
    | Return    Posn Expr
    | If        Posn Expr   Stmt (Maybe Stmt)
    | While     Posn Expr   Stmt
	| Print     Posn [Expr]
	| Block     [Stmt]
	| BlockExpr Expr
    | ExprStmt  Expr
    deriving (Show, Eq)


exprPosn :: Expr -> Posn
exprPosn exp = case exp of
	Int p _         -> p
	Bool p _        -> p
	String p _      -> p
	Func p _ _      -> p
	Ident p _       -> p
	Call p _ _      -> p
	Infix p _ _ _   -> p
	Array p _       -> p
	Subscript p _ _ -> p


stmtPosn :: Stmt -> Posn
stmtPosn stmt = case stmt of
	Assign p _ _  -> p
	Set p _ _     -> p
	Return p _    -> p
	If p _ _ _    -> p
	While p _ _   -> p
	BlockExpr e   -> exprPosn e
	ExprStmt e    -> exprPosn e
