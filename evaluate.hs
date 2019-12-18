module Evaluate where

import Object
import qualified AST as S
import Control.Monad.State

data Object
	= Flt Double
	| Block {
		body :: [S.Statement]
		}
	| Call {
		name :: String,
		args :: [S.Expr]
		}
	deriving Show

type Env = [(String, Object)]
type Eval a = StateT Env (Either String) a

evExpr :: S.Expr -> Eval Object
evExpr (S.LitFloat f) =
	return $ Flt f
evExpr (S.Ident name) = do
	env <- get
	case lookup name env of
		Just ob -> return ob
		Nothing -> lift $ Left (name ++ " not found")

evStmt :: S.Statement -> Eval ()
evStmt (S.Assign (S.Ident name) expr) = do
	env <- get
	exp <- evExpr expr
	case lookup name env of
		Just ob -> lift $ Left (name ++ " already defined")
		Nothing -> put $ env ++ [(name, exp)]

runEvState e = runStateT e []
