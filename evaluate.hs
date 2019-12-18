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
	| Func S.Expr
	deriving Show

type Env = [(String, Object)]
type Eval a = StateT Env (Either String) a

evExpr :: S.Expr -> Eval Object
evExpr e = case e of
	S.LitFloat f ->
		return $ Flt f

	S.Ident name -> do
		env <- get
		case lookup name env of
			Just ob -> return ob
			Nothing -> lift $ Left (name ++ " not found")

	S.Infix op e1 e2 -> do
		e1' <- evExpr e1
		e2' <- evExpr e2
		case (e1', op, e2') of
			(Flt x, S.Plus, Flt y)   -> return $ Flt (x + y)
			(Flt x, S.Minus, Flt y)  -> return $ Flt (x - y)
			(Flt x, S.Times, Flt y)  -> return $ Flt (x * y)
			(Flt x, S.Divide, Flt y) -> return $ Flt (x / y)
			_ -> lift $ Left "invalid operator expression"
	
	S.LitFunc exprs stmt ->
		return $ Func e

	S.Call (S.Ident name) exs -> do
		env <- get
		case lookup name env of
			Just (Func (S.LitFunc args blk)) -> do
				exs' <- mapM evExpr exs
				let symTab = zip args exs'
				let obs = evalStateT (evFnBlock blk) symTab 
				case obs of
					Right [ob] -> return ob
					_ -> lift $ Left "funciont error" 
				
			_ -> lift $ Left ("function: " ++ name ++ " not found")


evFnBlock :: S.Statement -> Eval [Object]
evFnBlock (S.Block xs) = case xs of
	[]  -> return []

	[x] -> case x of
		S.Return expr -> do
			ob <- evExpr expr
			return [ob]
		_ -> lift $ Left "expecting return"

	(x:xs) -> do
		evStmt x
		evFnBlock (S.Block xs)


evStmt :: S.Statement -> Eval ()
evStmt stmt = case stmt of
	S.Assign (S.Ident name) expr -> do
		exp <- evExpr expr
		env <- get
		case lookup name env of
			Just ob -> lift $ Left (name ++ " already defined")
			Nothing -> put $ (name, exp):env

	S.Block xs ->
		mapM_ evStmt xs

evProg :: S.Program -> Eval ()
evProg =
	mapM_ evStmt

runEvState e = runStateT e []
