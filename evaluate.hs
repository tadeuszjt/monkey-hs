module Evaluate where

import qualified AST as S
import qualified Data.Map as Map
import Control.Monad.State

data Object
	= Flt Double
	| Bl Bool
	| Func S.Expr
	| Call String [S.Expr]
	deriving Show

emptyEnv = Map.empty

type Env = Map.Map String Object 
type Eval a = StateT Env (Either String) a

evInfix :: S.Expr -> Eval Object
evInfix (S.Infix op e1 e2) = do
	e1' <- evExpr e1
	e2' <- evExpr e2
	let tup = (e1', e2', op)
	case tup of
		(Flt x, Flt y, S.Plus) -> return $ Flt (x + y)
		(Flt x, Flt y, S.Minus) -> return $ Flt (x - y)
		(Flt x, Flt y, S.Times) -> return $ Flt (x * y)
		(Flt x, Flt y, S.Divide) -> return $ Flt (x / y)
		(Flt x, Flt y, S.LThan) -> return $ Bl (x < y)
		(Flt x, Flt y, S.GThan) -> return $ Bl (x > y)
		_ -> lift $ Left ("invalid infix: " ++ show tup)


evExpr :: S.Expr -> Eval Object
evExpr e = case e of
	S.LitFlt f ->
		return $ Flt f

	S.LitBool b ->
		return $ Bl b

	S.Ident name -> do
		env <- get
		case Map.lookup name env of
			Just ob -> return ob
			Nothing -> lift $ Left (name ++ " not found")

	S.Infix _ _ _ ->
		evInfix e
	
	S.LitFunc exprs stmt ->
		return $ Func e

	S.Call (S.Ident name) exs -> do
		env <- get
		case Map.lookup name env of
			Just (Func (S.LitFunc args blk)) -> do
				exs' <- mapM evExpr exs
				let symTab = Map.fromList $ zip args exs'
				case evalStateT (evFnBlock blk) symTab of
					Right [ob] -> return ob
					_ -> lift $ Left "funciont error" 
				
			_ -> lift $ Left ("function: " ++ name ++ " not found")
	
	S.IfExpr cnd exp1 exp2 -> do
		cnd' <- evExpr cnd
		case cnd' of
			Bl True -> evExpr exp1
			Bl False -> evExpr exp2
			_ -> lift $ Left "if condition not bool"


evFnBlock :: S.Stmt -> Eval [Object]
evFnBlock (S.Block xs) = case xs of
	[] ->
		return []

	[(S.ExprStmt expr)] ->
		fmap (:[]) $ evExpr expr

	((S.Return expr):_) ->
		fmap (:[]) $ evExpr expr

	(x:xs) -> do
		evStmt x
		evFnBlock $ S.Block xs


evStmt :: S.Stmt -> Eval ()
evStmt stmt = case stmt of
	S.Assign (S.Ident name) expr -> do
		exp <- evExpr expr
		env <- get
		case Map.lookup name env of
			Just ob -> lift $ Left (name ++ " already defined")
			Nothing -> put $ Map.insert name exp env

	S.Set (S.Ident name) expr -> do
		exp <- evExpr expr
		env <- get
		case Map.lookup name env of
			Just ob -> put $ Map.insert name exp env
			Nothing -> lift $ Left "set: name not found"

	S.IfStmt cnd (S.Block blk) -> do
		cnd' <- evExpr cnd
		case cnd' of
			Bl True -> mapM_ evStmt blk
			Bl False -> return ()
			_ -> lift $ Left "if condition not bool"

	S.While cnd (S.Block blk) -> do
		cnd' <- evExpr cnd
		case cnd' of
			Bl True -> do
				mapM_ evStmt blk
				evStmt stmt
			Bl False -> return ()
			_ -> lift $ Left "while condition not bool"

	S.Block xs ->
		mapM_ evStmt xs


evProg :: S.Program -> Eval ()
evProg =
	mapM_ evStmt


runEvState e = runStateT e []
