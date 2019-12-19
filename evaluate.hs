module Evaluate where

import Control.Monad.State
import Control.Monad.Trans.Maybe

import qualified AST as S
import qualified Data.Map as Map

data Object
	= OFlt Double
	| OInt Integer
	| OBool Bool
	| OFunc S.Expr
	| OCall String [S.Expr]
	deriving Show

type Env = [Map.Map String Object]

emptyEnv :: Env
emptyEnv =
	[Map.empty]

type Eval a = StateT Env (MaybeT IO) a

liftMaybe :: Maybe a -> Eval a
liftMaybe m =
	lift $ MaybeT (pure m)

err :: String -> Eval a
err str = do
	liftIO $ putStrLn str
	liftMaybe Nothing

envPush :: Eval ()
envPush = do
	env <- get
	put $ (Map.empty):env

envPop :: Eval ()
envPop =
	get >>= put . tail

envLookup :: String -> Env -> Maybe Object
envLookup name [] =
	Nothing
envLookup name (x:xs) =
	case Map.lookup name x of
		Nothing -> envLookup name xs
		Just ob -> Just ob 

envGet :: String -> Eval Object
envGet name = do
	env <- get 
	case envLookup name env of
		Nothing -> err ("get: " ++ name ++ " does not exist")
		Just ob -> return ob

envSet :: String -> Object -> Eval ()
envSet name ob =
	get >>= envSet' name ob []
	where
		envSet' name _ _ []       = err $ "set: " ++ name ++ " does not exist"
		envSet' name ob ps (x:xs) = case Map.lookup name x of
			Just _  -> put $ ps ++ (Map.insert name ob x):xs
			Nothing -> envSet' name ob (ps ++ [x]) xs
	
envAdd :: String -> Object -> Eval ()
envAdd name ob = do
	(x:xs) <- get
	case Map.lookup name x of
		Nothing -> put $ (Map.insert name ob x):xs
		Just _  -> err $ name ++ " already defined"
		
-- main functions

execEval :: Eval a -> IO ()
execEval ev = do
	_ <- runMaybeT $ execStateT ev emptyEnv
	return ()

evProg :: S.Program -> Eval ()
evProg p = do
	mapM_ evStmt p
	return ()

-- statement functions

evStmt :: S.Stmt -> Eval ()
evStmt stmt = case stmt of
	S.Assign name expr -> evExpr expr >>= envAdd name
	S.Set name expr    -> evExpr expr >>= envSet name
	S.ExprStmt expr    -> evExpr expr >>= liftIO . print
	S.Block _          -> evBlock stmt
	S.IfStmt _ _       -> evIfStmt stmt
	S.While _ _        -> evWhile stmt
	_                  -> err $ show stmt ++ " unhandled"

evFnBlock :: S.Stmt -> Eval [Object]
evFnBlock (S.Block [S.Return expr]) =
	fmap (:[]) (evExpr expr)
evFnBlock (S.Block (x:xs)) = do
	evStmt x
	evFnBlock (S.Block xs)

evBlock :: S.Stmt -> Eval ()
evBlock (S.Block xs) = do
	envPush
	mapM_ evStmt xs
	envPop

evIfStmt :: S.Stmt -> Eval ()
evIfStmt (S.IfStmt cnd blk) = do
	cnd' <- evExpr cnd
	case cnd' of
		OBool True  -> evBlock blk
		OBool False -> return ()
		_        -> err "if cnd not bool"

evWhile :: S.Stmt -> Eval ()
evWhile stmt@(S.While cnd blk) = do
	cnd' <- evExpr cnd
	case cnd' of
		OBool True  -> evBlock blk >> evWhile stmt 
		OBool False -> return ()
		_        -> err "while cnd not bool"

-- expression functions

evExpr :: S.Expr -> Eval Object
evExpr expr = case expr of
	S.LitFlt f    -> return (OFlt f)
	S.LitFunc _ _ -> return (OFunc expr)
	S.Ident name  -> envGet name
	S.Infix _ _ _ -> evInfix expr
	S.Call name _ -> do
		ret <- evCall expr
		case ret of
			[ob] -> return ob
			_    -> err (name ++ " did not return an expression")
	_ -> err (show expr ++ ": unknown expr")


evInfix :: S.Expr -> Eval Object
evInfix (S.Infix op e1 e2) = do
	e1' <- evExpr e1
	e2' <- evExpr e2
	case (e1', e2', op) of
		(OFlt x, OFlt y, S.Plus) -> return $ OFlt (x + y)
		(OFlt x, OFlt y, S.Minus) -> return $ OFlt (x - y)
		(OFlt x, OFlt y, S.GThan) -> return $ OBool (x > y)
		(OFlt x, OFlt y, S.LThan) -> return $ OBool (x < y)
		(OFlt x, OFlt y, S.LThan) -> return $ OBool (x < y)

evCall :: S.Expr -> Eval [Object]
evCall (S.Call name exprs) = do
	ob <- envGet name
	let n = length exprs
	case ob of
		OFunc (S.LitFunc args blk) -> if length args /= n 
			then err $ name ++ " does not take " ++ (show n) ++ " args"
			else do
				exprs' <- mapM evExpr exprs
				envPush
				mapM_ (\(a, e) -> envAdd a e) $ zip args exprs'
				ret <- evFnBlock blk
				envPop
				return ret
		_ -> err (name ++ " is not a function")


 --type Env = Map.Map String Object 
 --type Eval a = StateT Env (Either String) a
 --
 --evInfix :: S.Expr -> Eval Object
 --evInfix (S.Infix op e1 e2) = do
 --	e1' <- evExpr e1
 --	e2' <- evExpr e2
 --	let tup = (e1', e2', op)
 --	case tup of
 --		(OFlt x, OFlt y, S.Plus) -> return $ OFlt (x + y)
 --		(OFlt x, OFlt y, S.Minus) -> return $ OFlt (x - y)
 --		(OFlt x, OFlt y, S.Times) -> return $ OFlt (x * y)
 --		(OFlt x, OFlt y, S.Divide) -> return $ OFlt (x / y)
 --		(OFlt x, OFlt y, S.LThan) -> return $ OBool (x < y)
 --		(OFlt x, OFlt y, S.GThan) -> return $ OBool (x > y)
 --		_ -> lift $ Left ("invalid infix: " ++ show tup)
 --
 --
 --evExpr :: S.Expr -> Eval Object
 --evExpr e = case e of
 --	S.LitOFlt f ->
 --		return $ OFlt f
 --
 --	S.LitBool b ->
 --		return $ OBool b
 --
 --	S.Ident name -> do
 --		env <- get
 --		case Map.lookup name env of
 --			Just ob -> return ob
 --			Nothing -> lift $ Left (name ++ " not found")
 --
 --	S.Infix _ _ _ ->
 --		evInfix e
 --	
 --	S.LitFunc exprs stmt ->
 --		return $ OFunc e
 --
 --	S.Call (S.Ident name) exs -> do
 --		env <- get
 --		case Map.lookup name env of
 --			Just (Func (S.LitFunc args blk)) -> do
 --				exs' <- mapM evExpr exs
 --				let symTab = Map.fromList $ zip args exs'
 --				case evalStateT (evFnBlock blk) symTab of
 --					Right [ob] -> return ob
 --					_ -> lift $ Left "funciont error" 
 --				
 --			_ -> lift $ Left ("function: " ++ name ++ " not found")
 --	
 --	S.IfExpr cnd exp1 exp2 -> do
 --		cnd' <- evExpr cnd
 --		case cnd' of
 --			Bl True -> evExpr exp1
 --			Bl False -> evExpr exp2
 --			_ -> lift $ Left "if condition not bool"
 --
 --
 --evFnBlock :: S.Stmt -> Eval [Object]
 --evFnBlock (S.Block xs) = case xs of
 --	[] ->
 --		return []
 --
 --	[(S.ExprStmt expr)] ->
 --		fmap (:[]) $ evExpr expr
 --
 --	((S.Return expr):_) ->
 --		fmap (:[]) $ evExpr expr
 --
 --	(x:xs) -> do
 --		evStmt x
 --		evFnBlock $ S.Block xs
 --
 --
 --evStmt :: S.Stmt -> Eval ()
 --evStmt stmt = case stmt of
 --	S.Assign (S.Ident name) expr -> do
 --		exp <- evExpr expr
 --		env <- get
 --		case Map.lookup name env of
 --			Just ob -> lift $ Left (name ++ " already defined")
 --			Nothing -> put $ Map.insert name exp env
 --
 --	S.Set (S.Ident name) expr -> do
 --		exp <- evExpr expr
 --		env <- get
 --		case Map.lookup name env of
 --			Just ob -> put $ Map.insert name exp env
 --			Nothing -> lift $ Left "set: name not found"
 --
 --	S.IfStmt cnd (S.Block blk) -> do
 --		cnd' <- evExpr cnd
 --		case cnd' of
 --			Bl True -> mapM_ evStmt blk
 --			Bl False -> return ()
 --			_ -> lift $ Left "if condition not bool"
 --
 --	S.While cnd (S.Block blk) -> do
 --		cnd' <- evExpr cnd
 --		case cnd' of
 --			Bl True -> do
 --				mapM_ evStmt blk
 --				evStmt stmt
 --			Bl False -> return ()
 --			_ -> lift $ Left "while condition not bool"
 --
 --	S.Block xs ->
 --		mapM_ evStmt xs
 --
 --
 --evProg :: S.Program -> Eval ()
 --evProg =
 --	mapM_ evStmt
 --
 --
 --runEvState e = runStateT e []
