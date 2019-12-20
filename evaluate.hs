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
	put . tail =<< get


envGet :: String -> Eval Object
envGet name = do
	get >>= envGet' name
	where
		envGet' name []     = err $ name ++ " does not exist"
		envGet' name (x:xs) = case Map.lookup name x of
			Just ob -> return ob
			Nothing -> envGet' name xs


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
	S.Assign name expr -> envAdd name =<< evExpr expr
	S.Set name expr    -> envSet name =<< evExpr expr
	S.ExprStmt expr    -> liftIO . print =<< evExpr expr
	S.Block s          -> envPush >> mapM_ evStmt s >> envPop
	S.IfStmt _ _       -> evIfStmt stmt
	S.While _ _        -> evWhile stmt
	_                  -> err $ show stmt ++ " not allowed here"


evFnBlock :: S.Stmt -> Eval [Object]
evFnBlock (S.Block blk) = case blk of
	[S.Return expr]   -> fmap (:[]) $ evExpr expr
	[S.ExprStmt expr] -> fmap (:[]) $ evExpr expr
	(x:xs)            -> evStmt x >> evFnBlock (S.Block xs)
	[]                -> return []


evIfStmt :: S.Stmt -> Eval ()
evIfStmt (S.IfStmt cnd blk) = do
	cnd' <- evExpr cnd
	case cnd' of
		OBool True  -> evStmt blk
		OBool False -> return ()
		_           -> err "if cnd not bool"


evWhile :: S.Stmt -> Eval ()
evWhile stmt@(S.While cnd blk) = do
	cnd' <- evExpr cnd
	case cnd' of
		OBool True  -> evStmt blk >> evWhile stmt 
		OBool False -> return ()
		_           -> err "while cnd not bool"

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
