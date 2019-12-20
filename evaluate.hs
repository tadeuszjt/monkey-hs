module Evaluate where

import Control.Monad.State
import Control.Monad.Trans.Maybe

import qualified AST as S
import qualified Data.Map as Map

data Object
	= OInt Int
	| OBool Bool
	| OString String
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

check :: Bool -> String -> Eval ()
check b s =
	if b
	then return ()
	else err s

suc :: a -> Eval a
suc =
	liftMaybe . Just

err :: String -> Eval a
err str =
	liftIO (putStrLn str) >> liftMaybe Nothing


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
		

noReturn :: String -> Eval (Maybe Object) -> Eval ()
noReturn str ev = do
	ret <- ev
	case ret of
		Just _  -> err $ str ++ " shoudld't return"
		Nothing -> return ()


-- main functions

execEval :: Eval a -> IO ()
execEval ev = do
	_ <- runMaybeT $ execStateT ev emptyEnv
	return ()


evProg :: S.Program -> Eval ()
evProg p =
	mapM_ evTopStmt p >> return ()


-- statement functions

evTopStmt :: S.Stmt -> Eval ()
evTopStmt stmt = case stmt of
	S.Assign name expr -> envAdd name =<< evExpr expr
	S.Set name expr    -> envSet name =<< evExpr expr
	S.ExprStmt _       -> evExprStmt stmt
	S.While _ _        -> evWhile stmt >> return ()
	S.Block _          -> noReturn "block" $ do {envPush; ret <- evBlock stmt; envPop; return ret}
	_ -> err $ show stmt ++ " not allowed in top level"


evBlock :: S.Stmt -> Eval (Maybe Object)
evBlock (S.Block [x]) = case x of
	S.Return expr      -> return . Just =<< evExpr expr
	S.ExprStmt _       -> evExprStmt x >> return Nothing
	S.Assign name expr -> evExpr expr >>= envAdd name >> return Nothing
	S.Set name expr    -> evExpr expr >>= envSet name >> return Nothing
	S.While _ _        -> evWhile x
	S.IfStmt _ _ _     -> evIfStmt x
	_                  -> err $ show x ++ " not allowed in block"
evBlock (S.Block (x:xs)) = do
	ret <- evBlock (S.Block [x])
	case ret of
		Nothing -> evBlock (S.Block xs)
		Just _  -> return ret


evExprStmt :: S.Stmt -> Eval ()
evExprStmt (S.ExprStmt expr) = case expr of
	S.Call _ _         -> evCall expr >> return ()
	_                  -> err $ show expr ++ " not allowed as statement"


evWhile :: S.Stmt -> Eval (Maybe Object)
evWhile stmt@(S.While cnd blk) = do
	cnd' <- evExpr cnd
	bool <- case cnd' of
		(OBool b) -> liftMaybe $ Just b
		_         -> err "while cnd not bool"
	if bool == False
	then return Nothing
	else do
		envPush
		ret <- evBlock blk
		envPop
		case ret of
			Just _  -> return ret
			Nothing -> evWhile stmt


evIfStmt :: S.Stmt -> Eval (Maybe Object)
evIfStmt (S.IfStmt cnd blk els) = do
	cnd' <- evExpr cnd
	bool <- case cnd' of
		(OBool b) -> liftMaybe $ Just b
		_         -> err $ "if cnd not bool"
	if bool
	then evBlock blk
	else case els of
		Nothing               -> return Nothing
		Just (S.IfStmt c b e) -> evIfStmt (S.IfStmt c b e)
		Just (S.Block b)      -> evBlock (S.Block b)


-- expression functions

evExpr :: S.Expr -> Eval Object
evExpr expr = case expr of
	S.EInt i      -> return (OInt i)
	S.EBool b     -> return (OBool b)
	S.EString s   -> return (OString s)
	S.LitFunc _ _ -> return (OFunc expr)
	S.Ident name  -> envGet name
	S.Infix _ _ _ -> evInfix expr
	S.Call name _ -> do
		ret <- evCall expr
		case ret of
			Just ob -> return ob
			Nothing -> err (name ++ ": expecting return")
	_ -> err (show expr ++ ": unknown expr")


evInfix :: S.Expr -> Eval Object
evInfix (S.Infix op e1 e2) = do
	e1' <- evExpr e1
	e2' <- evExpr e2
	case (e1', e2') of
		(OInt x, OInt y) -> return $ case op of
			S.Plus   -> OInt (x + y)
			S.Minus  -> OInt (x - y)
			S.Times  -> OInt (x * y)
			S.Divide -> OInt (x `div` y)
			S.Mod    -> OInt (x `mod` y)
			S.LThan  -> OBool (x < y)
			S.GThan  -> OBool (x > y)
			S.EqEq   -> OBool (x == y)
			S.LTEq   -> OBool (x <= y)
			S.GTEq   -> OBool (x >= y)
		(OBool x, OBool y) -> return $ case op of
			S.OrOr -> OBool (x || y)


evCall :: S.Expr -> Eval (Maybe Object)
evCall (S.Call "print" args) =
	evBuiltin "print" args
evCall (S.Call "str" [expr]) = do
	expr' <- evExpr expr
	case expr' of
		OInt i    -> return $ Just (OString $ show i)
		OBool b   -> return $ Just (OString $ show b)
		OString _ -> return $ Just expr'
		_         -> err $ show expr ++ " not stringable"
evCall (S.Call "str" x) =
	err $ "str does not take " ++ show (length x) ++ " arguments"
evCall (S.Call name exprs) = do
	ob <- envGet name
	(S.LitFunc args blk) <- case ob of
		OFunc fn -> liftMaybe $ Just fn
		_        -> err $ "call: " ++ name ++ " isn't a function"
	let nexprs = length exprs
	check (length args == nexprs) $ name ++ " does not take " ++ show nexprs ++ " args"
	exprs' <- mapM evExpr exprs
	envPush
	mapM_ (\(n, o) -> envAdd n o) $ zip args exprs'
	ret <- evBlock blk
	envPop
	return ret


evBuiltin :: String -> [S.Expr] -> Eval (Maybe Object)
evBuiltin "print" args = case args of
	[]    -> liftIO (putStrLn "") >> return Nothing
	[arg] -> do
		arg' <- evExpr arg
		str <- case arg' of
			OInt i    -> suc $ show i
			OBool b   -> suc $ show b
			OString s -> suc s
			_         -> err $ "cannot print " ++ show arg'
		liftIO (putStrLn str)
		return Nothing
	(x:xs) ->
		evBuiltin "print" [x] >>
		liftIO (putStr ", ") >>
		evBuiltin "print" xs
	

