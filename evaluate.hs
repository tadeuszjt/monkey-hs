module Evaluate where

import Object
import qualified AST as S
import Control.Monad.State

data Object
	= Flt Double
	| Block {
		env  :: Environment,
		body :: [S.Statement]
		}
	| Call {
		name :: String,
		args :: [S.Expr]
		}
	deriving Show

type Environment = [(String, Object)]
type EnvState = State Environment ObjParse
type ObjParse = Either String Object

emptyEnv :: Environment
emptyEnv = [("ten", Flt 10)]

runEnvState :: EnvState -> (ObjParse, Environment)
runEnvState e = runState e emptyEnv

newObj :: String -> Object -> EnvState
newObj name obj = do
	env <- get
	case lookup name env of
		Just _ -> return $ Left ("assign: " ++ name ++ " already defined")
		Nothing -> do
			put $ env ++ [(name, obj)]
			return $ Right obj

getObj :: String -> EnvState
getObj name = do
	env <- get
	return $ case lookup name env of
		Nothing -> Left $ name ++ " undeclared"
		Just ob -> Right ob
		
unlessE1 :: ObjParse -> (Object -> EnvState) -> EnvState
unlessE1 (Left e) _ =
	return $ Left e
unlessE1 (Right o) f =
	f o
		
unlessE2 :: ObjParse -> ObjParse -> ((Object, Object) -> EnvState) -> EnvState
unlessE2 ob1 ob2 f = case (ob1, ob2) of
	(Left e, _) -> return ob1
	(_, Left e) -> return ob2
	(Right ob1', Right ob2') -> f (ob1', ob2')
	
	
evalInfix :: S.Op -> Object -> Object -> EnvState
evalInfix op ob1 ob2 = return $ case (op, ob1, ob2) of
	(S.Plus, Flt f1, Flt f2) -> Right $ Flt (f1 + f2)
	(S.Minus, Flt f1, Flt f2) -> Right $ Flt (f1 - f2)
	(S.Times, Flt f1, Flt f2) -> Right $ Flt (f1 * f2)
	(S.Divide, Flt f1, Flt f2) -> Right $ Flt (f1 / f2)
	_ -> Left $ "Invalid infix: "
				++ show ob1 ++ " "
				++ show op  ++ " "
				++ show ob2

evalExpr :: S.Expr -> EnvState
evalExpr expr = case expr of
	S.LitFloat f -> return $ Right (Flt f)
	
	S.Ident name -> getObj name
	
	S.Infix op ex1 ex2 -> do
		ev1 <- evalExpr ex1
		ev2 <- evalExpr ex2
		unlessE2 ev1 ev2 $ \(e1, e2) -> evalInfix op e1 e2

evalStatement :: S.Statement -> EnvState
evalStatement s = case s of
	S.Assign (S.Ident name) expr -> do
		expr' <- evalExpr expr
		unlessE1 expr' $ \e -> newObj name e
	
evalProg :: S.Program -> EnvState
evalProg [x] =
	evalStatement x
evalProg (x:xs) = do
	ob <- evalStatement x
	unlessE1 ob $ \s -> evalProg xs

