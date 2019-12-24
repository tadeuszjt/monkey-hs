module Compiler where

import Control.Monad.State
import Data.Map as Map
import Data.List as List
import qualified AST as A

-- expression types

data Type
	= TInt
	| TBool
	deriving (Show, Eq)

data Val
	= VInt Int
	| VBool Bool
	| VIdent String Type
	| VInfix A.Op Val Val Type
	deriving Show

typeOf :: Val -> Type
typeOf v = case v of
	VInt _         -> TInt
	VBool _        -> TBool
	VIdent _ t     -> t
	VInfix _ _ _ t -> t

-- CmpState

data Opn
	= Assign String Val
	| Set String Val
	| Print Val
	| While Val
	deriving Show

data BlockState
	= BlockState {
		idCount :: Int,
		symTab  :: Map String Val -- always ident
		}
	deriving Show

emptyBlock = BlockState 0 Map.empty

data CmpState
	= CmpState {
		blocks :: [BlockState], -- stack
		opns   :: [Opn]         -- top level operations
		}
	deriving Show

emptyCmpState = CmpState {
	blocks = [emptyBlock],
	opns   = []
	}

type Cmp a = StateT CmpState (Either String) a

err :: String -> Cmp a
err str =
	lift $ (Left str)

uniqueId :: Cmp String
uniqueId = do
	(block:rest) <- gets blocks
	modify $ \s -> s {
		blocks = (block { idCount = idCount block + 1 }):rest
	}
	return $ "v" ++ show (idCount block)

cmpPush :: Cmp ()
cmpPush = do
	xs <- gets blocks
	modify $ \s -> s {blocks = emptyBlock:xs}

cmpPop :: Cmp ()
cmpPop = do
	xs <- gets blocks
	modify $ \s -> s {blocks = tail xs}

-- lookup ast name, return compiled name, type
getVal :: String -> Cmp Val -- always ident
getVal name =
	getVal' name =<< gets blocks
	where
		getVal' name []     = err $ name ++ " does not exist"
		getVal' name (x:xs) = case Map.lookup name (symTab x) of
			Just x  -> return x
			Nothing -> getVal' name xs


assignVar :: String -> Val -> Cmp ()
assignVar name val@(VIdent _ _) = do
	(block:rest) <- gets blocks
	let st = symTab block
	new <- case Map.lookup name st of
		Just _  -> err $ name ++ " already defined"
		Nothing -> return $ Map.insert name val st
	modify $ \s -> s { blocks = (block { symTab = new }):rest }


addOpn :: Opn -> Cmp ()
addOpn opn = do
	ops <- gets opns
	modify $ \s -> s { opns = ops ++ [opn] }
		

-- program output

data Prog
	= Prog {
		fns :: [()],
		ops :: [Opn]
		}
	deriving Show

evalCmp :: Cmp Prog -> Either String Prog
evalCmp cmp =
	evalStateT cmp emptyCmpState


cmpProg :: A.Program -> Cmp Prog
cmpProg astProg = do
	mapM_ cmpTopStmt astProg
	ops <- gets opns
	return (Prog [] ops)


cmpExpr :: A.Expr -> Cmp Val
cmpExpr expr = case expr of
	A.EInt i      -> return (VInt i)
	A.EBool b     -> return (VBool b)
	A.Ident s     -> return =<< getVal s
	A.Infix _ _ _ -> cmpInfix expr
	_             -> err $ "cmpExpr: " ++ show expr


infixTable = [
	((TInt, TInt, A.Plus), TInt),
	((TInt, TInt, A.Minus), TInt),
	((TInt, TInt, A.Times), TInt),
	((TInt, TInt, A.Divide), TInt),
	((TInt, TInt, A.LThan), TBool)
	]

cmpInfix :: A.Expr -> Cmp Val
cmpInfix (A.Infix op e1 e2) = do
	v1 <- infVal =<< cmpExpr e1
	v2 <- infVal =<< cmpExpr e2 

	typ <- case List.lookup (typeOf v1, typeOf v2, op) infixTable of
		Just t -> return t
		Nothing -> err $ "no infix for " ++ show (op, e1, e2)

	return $ VInfix op v1 v2 typ
	where
		infVal val = case val of
			VInt _     -> return val
			VBool _    -> return val
			VIdent _ _ -> return val
			_ -> do
				cname <- uniqueId
				addOpn $ Assign cname val
				return $ VIdent cname (typeOf val)


cmpTopStmt :: A.Stmt -> Cmp ()
cmpTopStmt stmt = case stmt of
	A.Assign name expr -> do
		val <- cmpExpr expr
		ident <- uniqueId
		assignVar name (VIdent ident $ typeOf val)
		addOpn $ Assign ident val

	A.Set name (A.EInt i) -> do
		(VIdent cname typ) <- getVal name
		case typ of
			TInt -> addOpn $ Set cname (VInt i)
			_    -> err $ show typ ++ " not int"

	A.ExprStmt (A.Call "print" [arg]) -> do
		val <- cmpExpr arg
		addOpn $ Print val


	A.While cnd (A.Block blk) -> do
		val <- cmpExpr cnd
		_ <- case typeOf val of
			TBool -> return ()
			_     -> err $ "while cnd not bool"

		addOpn $ While val
		cmpPush
		cmpPop

	_                        -> err $ "invalid top stmt: " ++ show stmt

