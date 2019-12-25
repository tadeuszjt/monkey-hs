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
	| Print [Val]
	| LoopBegin   | LoopBreak | LoopEnd
	| IfBegin Val | IfEnd     | IfElse
	deriving Show

data BlockState
	= BlockState {
		symTab  :: Map String Val -- always ident
		}
	deriving Show

emptyBlock = BlockState Map.empty

data CmpState
	= CmpState {
		idCount :: Int,
		blocks  :: [BlockState], -- stack
		opns    :: [Opn]         -- top level operations
		}
	deriving Show

emptyCmpState = CmpState {
	idCount = 0,
	blocks  = [emptyBlock],
	opns    = []
	}

type Cmp a = StateT CmpState (Either String) a

err :: String -> Cmp a
err str =
	lift $ (Left str)

uniqueId :: Cmp String
uniqueId = do
	count <- gets idCount
	modify $ \s -> s {idCount = count + 1}
	return $ "v" ++ show count

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
	mapM_ cmpStmt astProg
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
	((TInt, TInt, A.LThan), TBool),
	((TInt, TInt, A.GThan), TBool),
	((TBool, TBool, A.OrOr), TBool)
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


cmpStmt :: A.Stmt -> Cmp ()
cmpStmt stmt = case stmt of
	A.Assign name expr -> do
		val <- cmpExpr expr
		ident <- uniqueId
		assignVar name (VIdent ident $ typeOf val)
		addOpn $ Assign ident val

	A.Set name expr -> do
		(VIdent cname typ) <- getVal name
		val <- cmpExpr expr
		if typeOf val == typ
		then addOpn $ Set cname val
		else err $ show typ ++ " does not match"

	A.ExprStmt (A.Call "print" args) -> do
		vals <- mapM cmpExpr args
		addOpn $ Print vals

	A.While cnd (A.Block blk) -> do
		addOpn $ LoopBegin
		val <- cmpExpr cnd

		_ <- case typeOf val of
			TBool -> return ()
			_     -> err $ "while cnd not bool"

		addOpn $ IfBegin val
		addOpn (LoopBreak)
		addOpn IfEnd

		cmpPush
		mapM_ cmpStmt blk
		cmpPop
		addOpn LoopEnd

	A.IfStmt cnd (A.Block blk) els -> do
		val <- cmpExpr cnd
		case typeOf val of
			TBool -> return ()
			_     -> err $ "if cnd not bool"

		addOpn $ IfBegin val
		cmpPush
		mapM_ cmpStmt blk
		cmpPop
		addOpn IfEnd

		case els of
			Nothing -> return ()
			Just x  -> addOpn IfElse >> cmpStmt x

	_ -> err $ "invalid top stmt: " ++ show stmt

