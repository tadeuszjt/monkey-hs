module Compiler where

import Control.Monad.State
import Data.Map as Map
import qualified AST as A

-- CmpState

data Type
	= TInt
	deriving Show

data Val
	= VInt Int
	| VIdent String
	deriving Show

data Opn
	= Assign String Val
	| Set String Val
	| Print Val
	deriving Show

data BlockState
	= BlockState {
		idCount :: Int,
		symTab  :: Map String (String, Type)
		}
	deriving Show

data CmpState
	= CmpState {
		blocks :: [BlockState], -- stack
		opns   :: [Opn]         -- top level operations
		}
	deriving Show

emptyCmpState = CmpState {
	blocks = [BlockState 0 Map.empty],
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


-- lookup ast name, return compiled name, type
getVar :: String -> Cmp (String, Type)
getVar name =
	getVar' name =<< gets blocks
	where
		getVar' name []     = err $ name ++ " does not exist"
		getVar' name (x:xs) = case Map.lookup name (symTab x) of
			Just x  -> return x
			Nothing -> getVar' name xs


assignVar :: String -> String -> Type -> Cmp ()
assignVar name cname typ = do
	(block:rest) <- gets blocks
	let st = symTab block
	new <- case Map.lookup name st of
		Just _  -> err $ name ++ " already defined"
		Nothing -> return $ Map.insert name (cname, typ) st
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

-- compilation functions

cmpTopStmt :: A.Stmt -> Cmp ()
cmpTopStmt stmt = case stmt of
	A.Assign name (A.EInt i) -> do
		ident <- uniqueId
		assignVar name ident TInt
		addOpn $ Assign ident (VInt i)

	A.Assign name (A.Infix A.Plus (A.EInt a) (A.EInt b)) -> do
		ident <- uniqueId
		assignVar name ident TInt
		addOpn $ Assign ident $ VInt (a + b)

	A.Set name (A.EInt i) -> do
		(cname, typ) <- getVar name
		case typ of
			TInt -> addOpn $ Set cname (VInt i)
			_    -> err $ show typ ++ " not int"

	A.ExprStmt (A.Call "print" [arg]) -> case arg of
		A.EInt i  -> addOpn $ Print (VInt i)
		A.Ident s -> do
			(cname, typ) <- getVar s
			case typ of
				TInt -> addOpn $ Print (VIdent cname)
				_    -> err $ "print: " ++ show typ ++ " not int"

	_                        -> err $ "invalid top stmt: " ++ show stmt

