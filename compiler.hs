module Compiler where

import Control.Monad.State
import Data.Map as Map
import Data.List as List
import IR
import qualified AST as A


type Blocks = [Map String (Ident, Type)]
emptyBlocks = [Map.empty]


data CmpState
	= CmpState {
		idCount  :: Int,
		symTab   :: Map Ident Func,
		fnStack  :: [(Ident, Blocks)]
		}
	deriving Show

emptyCmpState = CmpState {
	idCount = 1,
	symTab  = Map.singleton 0 (TFunc, []),
	fnStack = [(0, emptyBlocks)]
	}

type Cmp a = StateT CmpState (Either String) a


-- lookup string name in entire block stack
lookupBlocks :: String -> Blocks -> Maybe (Ident, Type)
lookupBlocks name []     = Nothing
lookupBlocks name (b:bs) =
	case Map.lookup name b of
		Just (id, typ) -> Just (id, typ)
		Nothing        -> lookupBlocks name bs


uniqueId :: Cmp Ident
uniqueId = do
	count <- gets idCount
	modify $ \s -> s {idCount = count + 1}
	return count


-- creates a new function to append operations to
pushFunc :: Cmp Ident
pushFunc = do
	id <- uniqueId
	st <- gets symTab
	fs <- gets fnStack
	modify $ \s -> s {
		fnStack = (id, emptyBlocks):fs,
		symTab = Map.insert id (TFunc, []) st
		}
	return id


popFunc :: Cmp ()
popFunc = do
	fs <- gets fnStack
	modify $ \s -> s {fnStack = tail fs}


-- pushes a new block to store names
pushScope :: Cmp ()
pushScope = do
	(id, blks):fs <- gets fnStack
	modify $ \s -> s {fnStack = (id, (Map.empty:blks)):fs}


popScope :: Cmp ()
popScope = do
	(id, blks):fs <- gets fnStack
	modify $ \s -> s {fnStack = (id, tail blks):fs}


scope :: Cmp a -> Cmp a
scope cmp = do
	pushScope
	r <- cmp
	popScope
	return r


-- searches blocks for a name
getName :: String -> Cmp (Ident, Type)
getName name = do
	(_, blks):_ <- gets fnStack
	case lookupBlocks name blks of
		Nothing  -> err $ name ++ " does not exist"
		Just x   -> return x


-- assigns a name to an id and type in top block
assignName :: String -> Type -> Cmp Ident
assignName name typ = do
	(curId, b:bs):fs <- gets fnStack
	case lookupBlocks name [b] of
		Just _  -> err $ name ++ " already defined"
		Nothing -> return ()

	id <- uniqueId
	modify $ \s -> s {fnStack = (curId, (Map.insert name (id, typ) b):bs):fs}
	return id


-- appends an operation to the current function
addOpn :: Opn -> Cmp ()
addOpn opn = do
	(curId, _):_ <- gets fnStack
	st           <- gets symTab

	let Just (typ, opns) = Map.lookup curId st
	let fn' = (typ, opns ++ [opn])
	let st' = Map.insert curId fn' st

	modify $ \s -> s {symTab = st'} 


err :: String -> Cmp a
err str =
	lift (Left str)


assert :: Bool -> String -> Cmp ()
assert cnd str =
	if cnd
	then return ()
	else err str


-- program output
evalCmp :: Cmp Prog -> Either String Prog
evalCmp cmp =
	evalStateT cmp emptyCmpState


cmpProg :: A.Program -> Cmp Prog
cmpProg astProg = do
	mapM_ cmpStmt astProg
	st <- gets symTab
	return (Map.toList st)


-- compile expressions
cmpExpr :: A.Expr -> Cmp Val
cmpExpr expr = case expr of
	A.Int i       -> return (VInt i)
	A.Bool b      -> return (VBool b)
	A.String s    -> return (VString s)
	A.Ident s     -> getName s >>= \(id, typ) -> return $ VIdent id typ
	A.Infix _ _ _ -> cmpInfix expr
	A.Func _ _    -> cmpFuncExpr expr
	_             -> err $ "cannot compile expression: " ++ show expr


cmpFuncExpr :: A.Expr -> Cmp Val
cmpFuncExpr (A.Func args (A.Block stmts)) = do
	id <- pushFunc
	scope $ mapM_ cmpStmt stmts
	popFunc
	return $ VIdent id TFunc


cmpInfix :: A.Expr -> Cmp Val
cmpInfix (A.Infix op e1 e2) = do
	v1 <- infVal =<< cmpExpr e1
	v2 <- infVal =<< cmpExpr e2 

	typ <- case List.lookup (valType v1, valType v2, op) infixTable of
		Just t -> return t
		Nothing -> err $ "no infix for " ++ show (op, e1, e2)

	return $ VInfix op v1 v2 typ
	where
		infixTable = [
			((TInt,  TInt,  A.Plus),   TInt),
			((TInt,  TInt,  A.Minus),  TInt),
			((TInt,  TInt,  A.Times),  TInt),
			((TInt,  TInt,  A.Divide), TInt),
			((TInt,  TInt,  A.Mod),    TInt),
			((TInt,  TInt,  A.EqEq),   TBool),
			((TInt,  TInt,  A.LThan),  TBool),
			((TInt,  TInt,  A.GThan),  TBool),
			((TBool, TBool, A.OrOr),   TBool)
			]

		infVal val = case val of
			VInt _     -> return val
			VBool _    -> return val
			VIdent _ _ -> return val
			_ -> do
				id <- uniqueId
				addOpn $ Assign id val
				return $ VIdent id (valType val)


-- compile statements
cmpStmt :: A.Stmt -> Cmp ()
cmpStmt stmt = case stmt of
	A.Assign _ _ -> cmpAssign stmt
	A.Set _ _    -> cmpSet stmt
	A.ExprStmt _ -> cmpExprStmt stmt
	A.While _ _  -> cmpWhile stmt
	A.If _ _ _   -> cmpIf stmt
	_ -> err $ "statement unhandled: " ++ show stmt 


cmpAssign :: A.Stmt -> Cmp ()
cmpAssign (A.Assign name expr) = do
	val <- cmpExpr expr
	id  <- assignName name (valType val)
	addOpn $ Assign id val


cmpSet :: A.Stmt -> Cmp ()
cmpSet (A.Set name expr) = do
	(id, typ) <- getName name
	val <- cmpExpr expr
	if typ == valType val
	then addOpn (Set id val)
	else err $ "set: type mismatch: " ++ show typ ++ " " ++ show (valType val)


cmpExprStmt :: A.Stmt -> Cmp ()
cmpExprStmt (A.ExprStmt expr) = case expr of
	A.Call (A.Ident "print") args -> addOpn . Print =<< mapM cmpExpr args
	A.Call _ _                    -> cmpCallStmt expr
	_ -> err $ "expr stmt unhandled: " ++ show expr


cmpCallStmt :: A.Expr -> Cmp ()
cmpCallStmt (A.Call expr []) = do
	val <- cmpExpr expr
	assert (valType val == TFunc) $ "isn't func: " ++ show expr
	let VIdent id typ = val
	addOpn $ Call id


cmpWhile :: A.Stmt -> Cmp ()
cmpWhile (A.While cnd (A.Block stmts)) = do	
	addOpn LoopBegin
	cndVal <- cmpExpr cnd
	assert (valType cndVal == TBool) $ "while cnd not bool: " ++ show cnd

	cndId <- uniqueId 
	addOpn $ Assign cndId cndVal

	addOpn . IfBegin $ VInfix A.EqEq (VIdent cndId TBool) (VBool False) TBool
	addOpn LoopBreak
	addOpn IfEnd

	scope $ mapM_ cmpStmt stmts
	addOpn LoopEnd


cmpIf :: A.Stmt -> Cmp ()
cmpIf (A.If cnd (A.Block stmts) els) = do
	cndVal <- cmpExpr cnd
	assert (valType cndVal == TBool) $ "if cnd not bool: " ++ show cnd

	addOpn $ IfBegin cndVal
	scope $ mapM_ cmpStmt stmts

	case els of
		Nothing               -> return ()
		Just (A.Block stmts') -> addOpn IfElse >> scope (mapM_ cmpStmt stmts')
		Just elif             -> addOpn IfElse >> cmpIf elif

	addOpn IfEnd
