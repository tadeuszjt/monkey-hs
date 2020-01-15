module Compiler where

import Control.Monad.State
import Data.Map as Map
import Data.List as List
import IR
import qualified AST as S
import qualified Lexer as L


--symbol table is a stack of maps representing scope
type SymTab = [Map S.Name (Ident, Type)]

initSymTab = [Map.empty]

lookupSymTab :: S.Name -> SymTab -> Maybe (Ident, Type)
lookupSymTab name []     = Nothing
lookupSymTab name (x:xs) = case Map.lookup name x of
	Just (i, t) -> Just (i, t)
	Nothing     -> lookupSymTab name xs

insertSymTab :: S.Name -> (Ident, Type) -> SymTab -> SymTab
insertSymTab name val (x:xs) =
	(Map.insert name val x):xs

pushSymTab :: SymTab -> SymTab
pushSymTab st =
	(Map.empty):st

popSymTab :: SymTab -> SymTab
popSymTab st =
	tail st


-- cmp state
data CmpState
	= CmpState {
		idCount  :: Int,                -- counts identifiers for unique function
		funcs    :: Map Ident Func, -- the main map of functions
		fnStack  :: [(Ident, SymTab)] -- stack shows current compiling function and symtab
		}
	deriving Show

initCmpState = CmpState {
	idCount = 1,
	funcs   = Map.singleton 0 (TFunc [] TInt, []),
	fnStack = [(0, initSymTab)]
	}


type CmpError = (L.AlexPosn, String)

-- cmp uses either as a monad
type Cmp a = StateT CmpState (Either CmpError) a


-- error handling functions
err :: L.AlexPosn -> String -> Cmp a
err pos str =
	lift $ Left (pos, str)

emptyErr :: Cmp a
emptyErr =
	lift $ Left (L.AlexPn 0 0 0, "")

withErr :: Cmp a -> L.AlexPosn -> String -> Cmp a
withErr cmp pos str = do
	s <- get
	case runStateT cmp s of
		Right (v, s') -> do
			put s'
			return v
		Left _ -> err pos str

assert :: Bool -> L.AlexPosn -> String -> Cmp ()
assert b pos str =
	if b
	then return ()
	else err pos str


uniqueId :: Cmp Ident
uniqueId = do
	count <- gets idCount
	modify $ \s -> s {idCount = count + 1}
	return count


createFunc :: Type -> Cmp ()
createFunc fnType@(TFunc _ _) = do
	id <- uniqueId
	fns <- gets funcs
	stk <- gets fnStack
	modify $ \s -> s {
		funcs   = Map.insert id (fnType, []) fns,
		fnStack = (id, initSymTab):stk
		}


finishFunc :: Cmp ()
finishFunc = do
	_:rest <- gets fnStack
	modify $ \s -> s { fnStack = rest }


pushScope :: Cmp ()
pushScope = do
	(id, st):rest <- gets fnStack
	modify $ \s -> s { fnStack = (id, pushSymTab st):rest }


popScope :: Cmp ()
popScope = do
	(id, st):rest <- gets fnStack
	modify $ \s -> s { fnStack = (id, popSymTab st):rest }
	

getName :: S.Expr -> Cmp (Ident, Type)
getName (S.Ident p name) = do
	(_, st):_ <- gets fnStack
	case lookupSymTab name st of
			Nothing     -> err p (name ++ " does not exist")
			Just (i, t) -> return (i, t) 


compile :: S.Program -> Either CmpError Prog
compile astProg = do
	evalStateT cmp initCmpState
	where
		cmp :: Cmp Prog
		cmp = do 
			mapM_ stmt astProg
			fns <- gets funcs
			return $ Map.toList fns


emit :: Opn -> Cmp ()
emit opn = do
	(fid, _):_ <- gets fnStack
	fns        <- gets funcs
	let Just (t, opns) = Map.lookup fid fns
	let f' = (t, opns ++ [opn])
	modify $ \s -> s { funcs = Map.insert fid f' fns }



-- assigns a name to an id and type in top symbol table
declare :: S.Name -> Type -> Cmp Ident
declare name typ = do
	(fid, st):fs <- gets fnStack
	case lookupSymTab name [head st] of
		Just _  -> emptyErr
		Nothing -> return ()
	id <- uniqueId
	let st' = insertSymTab name (id, typ) st
	modify $ \s -> s { fnStack = (fid, st'):fs }
	return id


stmt :: S.Stmt -> Cmp ()
stmt s = case s of
	S.Assign _ _ _  -> assign s
	S.Set _ _ _     -> set s
	S.If _ _ _ _    -> iff s
	S.While _ _ _   -> while s
	S.Block _ s     -> pushScope >> mapM_ stmt s >> popScope
	S.ExprStmt e    -> expr e >> return ()


assign :: S.Stmt -> Cmp ()
assign (S.Assign pos name exp) = do
	val <- expr exp
	id <- withErr (declare name $ typeOf val) pos (name ++ " already defined")
	emit $ Assign id val


expr :: S.Expr -> Cmp Val
expr exp = case exp of
	S.Int _ i       -> return (VInt i)
	S.Bool _ b      -> return (VBool b)
	S.String _ s    -> return (VString s)
	S.Ident _ s     -> getName exp >>= \(id, typ) -> return (VIdent id typ)
	S.Infix _ _ _ _ -> infixx exp

	
infixx :: S.Expr -> Cmp Val
infixx (S.Infix pos op e1 e2) = do
	v1 <- infVal =<< expr e1
	v2 <- infVal =<< expr e2 

	typ <- case List.lookup (typeOf v1, typeOf v2, op) infixTable of
		Just t  -> return t
		Nothing -> err pos "invalid infix expression"

	return $ VInfix op v1 v2 typ
	where
		infixTable = [
			((TInt,  TInt,  S.Plus),   TInt),
			((TInt,  TInt,  S.Minus),  TInt),
			((TInt,  TInt,  S.Times),  TInt),
			((TInt,  TInt,  S.Divide), TInt),
			((TInt,  TInt,  S.Mod),    TInt),
			((TInt,  TInt,  S.EqEq),   TBool),
			((TInt,  TInt,  S.LThan),  TBool),
			((TInt,  TInt,  S.GThan),  TBool),
			((TBool, TBool, S.OrOr),   TBool)
			]

		infVal val = case val of
			VInt _     -> return val
			VBool _    -> return val
			VIdent _ _ -> return val
			_ -> do
				id <- uniqueId
				emit $ Assign id val
				return $ VIdent id (typeOf val)


set :: S.Stmt -> Cmp ()
set (S.Set pos name exp) = do
	(id, typ) <- getName (S.Ident pos name)
	val <- expr exp
	if typ == typeOf val
	then emit (Set id val)
	else err pos "type mismatch"



exprStmt :: S.Stmt -> Cmp ()
exprStmt (S.ExprStmt exp) = case exp of
	S.Call _ (S.Ident _ "print") args -> emit . Print =<< mapM expr args
	_ -> error $ "expr stmt unhandled: " ++ show exp


while :: S.Stmt -> Cmp ()
while (S.While pos cnd (S.Block _ stmts)) = do	
	emit LoopBegin
	cndv <- expr cnd
	assert (typeOf cndv == TBool) pos "while condition isn't boolean"

	cndId <- uniqueId 
	emit $ Assign cndId cndv

	emit . IfBegin $ VInfix S.EqEq (VIdent cndId TBool) (VBool False) TBool
	emit LoopBreak
	emit IfEnd

	pushScope
	mapM_ stmt stmts
	popScope
	emit LoopEnd


iff :: S.Stmt -> Cmp ()
iff (S.If pos cnd (S.Block _ stmts) els) = do
	cndVal <- expr cnd
	assert (typeOf cndVal == TBool) pos "if condition isn't boolean"

	emit (IfBegin cndVal)
	pushScope
	mapM_ stmt stmts
	popScope

	case els of
		Nothing                 -> return ()
		Just (S.Block _ stmts') -> emit IfElse >> pushScope >> mapM_ stmt stmts' >> popScope
		Just elif               -> emit IfElse >> iff elif

	emit IfEnd
