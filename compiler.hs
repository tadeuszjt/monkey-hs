module Compiler where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List as List
import IR
import qualified AST as S
import qualified Lexer as L


--symbol table is a stack of maps representing scope
type SymTab = [Map.Map S.Name (Ident, Type)]

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
		funcs    :: Map.Map Index Func, -- the main map of functions
		fnStack  :: [(Index, SymTab)] -- stack shows current compiling function and symtab
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


uniqueId :: Cmp Index
uniqueId = do
	count <- gets idCount
	modify $ \s -> s {idCount = count + 1}
	return count


createFunc :: [Type] -> Cmp Index
createFunc targs = do
	-- add func to stack
	id <- uniqueId
	fns <- gets funcs
	stk <- gets fnStack
	modify $ \s -> s {
		funcs   = Map.insert id (TFunc targs TOrd, []) fns, -- any is placeholder
		fnStack = (id, initSymTab):stk
		}
	return id


finishFunc :: Type -> Cmp ()
finishFunc retty = do
	(fid, _):rest <- gets fnStack
	fns <- gets funcs
	let Just (TFunc targs _, opns) = Map.lookup fid fns
	let fns' = Map.insert fid (TFunc targs retty, opns) fns
	modify $ \s -> s {
		fnStack = rest,
		funcs = fns'
		}


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


-- takes a value and turns it into an ident
identify :: Val -> Cmp Val
identify val@(VIdent _ _) = return val
identify val = do
	id <- uniqueId
	emit $ Assign (Var id) val
	return $ VIdent (Var id) (typeOf val)


-- assigns a name to an id and type in top symbol table
declare :: S.Name -> Type -> Cmp Ident
declare name typ = do
	(fid, st):fs <- gets fnStack
	case lookupSymTab name [head st] of
		Just _  -> emptyErr
		Nothing -> return ()
	id <- uniqueId
	let st' = insertSymTab name (Var id, typ) st
	modify $ \s -> s { fnStack = (fid, st'):fs }
	return (Var id)


-- assigns name to an arg in symbol table
declareArg :: S.Name -> Type -> Index -> Cmp Ident
declareArg name typ ind = do
	(fid, st):fs <- gets fnStack
	case lookupSymTab name [head st] of
		Just _  -> emptyErr
		Nothing -> return ()
	let st' = insertSymTab name (Arg ind, typ) st
	modify $ \s -> s { fnStack = (fid, st'):fs }
	return (Arg ind)


stmt :: S.Stmt -> Cmp ()
stmt s = case s of
	S.Assign _ _ _  -> assign s
	S.Set _ _ _     -> set s
	S.If _ _ _ _    -> iff s
	S.While _ _ _   -> while s
	S.Block _ s     -> pushScope >> mapM_ stmt s >> popScope
	S.ExprStmt e    -> exprStmt s
	S.Return _ _    -> returnStmt s


returnStmt :: S.Stmt -> Cmp ()
returnStmt (S.Return pos exp) = do
	val <- expr exp
	case typeOf val of
		TStaticArray _ -> err pos "can't return arrays"
		_              -> return ()
	emit (Return val)


assign :: S.Stmt -> Cmp ()
assign (S.Assign pos name exp) = do
	val <- expr exp
	id <- withErr (declare name $ typeOf val) pos (name ++ " already defined")
	emit $ Assign id val


expr :: S.Expr -> Cmp Val
expr exp = case exp of
	S.Int _ i         -> return (VInt i)
	S.Bool _ b        -> return (VBool b)
	S.String _ s      -> return (VString s)
	S.Ident _ s       -> getName exp >>= \(id, typ) -> return (VIdent id typ)
	S.Infix _ _ _ _   -> infixx exp
	S.Func _ _ _      -> func exp
	S.Call _ _ _      -> call exp
	S.Array _ _       -> staticArray exp
	S.Subscript _ _ _ -> subscript exp
	_                 -> error $ "expr unhandled: " ++ (take 60 $ show exp) ++ "..."


subscript :: S.Expr -> Cmp Val
subscript (S.Subscript pos arr ind) = do
	arrVal <- expr arr
	elemTyp <- case typeOf arrVal of
		TStaticArray t -> return t
		_              -> err pos "subscripting on non array value"

	indVal <- expr ind
	assert (typeOf indVal == TInt) pos "subscript type ins't int"
	return $ VSubscript arrVal indVal


staticArray :: S.Expr -> Cmp Val
staticArray (S.Array pos exps) = do
	vals <- mapM expr exps
	let typeSet = Set.fromList $ map typeOf vals
	elemType <- case length $ Set.toList typeSet of
		1 -> return (Set.elemAt 0 typeSet)
		_ -> return TOrd

	-- check type
	case elemType of
		TInt -> return ()
		TBool -> return ()
		TStaticArray _ -> err pos "can't have arrays in arrays"
		TOrd -> err pos "can't have arrays of any"

	return $ VStaticArray vals elemType


call :: S.Expr -> Cmp Val
call (S.Call pos nameExpr argExprs) = do
	vIdent <- identify =<< expr nameExpr
	case typeOf vIdent of
		TFunc _ _ -> return ()
		_         -> err pos "call isn't function"

	let VIdent id (TFunc targs retty) = vIdent
	assert (length targs == length argExprs) pos "incorrect number of args"

	argVals <- mapM expr argExprs
	sanVals <- mapM sanitise argVals
	return $ VCall id sanVals retty
	where
		sanitise val@(VStaticArray _ _) = identify val
		sanitise val                    = return val


func :: S.Expr -> Cmp Val
func (S.Func pos args (S.Block _ stmts)) = do
	let targs = replicate (length args) TOrd

	fid <- createFunc targs
	mapM_ (\((name, typ), ind) -> declareArg name typ ind) (zip (zip args targs) [0..])

	mapM_ stmt stmts

	-- look at returns
	fns <- gets funcs
	let Just (t, opns) = Map.lookup fid fns
	let retTypeSet = Set.fromList $ map (\(Return val) -> typeOf val) (filter isReturn opns)

	retty <- case (length $ Set.toList retTypeSet) of
		0 -> err pos "no return statement"
		1 -> return $ Set.elemAt 0 retTypeSet
		_ -> return TOrd

	finishFunc retty
	return $ VIdent (Var fid) (TFunc targs retty)
	where
		isReturn (Return _) = True
		isReturn _          = False


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
			((TOrd,  TInt,  S.Plus),   TInt),
			((TInt,  TOrd,  S.Plus),   TInt),
			((TOrd,  TOrd,  S.Plus),   TInt),

			((TInt,  TInt,  S.Minus),  TInt),
			((TOrd,  TInt,  S.Minus),  TInt),
			((TInt,  TOrd,  S.Minus),  TInt),
			((TOrd,  TOrd,  S.Minus),  TInt),

			((TInt,  TInt,  S.Times),  TInt),
			((TOrd,  TInt,  S.Times),  TInt),
			((TInt,  TOrd,  S.Times),  TInt),
			((TOrd,  TOrd,  S.Times),  TInt),

			((TInt,  TInt,  S.Divide), TInt),
			((TInt,  TInt,  S.Mod),    TInt),

			((TInt,  TInt,  S.EqEq),   TBool),
			((TOrd,  TInt,  S.EqEq),   TBool),
			((TInt,  TOrd,  S.EqEq),   TBool),
			((TOrd,  TOrd,  S.EqEq),   TBool),

			((TInt,  TInt,  S.LThan),  TBool),
			((TOrd,  TInt,  S.LThan),  TBool),
			((TInt,  TOrd,  S.LThan),  TBool),
			((TOrd,  TOrd,  S.LThan),  TBool),

			((TInt,  TInt,  S.GThan),  TBool),
			((TOrd,  TInt,  S.GThan),  TBool),
			((TInt,  TOrd,  S.GThan),  TBool),
			((TOrd,  TOrd,  S.GThan),  TBool),

			((TBool, TBool, S.OrOr),   TBool)
			]

		infVal val = case val of
			VInt _     -> return val
			VBool _    -> return val
			VIdent _ _ -> return val
			_          -> identify val



set :: S.Stmt -> Cmp ()
set (S.Set pos name exp) = do
	(id, typ) <- getName (S.Ident pos name)
	val <- expr exp
	if typ == typeOf val
	then emit (Set id val)
	else err pos "type mismatch"



exprStmt :: S.Stmt -> Cmp ()
exprStmt (S.ExprStmt exp) = case exp of
	S.Call _ (S.Ident _ "print") args -> emit . Print =<< mapM sanitise =<< mapM expr args
	_                                 -> emit . Expr =<< expr exp
	where
		sanitise val@(VStaticArray _ _) = identify val
		sanitise val                    = return val


while :: S.Stmt -> Cmp ()
while (S.While pos cnd (S.Block _ stmts)) = do	
	emit LoopBegin
	cndVIdent <- identify =<< expr cnd
	assert (typeOf cndVIdent == TBool) pos "while condition isn't boolean"

	emit . IfBegin $ VInfix S.EqEq cndVIdent (VBool False) TBool
	emit LoopBreak
	emit IfEnd

	pushScope
	mapM_ stmt stmts
	popScope
	emit LoopEnd


iff :: S.Stmt -> Cmp ()
iff (S.If pos cnd (S.Block _ stmts) els) = do
	cndVal <- expr cnd
	case typeOf cndVal of
		TBool -> return ()
		TOrd  -> return ()
		_     -> err pos "if condition isn't bool"

	emit (IfBegin cndVal)
	pushScope
	mapM_ stmt stmts
	popScope

	case els of
		Nothing                 -> return ()
		Just (S.Block _ stmts') -> emit IfElse >> pushScope >> mapM_ stmt stmts' >> popScope
		Just elif               -> emit IfElse >> iff elif

	emit IfEnd
