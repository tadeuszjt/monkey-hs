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


-- compilation primitives
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
		cmp = mapM_ stmt astProg >> gets funcs >>= return . Map.toList


emit :: Opn -> Cmp ()
emit opn = do
	(curFn, _):_ <- gets fnStack
	fns          <- gets funcs
	let Just (t, opns) = Map.lookup curFn fns
	let f' = (t, opns ++ [opn])
	modify $ \s -> s { funcs = Map.insert curFn f' fns }


-- takes a value and turns it into an ident
identify :: Val -> Cmp Val
identify val@(VIdent _ _) = return val
identify val = do
	id <- uniqueId
	emit $ Assign (Var id) val
	return $ VIdent (Var id) (typeOf val)


-- assigns a name to an id and type in top symbol table
declare :: S.Name -> Ident -> Type -> Cmp ()
declare name id typ = do
	(curFn, symTab):fs <- gets fnStack
	case lookupSymTab name [head symTab] of
		Just _  -> emptyErr
		Nothing -> return ()

	let symTab' = insertSymTab name (id, typ) symTab
	modify $ \s -> s { fnStack = (curFn, symTab'):fs }


-- assigns name to an arg in symbol table
declareArg :: S.Name -> Type -> Index -> Cmp Ident
declareArg name typ ind = do
	(curFn, symTab):fs <- gets fnStack
	case lookupSymTab name [head symTab] of
		Just _  -> emptyErr
		Nothing -> return ()

	let symTab' = insertSymTab name (Arg ind, typ) symTab
	modify $ \s -> s { fnStack = (curFn, symTab'):fs }
	return (Arg ind)


stmt :: S.Stmt -> Cmp ()
stmt s = case s of
	S.Assign _ _ _  -> assign s
	S.Set _ _ _     -> set s
	S.Return _ _    -> returnStmt s
	S.ExprStmt e    -> exprStmt s
	S.Block _ s     -> pushScope >> mapM_ stmt s >> popScope
	S.If _ _ _ _    -> ifStmt s
	S.While _ _ _   -> while s


assign :: S.Stmt -> Cmp ()
assign (S.Assign pos name exp) = do
	val <- expr exp
	case val of
		VIdent id (TArray _ _) ->
			withErr (declare name id $ typeOf val) pos (name ++ " already defined")
		_ -> do
			id <- uniqueId
			withErr (declare name (Var id) $ typeOf val) pos (name ++ " already defined")
			emit $ Assign (Var id) val


set :: S.Stmt -> Cmp ()
set (S.Set pos name exp) = do
	(id, typ) <- getName (S.Ident pos name)
	val <- expr exp
	if typ == typeOf val
	then emit (Set id val)
	else err pos "type mismatch"


returnStmt :: S.Stmt -> Cmp ()
returnStmt (S.Return pos exp) = do
	val <- expr exp
	case typeOf val of
		TArray _ _ -> err pos "can't return arrays"
		_          -> return ()
	emit (Return val)


exprStmt :: S.Stmt -> Cmp ()
exprStmt (S.ExprStmt exp) = case exp of
	S.Call _ (S.Ident _ "print") args -> emit . Print =<< mapM expr args
	S.Call _ _ _                      -> emit . Expr =<< call exp
	_                                 -> emit . Expr =<< expr exp



ifStmt :: S.Stmt -> Cmp ()
ifStmt (S.If pos cnd (S.Block _ stmts) els) = do
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
		Just elif               -> emit IfElse >> ifStmt elif

	emit IfEnd





expr :: S.Expr -> Cmp Val
expr exp = case exp of
	S.Int _ i         -> return (VInt i)
	S.Bool _ b        -> return (VBool b)
	S.String _ s      -> return (VString s)
	S.Ident _ s       -> getName exp >>= \(id, typ) -> return (VIdent id typ)
	S.Infix _ _ _ _   -> infixExpr exp
	S.Func _ _ _      -> func exp
	S.Call _ _ _      -> exprCall exp
	S.Subscript _ _ _ -> subscript exp
	S.Array _ _       -> array exp
	_ -> error $ "unhandled expression: " ++ show exp



array :: S.Expr -> Cmp Val
array (S.Array pos exps) = do
	vals <- mapM expr exps
	let typeSet = Set.toList $ Set.fromList (map typeOf vals)
	et <- elemType typeSet
	id <- uniqueId
	emit $ Alloc (Var id) vals et
	return $ VIdent (Var id) (TArray et $ length vals)
	where
		elemType :: [Type] -> Cmp Type
		elemType typeSet
			| length typeSet == 0  = return TAny
			| length typeSet == 1  = return (head typeSet)
			| allSameArray typeSet = return (head typeSet) 
			| allOrd typeSet       = return TOrd
			| otherwise            = return TAny

		allOrd [t]    = elem t [TInt, TBool, TString, TOrd]
		allOrd (t:ts) = allOrd [t] && allOrd ts

		allSameArray typeSet = case typeSet of
			[TArray _ _]                      -> True
			(TArray t1 _ :ts@(TArray t2 _:_)) -> t1 == t2 && allSameArray ts
			_                                 -> False


subscript :: S.Expr -> Cmp Val
subscript (S.Subscript pos arr ind) = do
	arrVal <- expr arr
	elemTyp <- case typeOf arrVal of
		TArray t l -> return t
		_          -> err pos "subscripting on non array value"

	indVal <- expr ind
	assert (typeOf indVal == TInt) pos "subscript type ins't int"
	return $ VSubscript arrVal indVal



exprCall :: S.Expr -> Cmp Val
exprCall exp@(S.Call pos _ _) = do
	val@(VCall id vals retty) <- call exp
	case retty of
		TVoid -> err pos "function does not return a value"
		_     -> return val


call :: S.Expr -> Cmp Val
call (S.Call pos nameExpr argExprs) = do
	vIdent <- identify =<< expr nameExpr
	case typeOf vIdent of
		TFunc _ _ -> return ()
		_         -> err pos "call isn't function"

	let VIdent id (TFunc targs retty) = vIdent
	assert (length targs == length argExprs) pos "incorrect number of args"

	argVals <- mapM expr argExprs
	return $ VCall id argVals retty
	where


func :: S.Expr -> Cmp Val
func (S.Func pos args (S.Block _ stmts)) = do
	let argTypes = replicate (length args) TOrd

	id <- createFunc argTypes
	mapM_ (\(name, typ, ind) -> declareArg name typ ind) (zip3 args argTypes [0..])
	mapM_ stmt stmts

	-- look at returns
	fns <- gets funcs
	let Just (t, opns) = Map.lookup id fns
	let retTypeSet = Set.fromList $ map (\(Return val) -> typeOf val) (filter isReturn opns)

	let retty = case length (Set.toList retTypeSet) of
		0 -> TVoid
		1 -> Set.elemAt 0 retTypeSet
		n -> TOrd

	finishFunc retty
	return $ VIdent (Var id) (TFunc argTypes retty)
	where
		isReturn (Return _) = True
		isReturn _          = False


infixExpr :: S.Expr -> Cmp Val
infixExpr (S.Infix pos op e1 e2) = do
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
