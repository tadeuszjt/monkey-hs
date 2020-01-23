module Compiler where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List as List
import IR
import qualified AST as S
import qualified Lexer as L

import CmpLib


compile :: S.Program -> Either CmpError Prog
compile astProg = do
	evalStateT cmp initCmpState
	where
		cmp :: Cmp Prog
		cmp = mapM_ stmt astProg >> gets funcs >>= return . Map.toList


stmt :: S.Stmt -> Cmp ()
stmt s = case s of
	S.Assign pos name exp -> do
		val <- expr exp
		let typ = typeOf val
		let str = name ++ " already defined"
		case val of
			VIdent id (TArray _ _) -> withErr (declare name id typ) pos str
			_                      -> do
				id <- fmap Var unique
				withErr (declare name id typ) pos str
				emit $ Assign id val

	S.Set pos name exp -> do
		(id, typ) <- getName (S.Ident pos name)
		val <- expr exp
		if typ == typeOf val
		then emit (Set id val)
		else err pos "type mismatch"

	S.Return pos exp -> do
		val <- expr exp
		case typeOf val of
			TArray _ _ -> err pos "can't return arrays"
			_          -> return ()
		emit (Return val)

	S.ExprStmt exp -> case exp of
		S.Call _ (S.Ident _ "print") args -> emit . Print =<< mapM expr args
		S.Call _ _ _                      -> emit . Expr =<< call exp
		_                                 -> emit . Expr =<< expr exp

	S.Block _ s     -> pushScope >> mapM_ stmt s >> popScope
	S.If _ _ _ _    -> ifStmt s
	S.While _ _ _   -> while s


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


array :: S.Expr -> Cmp Val
array (S.Array pos exps) = do
	vals <- mapM expr exps
	let typeSet = Set.toList $ Set.fromList (map typeOf vals)
	elemType <- getElemType typeSet
	
	id <- fmap Var unique
	emit $ Alloc id vals elemType
	return $ VIdent id (TArray elemType $ length vals)
	where
		getElemType :: [Type] -> Cmp Type
		getElemType typeSet
			| length typeSet == 0  = return TAny
			| length typeSet == 1  = return (head typeSet)
			| allArray typeSet     = return (head typeSet) 
			| allOrd typeSet       = return TOrd
			| otherwise            = return TAny

		allOrd [t]    = elem t [TInt, TBool, TString, TOrd]
		allOrd (t:ts) = allOrd [t] && allOrd ts

		allArray [TArray _ _] = True
		allArray (t:ts)       = allArray [t] && allArray ts



subscript :: S.Expr -> Cmp Val
subscript (S.Subscript pos arr ind) = do
	arrVal <- expr arr
	elemTyp <- case typeOf arrVal of
		TArray t l -> return t
		TAny       -> return TAny
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
