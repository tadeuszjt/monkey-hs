module TypeChecker where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import qualified Lexer as L
import qualified SymTab 


type Index       = Int
type CheckedExpr = S.Expr
type CheckedStmt = S.Stmt

data Type
	= TInt
	| TBool
	| TString
	| TArray Type
	| TFunc [Type] Type
	| TypeOf Index
	deriving (Show, Ord, Eq)

data CheckedAST  = CheckedAST {
	symbols :: Map.Map Index (Set.Set Type),
	ast     :: S.AST
	}
	deriving (Show)

emptyCheckedAST = CheckedAST {
	symbols = Map.empty,
	ast     = []
	}


-- Compiler Monad
type CmpError = (L.AlexPosn, String)


data CmpState
	= CmpState {
		idCount  :: Index,
		symTypes :: Map.Map Index (Set.Set Type),
		symTab   :: SymTab.SymTab S.Name Index,
		returnId :: Index
		}
	deriving Show


initCmpState = CmpState {
	idCount  = 2,
	symTypes = Map.empty,
	symTab   = SymTab.initSymTab,
	returnId = 1
	}


type Cmp a = StateT CmpState (Either CmpError) a


pushScope :: Cmp ()
pushScope = do
	st <- gets symTab
	modify $ \s -> s { symTab = SymTab.push st }


popScope :: Cmp ()
popScope = do
	st <- gets symTab
	modify $ \s -> s { symTab = SymTab.pop st }


err :: L.AlexPosn -> String -> Cmp a
err pos str = lift $ Left (pos, str)


assert :: L.AlexPosn -> Bool -> String -> Cmp ()
assert pos b msg = if b then return () else err pos msg


-- Compilation functions
unique :: Cmp Index
unique = do
	count <- gets idCount
	modify $ \s -> s { idCount = count + 1 }
	return count


upgrade :: Index -> Type -> Cmp ()
upgrade id typ = do
	syms <- gets symTypes
	let set' = case Map.lookup id syms of
		Just set -> Set.insert typ set
		Nothing  -> Set.singleton typ
	modify $ \s -> s { symTypes = Map.insert id set' syms }


look :: L.AlexPosn -> S.Name -> Cmp Index
look pos name = do
	st <- gets symTab
	case SymTab.lookup name st of
		Just id -> return id
		Nothing -> err pos $ name ++ " doesn't exist"


typeOf :: CheckedExpr -> Cmp Type
typeOf e = case e of
	S.Int _ _      -> return TInt
	S.Bool _ _     -> return TBool
	S.String _ _   -> return TString
	S.Ident pos id -> return $ TypeOf (read id)


-- Compile AST
satisfies :: CheckedExpr -> Type -> Cmp Bool
satisfies e typ = case e of
	S.Int _ _      -> return (typ == TInt)
	S.Bool _ _     -> return (typ == TBool)
	S.String _ _   -> return (typ == TString)
	S.Ident pos id -> idSatisfies pos (read id) typ
	where
		idSatisfies :: L.AlexPosn -> Index -> Type -> Cmp Bool
		idSatisfies pos id typ = do
			syms <- gets symTypes
			let Just typeSet = Map.lookup id syms
			bs <- mapM (flip (typeSatisfies pos) $ typ) (Set.toList typeSet)
			return $ True `elem` bs

		typeSatisfies :: L.AlexPosn -> Type -> Type -> Cmp Bool
		typeSatisfies pos ta tb = do
			if ta == tb
			then return True
			else case ta of
				TypeOf id -> idSatisfies pos id tb
				_         -> return False



compile :: S.AST -> Either CmpError CheckedAST
compile ast = do
	evalStateT cmp initCmpState
	where
		cmp :: Cmp CheckedAST
		cmp = do
			stmts <- mapM stmt ast
			symbols <- gets symTypes
			return $ CheckedAST symbols stmts


stmt :: S.Stmt -> Cmp S.Stmt
stmt s = case s of
	S.Assign pos name e -> do
		st@(s:_) <- gets symTab
		case SymTab.lookup name [s] of
			Just _  -> err pos $ name ++ " already declared"
			Nothing -> return ()

		id <- unique
		let st' = SymTab.insert name id st
		modify $ \s -> s { symTab = st' }

		e' <- expr e
		upgrade id =<< typeOf e'
		return $ S.Assign pos (show id) e'
	
	S.Set pos name e -> do
		id <- look pos name
		e' <- expr e
		upgrade id =<< typeOf e'
		return $ S.Set pos (show id) e'

	S.Return pos e -> do
		e' <- expr e
		retId <- gets returnId
		upgrade retId =<< typeOf e'
		return $ S.Return pos e'

	S.If pos cnd blk els -> do
		cnd' <- expr cnd
		b <- cnd' `satisfies` TBool
		assert pos b "if condition isn't boolean"

		blk' <- stmt blk
		els' <- case els of
			Just s  -> fmap Just (stmt s)
			Nothing -> return Nothing
		return $ S.If pos cnd' blk' els'

	S.While pos cnd blk -> do
		cnd' <- expr cnd
		b <- cnd' `satisfies` TBool
		assert pos b "while condition isn't boolean"

		blk' <- stmt blk
		return $ S.While pos cnd' blk'

	S.Block stmts -> do
		pushScope
		stmts' <- mapM stmt stmts
		popScope
		return $ S.Block stmts'


expr :: S.Expr -> Cmp S.Expr
expr e = case e of
	S.Int pos n       -> return e
	S.Bool pos b      -> return e
	S.String pos s    -> return e
	S.Ident pos name  -> return . (S.Ident pos) =<< fmap show (look pos name)
	S.Array pos elems -> return . (S.Array pos) =<< mapM expr elems
