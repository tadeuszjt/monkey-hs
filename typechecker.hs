module TypeChecker where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import qualified Lexer as L
import qualified SymTab 
import IR


data Node
	= NIndex Index
	| NType  Type
	| NArray (Set.Set Node)
	deriving (Show, Eq, Ord)


type TypeGraph  = Map.Map Index (Set.Set Node)


resolveTypeGraph :: TypeGraph -> Map.Map Index Type
resolveTypeGraph graph =
	Map.map resolve graph
	where
		resolve :: Set.Set Node -> Type
		resolve set = case Set.toList set of 
			[]           -> TVoid
			[NType t]    -> t
			[NIndex id]  -> resolve (graph Map.! id)
			[NArray set] -> TArray (resolve set) 0
			[n]          -> error (show n)
			ns           -> resolveTypes $ map (\n -> resolve $ Set.singleton n) ns


-- Compiler Monad
type CheckedAST = (Map.Map Index Type, S.AST)

initCheckedAST = (Map.empty, [])

type CmpError = (L.AlexPosn, String)


data CmpState
	= CmpState {
		idCount   :: Index,
		typeGraph :: TypeGraph,
		symTab    :: SymTab.SymTab S.Name Index,
		returnId  :: Index
		}
	deriving Show


initCmpState = CmpState {
	idCount   = 2,
	typeGraph = Map.empty,
	symTab    = SymTab.initSymTab,
	returnId  = 1
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


upgrade :: Index -> Node -> Cmp ()
upgrade id node = do
	graph <- gets typeGraph
	let nodeSet' = case Map.lookup id graph of
		Just set -> Set.insert node set
		Nothing  -> Set.singleton node
	modify $ \s -> s { typeGraph = Map.insert id nodeSet' graph }


look :: L.AlexPosn -> S.Name -> Cmp Index
look pos name = do
	table <- gets symTab
	case SymTab.lookup name table of
		Just id -> return id
		Nothing -> err pos $ name ++ " doesn't exist"


nodeOf :: S.Expr -> Node
nodeOf e = case e of
	S.Int _ _      -> NType TInt
	S.Bool _ _     -> NType TBool
	S.String _ _   -> NType TString
	S.Ident pos id -> NIndex (read id)
	S.Array pos es -> NArray (Set.fromList $ map nodeOf es)


satisfies :: S.Expr -> Type -> Cmp Bool
satisfies (S.Int _ _) TInt       = return True
satisfies (S.Bool _ _) TBool     = return True
satisfies (S.String _ _) TString = return True
satisfies _ _ = error "not here"



-- Compile AST
compile :: S.AST -> Either CmpError CheckedAST
compile ast = do
	evalStateT cmp initCmpState
	where
		cmp :: Cmp CheckedAST
		cmp = do
			stmts <- mapM stmt ast
			graph <- gets typeGraph
			return $ (resolveTypeGraph graph, stmts)


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
		upgrade id (nodeOf e')
		return $ S.Assign pos (show id) e'
	
	S.Set pos name e -> do
		id <- look pos name
		e' <- expr e
		upgrade id (nodeOf e')
		return $ S.Set pos (show id) e'

	S.Return pos e -> do
		e' <- expr e
		retId <- gets returnId
		upgrade retId (nodeOf e')
		return $ S.Return pos e'

	S.Print pos es -> do
		es' <- mapM expr es
		return $ S.Print pos es'

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
