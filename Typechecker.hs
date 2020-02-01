module TypeChecker where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import qualified Lexer as L
import qualified SymTab 
import IR


type Unique    = S.Name
type TypeGraph = Map.Map Unique (Set.Set Node)
type TypeMap   = Map.Map Unique Type


data Node
	= NName  Unique
	| NType  Type
	| NArray (Set.Set Node) Int
	| NInfix S.Op Node Node
	deriving (Show, Eq, Ord)


resolveTypeGraph :: TypeGraph -> TypeMap
resolveTypeGraph graph =
	Map.mapWithKey (\un _ -> (resolveSet (graph Map.! un) graph)) graph


resolveSet :: Set.Set Node -> TypeGraph -> Type
resolveSet set graph = case Set.toList set of 
	[]  -> TVoid
	[n] -> resolveNode n graph
	_   -> resolveTypes $ Set.map ((\set -> resolveSet set graph) . Set.singleton) set


resolveNode :: Node -> TypeGraph -> Type
resolveNode node graph = case node of
	NType t             -> t
	NName un            -> resolveSet (graph Map.! un) graph
	NArray set len      -> TArray (resolveSet set graph) len
	NInfix S.Plus _ _   -> TInt
	NInfix S.Minus _ _  -> TInt
	NInfix S.Times _ _  -> TInt
	NInfix S.Divide _ _ -> TInt
	_    -> error $ "resolveNode: " ++ show node


resolveUnique :: Unique -> TypeGraph -> Type
resolveUnique un graph = resolveSet (graph Map.! un) graph


-- Compiler Monad
data CmpState
	= CmpState {
		unCount   :: Int,
		typeGraph :: TypeGraph,
		symTab    :: SymTab.SymTab S.Name Unique
		}
	deriving Show


initCmpState = CmpState {
	unCount   = 0,
	typeGraph = Map.empty,
	symTab    = SymTab.initSymTab
	}


type CheckedAST = (TypeMap, S.AST)
type CmpError   = (L.AlexPosn, String)
type Cmp a      = StateT CmpState (Either CmpError) a


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


-- Compilation functions
unique :: Cmp Unique
unique = do
	count <- gets unCount
	modify $ \s -> s { unCount = count + 1 }
	return (show count)


insertNode :: Unique -> Node -> Cmp ()
insertNode name node = do
	graph <- gets typeGraph
	let nodeSet' = case Map.lookup name graph of
		Just set -> Set.insert node set
		Nothing  -> Set.singleton node
	modify $ \s -> s { typeGraph = Map.insert name nodeSet' graph }


look :: L.AlexPosn -> S.Name -> Cmp Unique
look pos name = do
	table <- gets symTab
	case SymTab.lookup name table of
		Just un -> return un
		Nothing -> err pos $ name ++ " doesn't exist"


nodeOf :: S.Expr -> Node
nodeOf e = case e of
	S.Int _ _          -> NType TInt
	S.Bool _ _         -> NType TBool
	S.String _ s       -> NArray (Set.singleton $ NType TChar) (length s)
	S.Ident _ un       -> NName un
	S.Array _ es       -> NArray (Set.fromList $ map nodeOf es) (length es)
	S.Infix _ op e1 e2 -> NInfix op (nodeOf e1) (nodeOf e2) 
	_                  -> error $ "nodeOf: " ++ show e


satisfies :: S.Expr -> Type -> Cmp Bool
satisfies (S.Int _ _) TInt           = return True
satisfies (S.Bool _ _) TBool         = return True
satisfies (S.Infix _ S.LT _ _) TBool = return True
satisfies (S.Ident _ un) typ         = do
	unType <- return . (resolveUnique un) =<< gets typeGraph
	case (unType, typ) of
		(TInt, TInt)   -> return True
		(TBool, TBool) -> return True
		_            -> error "satisfies"
satisfies _ _                    = return False


-- Compile AST
compile :: S.AST -> Either CmpError CheckedAST
compile ast = do
	evalStateT cmp initCmpState
	where
		cmp :: Cmp CheckedAST
		cmp = do
			stmts <- mapM stmt ast
			graph <- gets typeGraph
			return (resolveTypeGraph graph, stmts)


stmt :: S.Stmt -> Cmp S.Stmt
stmt s = case s of
	S.Assign pos name e -> do
		st@(s:_) <- gets symTab
		case SymTab.lookup name [s] of
			Just _  -> err pos $ name ++ " already declared"
			Nothing -> return ()

		un <- unique
		let st' = SymTab.insert name un st
		modify $ \s -> s { symTab = st' }

		e' <- expr e
		insertNode un (nodeOf e')
		return $ S.Assign pos un e'
	
	S.Set pos name e -> do
		un <- look pos name
		e' <- expr e
		insertNode un (nodeOf e')
		return $ S.Set pos un e'

	S.Print pos es ->
		fmap (S.Print pos) (mapM expr es)

	S.If pos cnd blk els -> do
		cnd' <- expr cnd
		b <- cnd' `satisfies` TBool
		unless b (err pos "if condition isn't bool")

		blk' <- stmt blk
		els' <- case els of
			Just s  -> fmap Just (stmt s)
			Nothing -> return Nothing
		return $ S.If pos cnd' blk' els'

	S.While pos cnd blk -> do
		cnd' <- expr cnd
		b <- cnd' `satisfies` TBool
		unless b (err pos "while condition isn't bool")

		blk' <- stmt blk
		return $ S.While pos cnd' blk'

	S.Block stmts -> do
		pushScope
		stmts' <- mapM stmt stmts
		popScope
		return $ S.Block stmts'


expr :: S.Expr -> Cmp S.Expr
expr e = case e of
	S.Int pos n      -> return e
	S.Bool pos b     -> return e
	S.String pos s   -> return e
	S.Ident pos un   -> fmap (S.Ident pos) (look pos un)
	S.Array pos es   -> fmap (S.Array pos) (mapM expr es)
	S.Infix _ _ _ _  -> exprInfix e


infixTable = [
	(S.Plus, [(TInt, TInt)]),
	(S.Minus, [(TInt, TInt)]),
	(S.Times, [(TInt, TInt)]),
	(S.Divide, [(TInt, TInt)])
	]




exprInfix :: S.Expr -> Cmp S.Expr
exprInfix (S.Infix pos op e1 e2) = do
	e1' <- expr e1
	e2' <- expr e2

	case op of
		S.Plus -> do
			b1 <- e1' `satisfies` TInt
			b2 <- e2' `satisfies` TInt
			unless (b1 && b2) (err pos "invalid infix")
			return $ S.Infix pos op e1' e2'

		S.Minus -> do
			b1 <- e1' `satisfies` TInt
			b2 <- e2' `satisfies` TInt
			unless (b1 && b2) (err pos "invalid infix")
			return $ S.Infix pos op e1' e2'

		S.Times -> do
			b1 <- e1' `satisfies` TInt
			b2 <- e2' `satisfies` TInt
			unless (b1 && b2) (err pos "invalid infix")
			return $ S.Infix pos op e1' e2'

		S.Divide -> do
			b1 <- e1' `satisfies` TInt
			b2 <- e2' `satisfies` TInt
			unless (b1 && b2) (err pos "invalid infix")
			return $ S.Infix pos op e1' e2'

		S.LT -> do
			b1 <- e1' `satisfies` TInt
			b2 <- e2' `satisfies` TInt
			unless (b1 && b2) (err pos "invalid infix")
			return $ S.Infix pos op e1' e2'

		_ -> err pos "haven't done this yet"
