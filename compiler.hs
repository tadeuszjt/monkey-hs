module Compiler where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import qualified TypeChecker as T
import qualified Lexer as L
import IR


type CmpError = (L.AlexPosn, String)

data CmpState = CmpState {
	typeMap :: Map.Map Index Type,
	fnMap   :: Map.Map Index Func,
	fnStack :: [Index]
	}
	deriving Show


type Cmp a = StateT CmpState (Either CmpError) a


-- Compiler functions
emit :: Opn -> Cmp ()
emit opn = do
	id:_ <- gets fnStack
	funcs <- gets fnMap 

	let Just func = Map.lookup id funcs
	let func' = func { opns = (opns func) ++ [opn] }
	modify $ \s -> s { fnMap = Map.insert id func' funcs }


err :: L.AlexPosn -> String -> Cmp ()
err pos str = lift $ Left (pos, str)


getType :: Index -> Cmp Type
getType id = do
	Just typ <- return . (Map.lookup id) =<< gets typeMap
	return typ 
	
	
-- Compile AST
compile :: T.CheckedAST -> Either CmpError Program
compile checkedAST = do
	let types = resolveTypeMap (T.symbols checkedAST)
	let initFuncRetType = case Map.lookup 1 types of
		Just t  -> t
		Nothing -> TVoid
	let initFuncType = TFunc [] initFuncRetType

	let initCmpState = CmpState {
		typeMap = Map.union types (Map.fromList [(0, initFuncType), (1, initFuncRetType)]),
		fnMap   = Map.singleton 0 (Func initFuncType [] []),
		fnStack = [0]
		}

	evalStateT cmp initCmpState
	where
		cmp :: Cmp Program
		cmp = do
			mapM_ stmt (T.ast checkedAST)
			return . Map.toList =<< gets fnMap


resolveTypeMap :: Map.Map T.Index (Set.Set T.Type) -> Map.Map T.Index Type
resolveTypeMap typeMap =
	Map.fromList $ map (\(id, _) -> (id, resolveId id)) typeMapList  
	where 
		typeMapList = Map.toList typeMap

		resolveId :: T.Index -> Type
		resolveId id =
			let list = case Map.lookup id typeMap of
				Just set -> Set.toList set
				Nothing  -> []
			in resolveTypes list
				
		resolveTypes []    = TVoid
		resolveTypes [x]   = case x of
			T.TInt      -> TInt
			T.TBool     -> TBool
			T.TString   -> TString
			T.TypeOf id -> resolveId id
		resolveTypes types
			| allOrd types = TOrd
			| otherwise    = error "haven't gotten this far"
	
		allOrd [T.TypeOf id] = isOrd (resolveId id)
		allOrd [x]           = isOrd (resolveTypes [x])
		allOrd (x:xs)        = allOrd [x] && allOrd xs


stmt :: S.Stmt -> Cmp ()
stmt s = case s of
	S.Assign pos sid e -> do
		let id = read sid
		val <- expr e
		typ <- getType id
		emit $ Assign id val typ

	S.Set pos sid e -> do
		let id = read sid
		val <- expr e
		typ <- getType id
		emit $ Set id val typ

	S.Return pos e -> do
		val <- expr e
		id:_ <- gets fnStack
		TFunc _ retType <- getType id
		emit $ Return val retType

	S.If pos cnd blk els -> do
		cndVal <- expr cnd
		emit $ If cndVal
		stmt blk
		case els of
			Nothing            -> return ()
			Just b@(S.Block _) -> emit (ElseIf $ Bool True) >> stmt b
			_                  -> err pos "tadeusz can't do else if"
		emit End

	S.While pos cnd blk -> do
		cndVal <- expr cnd
		emit Loop
		emit $ If cndVal
		emit Break
		emit End
		stmt blk
		emit End

	S.Block blk -> do
		mapM_ stmt blk

	S.ExprStmt e -> do
		val <- expr e
		emit $ Expr val


expr :: S.Expr -> Cmp Val
expr e = case e of
	S.Int _ n     -> return (Int n)

	S.Bool _ b    -> return (Bool b)

	S.String _ s  -> return (String s)

	S.Ident _ sid ->
		let id = read sid
		in return . (Ident id) =<< getType id


