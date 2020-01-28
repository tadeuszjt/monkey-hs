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
    idCount :: Index,
	idMap   :: Map.Map S.Name Index,
	typeMap :: Map.Map Index Type,
	fnMap   :: Map.Map Index Func,
	fnStack :: [Index]
	}
	deriving Show


type Cmp a = StateT CmpState (Either CmpError) a


-- Compiler functions
unique :: Cmp Index
unique = do
	count <- gets idCount
	modify $ \s -> s { idCount = count + 1 }
	return count


emit :: Opn -> Cmp ()
emit opn = do
	id:_ <- gets fnStack
	funcs <- gets fnMap 

	let func = funcs Map.! id
	let func' = func { opns = (opns func) ++ [opn] }
	modify $ \s -> s { fnMap = Map.insert id func' funcs }


insertType :: Index -> Type -> Cmp ()
insertType id typ = do
	types <- gets typeMap
	modify $ \s -> s { typeMap = Map.insert id typ types }


insertId :: String -> Index -> Cmp ()
insertId sid id = do
	idMap <- gets idMap
	modify $ \s -> s { idMap = Map.insert sid id idMap }


err :: L.AlexPosn -> String -> Cmp ()
err pos str = lift $ Left (pos, str)


getType :: Index -> Cmp Type
getType id = do
	Just typ <- return . (Map.lookup id) =<< gets typeMap
	return typ 
	
	
-- Compile AST
compile :: T.CheckedAST -> Either CmpError Program
compile (typeMap, ast) = do
	let initIdCount = (head . reverse . Map.keys $ typeMap) + 1
	let initFuncRetType = case Map.lookup 1 typeMap of
		Just t  -> t
		Nothing -> TVoid
	let initFuncType = TFunc [] initFuncRetType

	let initCmpState = CmpState {
		idCount = initIdCount,
		idMap   = Map.empty,
		typeMap = Map.union typeMap (Map.fromList [(0, initFuncType), (1, initFuncRetType)]),
		fnMap   = Map.singleton 0 (Func initFuncType [] []),
		fnStack = [0]
		}

	evalStateT cmp initCmpState
	where
		cmp :: Cmp Program
		cmp = do
			mapM_ stmt ast
			return . Map.toList =<< gets fnMap



stmt :: S.Stmt -> Cmp ()
stmt s = case s of
	S.Assign pos sid e -> do
		val <- expr e
		case val of
			Ident id' (TArray _ _) -> insertId sid id'
			_                      -> do
				let id = read sid
				insertId sid id
				typ <- getType id
				emit $ Assign id val typ

	S.Set pos sid e -> do
		id <- return . (Map.! sid) =<< gets idMap
		val <- expr e
		typ <- getType id
		emit $ Set id val typ

	S.Return pos e -> do
		val <- expr e
		id:_ <- gets fnStack
		TFunc _ retType <- getType id
		emit $ Return val retType

	S.Print pos es -> do
		vals <- mapM expr es
		emit $ Print vals

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

	S.Ident _ sid -> do
		Just id <- return . (Map.lookup sid) =<< gets idMap
		return . (Ident id) =<< getType id

	S.Array _ es  -> do
		vals <- mapM expr es
		let elemType = resolveTypes (map typeOf vals)
		let arrayType = TArray elemType (length vals)
		id <- unique
		insertType id arrayType
		emit $ Alloc id vals elemType
		return $ Ident id arrayType


