module Compiler where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified AST as S
import qualified TypeChecker as T
import qualified Lexer as L
import IR


data CmpState = CmpState {
    idCount :: Index,
	idMap   :: Map.Map T.Unique Index,
	typeMap :: T.TypeMap, 
	fnMap   :: Map.Map Index Func,
	fnStack :: [Index]
	}
	deriving Show


type CmpError = (L.AlexPosn, String)
type Cmp a    = StateT CmpState (Either CmpError) a


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


insertType :: T.Unique -> Type -> Cmp ()
insertType un typ = do
	types <- gets typeMap
	modify $ \s -> s { typeMap = Map.insert un typ types }


insertUn :: T.Unique -> Index -> Cmp ()
insertUn un id = do
	idMap <- gets idMap
	modify $ \s -> s { idMap = Map.insert un id idMap }


err :: L.AlexPosn -> String -> Cmp ()
err pos str = lift $ Left (pos, str)


getType :: T.Unique -> Cmp Type
getType un = fmap (Map.! un) (gets typeMap)
	
	
-- Compile AST
compile :: T.CheckedAST -> Either CmpError Program
compile (typeMap, ast) = do
	let initFuncType = TFunc [] TVoid
	let initCmpState = CmpState {
		idCount = 0,
		idMap   = Map.empty,
		typeMap = typeMap,
		fnMap   = Map.singleton 0 (Func initFuncType [] []),
		fnStack = [0]
		}

	evalStateT cmp initCmpState
	where
		cmp :: Cmp Program
		cmp = do
			mapM_ stmt ast
			fmap Map.toList (gets fnMap)


stmt :: S.Stmt -> Cmp ()
stmt s = case s of
	S.Assign pos un e -> do
		val <- expr e
		typ <- getType un
		when (isArray typ) (insertType un TArrayPtr)
		id <- unique
		insertUn un id
		emit . (Assign id val) =<< getType un

	S.Set pos un e -> do
		val <- expr e
		typ <- getType un
		when (isArray typ) (insertType un TArrayPtr)
		id <- fmap (Map.! un) (gets idMap)
		emit $ Set id val typ

	S.Print pos es ->
		emit . Print =<< mapM expr es

	S.If pos cnd blk els -> do
		end <- unique
		emit . (If end) =<< expr cnd
		stmt blk
		case els of
			Nothing            -> return ()
			Just b@(S.Block _) -> emit (ElseIf $ Bool True) >> stmt b
			_                  -> err pos "tadeusz can't do else if"
		emit $ Label end

	S.While pos cnd blk -> do
		loop <- unique
		end <- unique
		endif <- unique
		emit $ Label loop
		emit . (If endif) =<< expr cnd
		emit $ Goto end
		emit $ Label endif
		stmt blk
		emit $ Goto loop
		emit $ Label end

	S.Block blk ->
		mapM_ stmt blk

	S.ExprStmt e ->
		emit . Expr =<< expr e


expr :: S.Expr -> Cmp Val
expr e = case e of
	S.Int _ n     -> return (Int n)

	S.Bool _ b    -> return (Bool b)

	S.String _ s  -> do
		id <- unique
		emit $ Alloc id (map Char s) TChar
		return $ Ident id (TArray TChar $ length s)

	S.Ident _ un -> do
		id <- fmap (Map.! un) (gets idMap)
		fmap (Ident id) (getType un)

	S.Array _ es -> do
		vals <- mapM expr es
		let elemType = case resolveTypes $ Set.fromList (map typeOf vals) of
			TArray _ _ -> TArrayPtr
			t          -> t

		let arrayType = TArray elemType (length vals)
		id <- unique
		emit $ Alloc id vals elemType
		return $ Ident id arrayType

	S.Infix _ op e1 e2 -> do
		v1 <- expr e1
		v2 <- expr e2

		case op of
			S.LT -> return $ Infix op v1 v2 TInt TInt TBool
			_    -> return $ Infix op v1 v2 TInt TInt TInt



