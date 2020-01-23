module CmpLib where

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
unique :: Cmp Index
unique = do
	count <- gets idCount
	modify $ \s -> s {idCount = count + 1}
	return count


createFunc :: [Type] -> Cmp Index
createFunc targs = do
	-- add func to stack
	id <- unique
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
	id <- unique
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
