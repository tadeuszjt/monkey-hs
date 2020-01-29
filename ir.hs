module IR where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified AST as S

type Index = Int
type Program = [(Index, Func)]


data Func = Func {
	typ  :: Type,
	args :: [Index],
	opns :: [Opn]
	}
	deriving Show


data Type
	= TInt
	| TBool
	| TString
	| TOrd
	| TArray Type Int
	| TFunc [Type] Type
	| TAny
	| TVoid 
	deriving (Show, Eq, Ord)


data Val
	= Int Int
	| Bool Bool
	| String String
	| Ident Index Type
	| Infix S.Op Val Val Type
	| Call Index [Val] Type
	| Subscript Val Val
	deriving Show

	
data Opn
	= Assign Index Val Type
	| Set Index Val Type
	| Return Val Type
	| Alloc Index [Val] Type
	| Print [Val]
	| Expr Val
	| Loop
	| Break
	| If Val
	| ElseIf Val
	| End
	deriving Show


typeOf :: Val -> Type
typeOf val = case val of
	Int _         -> TInt
	Bool _        -> TBool
	String _      -> TString
	Ident _ t     -> t
	Infix _ _ _ t -> t
	Call _ _ t    -> t
	Subscript a _ -> let TArray t _ = typeOf a in t


resolveTypes :: Set.Set Type -> Type
resolveTypes = resolve . Set.toList 
	where
		resolve []  = TVoid
		resolve [t] = t
		resolve ts
			| foldl1 (&&) (map isOrd ts) = TOrd
			-- TODO
			| otherwise                  = TAny


isOrd :: Type -> Bool
isOrd typ = typ `elem` [TInt, TBool, TOrd]

isArray :: Type -> Bool
isArray (TArray _ _) = True
isArray _            = False
