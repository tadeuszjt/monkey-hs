module IR where

import qualified AST as A

type Index = Int

data Ident
	= Var Index
	| Arg Index
	| Ret
	deriving Show

type Prog = [(Index, Func)]
type Func = (Type, [Opn])

data Type
	= TInt
	| TBool
	| TString
	| TFunc [Type] Type
	| TAny
	deriving (Show, Eq, Ord)

data Val
	= VInt Int
	| VBool Bool
	| VString String
	| VIdent Ident Type
	| VInfix A.Op Val Val Type
	| VCall Ident [Val] Type
	deriving Show
	
data Opn
	= Assign Ident Val
	| Set Ident Val
	| Print [Val]
	| LoopBegin   | LoopBreak | LoopEnd
	| IfBegin Val | IfElse    | IfEnd
	| Return Val
	deriving Show

typeOf :: Val -> Type
typeOf v = case v of
	VInt _         -> TInt
	VBool _        -> TBool
	VString _      -> TString
	VIdent _ t     -> t
	VInfix _ _ _ t -> t
	VCall _ _ t    -> t
