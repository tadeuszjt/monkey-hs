module IR where

import qualified AST as A

type Ident = Int
type Prog = [(Ident, Func)]
type Func = (Type, [Opn])

data Type
	= TInt
	| TBool
	| TString
	| TFunc [Type] Type
	| TAny
	deriving (Show, Eq)

data Val
	= VInt Int
	| VBool Bool
	| VString String
	| VIdent Ident Type
	| VInfix A.Op Val Val Type
	| VCall Ident [Val]
	deriving Show
	
data Opn
	= Assign Ident Val
	| Set Ident Val
	| Print [Val]
	| Call Ident
	| LoopBegin   | LoopBreak | LoopEnd
	| IfBegin Val | IfElse    | IfEnd
	deriving Show

typeOf :: Val -> Type
typeOf v = case v of
	VInt _         -> TInt
	VBool _        -> TBool
	VString _      -> TString
	VIdent _ t     -> t
	VInfix _ _ _ t -> t
