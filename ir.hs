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
	| TOrd
	| TVoid 
	| TFunc [Type] Type
	| TStaticArray Type
	deriving (Show, Eq, Ord)

data Val
	= VInt Int
	| VBool Bool
	| VString String
	| VIdent Ident Type
	| VInfix A.Op Val Val Type
	| VCall Ident [Val] Type
	| VStaticArray [Val] Type
	| VSubscript Val Val
	deriving Show
	
data Opn
	= Assign Ident Val
	| Set Ident Val
	| Print [Val]
	| Expr Val
	| LoopBegin   | LoopBreak | LoopEnd
	| IfBegin Val | IfElse    | IfEnd
	| Return Val
	deriving Show

typeOf :: Val -> Type
typeOf v = case v of
	VInt _           -> TInt
	VBool _          -> TBool
	VString _        -> TString
	VStaticArray _ t -> TStaticArray t
	VIdent _ t       -> t
	VInfix _ _ _ t   -> t
	VCall _ _ t      -> t
	VSubscript arr _ -> let TStaticArray t = typeOf arr in t
