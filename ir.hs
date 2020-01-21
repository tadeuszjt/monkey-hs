module IR where

import qualified AST as A
import System.IO

type Index = Int
type Prog = [(Index, Func)]
type Func = (Type, [Opn])


data Ident
	= Var Index
	| Arg Index
	deriving Show


data Type
	= TInt
	| TBool
	| TString
	| TOrd
	| TAny
	| TVoid 
	| TArray Type Int
	| TFunc [Type] Type
	deriving (Show, Eq, Ord)


data Val
	= VInt Int
	| VBool Bool
	| VString String
	| VIdent Ident Type
	| VInfix A.Op Val Val Type
	| VCall Ident [Val] Type
	| VSubscript Val Val
	deriving Show

	
data Opn
	= Assign Ident Val
	| Alloc Ident [Val] Type
	| Set Ident Val
	| Print [Val]
	| Expr Val
	| LoopBegin   | LoopBreak | LoopEnd
	| IfBegin Val | IfElse    | IfEnd
	| Return Val
	deriving Show


isOrd :: Type -> Bool
isOrd t = case t of
	TInt    -> True
	TBool   -> True
	TString -> True
	TOrd    -> True
	_       -> False


isArray :: Type -> Bool
isArray (TArray _ _) = True
isArray _            = False


typeOf :: Val -> Type
typeOf v = case v of
	VInt _           -> TInt
	VBool _          -> TBool
	VString _        -> TString
	VIdent _ t       -> t
	VInfix _ _ _ t   -> t
	VCall _ _ t      -> t
	VSubscript arr _ -> let TArray t _ = typeOf arr in t


hPrintIR :: Handle -> Prog -> IO ()
hPrintIR h prog                 = mapM_ (hPrintFn h) prog
hPrintFn h (index, (typ, opns)) = do
	hPutStrLn h $ show index ++ ": " ++ show typ
	mapM_ (hPrintOpn h) opns
	putStrLn ""	
hPrintOpn h opn                 = hPutStrLn h $ "\t" ++ show opn
