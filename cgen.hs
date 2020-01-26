module CGen where

import Control.Monad.State
import Data.List
import qualified Data.Map as Map

import qualified AST as A
import IR


-- Generator Monad
data GenState
	= GenState {
		indent  :: Int,
		retty   :: Type
	}

initGenState = GenState {
	indent = 0,
	retty = TInt
	}

type Gen a = StateT GenState IO a


-- Generation primitives
incIndent :: Gen ()
incIndent = do
	ind <- gets indent
	modify $ \s -> s { indent = ind + 1 }


decIndent :: Gen ()
decIndent = do
	ind <- gets indent
	modify $ \s -> s { indent = ind - 1 }


setRetty :: Type -> Gen ()
setRetty typ =
	modify $ \s -> s { retty = typ }


line :: [String] -> Gen ()
line strs = do
	indent <- gets indent
	let tabs = replicate indent '\t'
	liftIO $ putStrLn (tabs ++ concat strs)


stmt strs = line $ strs ++ [";"]


commaSep = intercalate ", "


-- Generate C code
generate :: Program -> IO ()
generate p = do
	evalStateT (prog p) initGenState


strId :: Index -> String
strId ind = 'v' : show ind


strCType :: Type -> String
strCType typ = case typ of
	TVoid      -> "void"
	TInt       -> "int"
	TBool      -> "bool"
	TString    -> "char*"
	TOrd       -> "Ord"
	TAny       -> "Any"
	TArray _ _ -> "Array"
	TFunc _ _  -> "void*"


strOp :: A.Op -> String
strOp op = case op of
	A.Plus   -> "+"
	A.Minus  -> "-"
	A.Times  -> "*"
	A.Divide -> "/"
	A.Mod    -> "%"
	A.LT     -> "<"
	A.GT     -> ">"
	A.LTEq   -> "<="
	A.GTEq   -> ">="
	A.EqEq   -> "=="
	A.OrOr   -> "||"


strVal :: Val -> String
strVal val = case val of
	Int n      -> show n
	Bool b     -> if b then "true" else "false"
	String s   -> "\"" ++ s ++ "\""
	Ident id _ -> strId id


strValAs :: Type -> Val -> String
strValAs typ val
	| typ == typeOf val = strVal val
strValAs typ val = case (typ, typeOf val) of
	(TInt, TOrd)  -> "ordToInt(" ++ strVal val ++ ")"
	(TBool, TOrd) -> "ordToBool(" ++ strVal val ++ ")"
	(TOrd, TInt)  -> "intToOrd(" ++ strVal val ++ ")"
	(TOrd, TBool) -> "boolToOrd(" ++ strVal val ++ ")"


prog :: Program -> Gen ()
prog p = do
	line ["#include \"cheader.h\""]
	mapM_ func p
	line [""]
	line ["int main() { v0(); return 0; }"]


func :: (Index, Func) -> Gen ()
func (id, Func (TFunc argTypes retType) argIds opns) = do
	let argStrs = zipWith (\t i -> strCType t ++ " " ++ strId i) argTypes argIds
	line [""]
	line [strCType retType, " ", strId id, "(", commaSep argStrs, ") {"]
	incIndent
	mapM_ opn opns
	decIndent
	line ["}"]


opn :: Opn -> Gen ()
opn o = case o of
	Assign id val typ -> stmt [strCType typ, " ", strId id, " = ", strValAs typ val]
	Set id val typ    -> stmt [strId id, " = ", strValAs typ val]
	Return val typ    -> stmt ["return ", strValAs typ val]
	If val            -> line ["if (", strValAs TBool val, ") {"] >> incIndent
	Loop              -> line ["for (;;) {"] >> incIndent
	End               -> decIndent >> line ["}"] 
	Break             -> stmt ["break"]
