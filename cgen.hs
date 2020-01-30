module CGen where

import Control.Monad.State
import Data.List
import qualified Data.Map as Map

import qualified AST as A
import IR


-- Generator Monad
data GenState = GenState {
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
	TArrayPtr  -> "Array"
	TFunc _ _  -> "void*"
	_          -> error $ "can't strCType: " ++ show typ


strType :: Type -> String
strType typ = show typ


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


toArray :: Val -> String
toArray (Ident id (TArray t len)) =
	concat ["array(", strId id, ", ", show len, ", ", strType t, ")"]


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
	(TInt, TOrd)            -> "ordToInt(" ++ strVal val ++ ")"
	(TInt, TAny)            -> "anyToOrd(ordToInt(" ++ strVal val ++ "))"
	(TBool, TOrd)           -> "ordToBool(" ++ strVal val ++ ")"
	(TBool, TAny)           -> "anyToOrd(ordToBool(" ++ strVal val ++ "))"
	(TOrd, TInt)            -> "intToOrd(" ++ strVal val ++ ")"
	(TOrd, TBool)           -> "boolToOrd(" ++ strVal val ++ ")"
	(TOrd, TAny)            -> "anyToOrd(" ++ strVal val ++ ")"
	(TAny, TInt)            -> "ordToAny(intToOrd(" ++ strVal val ++ "))"
	(TAny, TBool)           -> "ordToAny(boolToOrd(" ++ strVal val ++ "))"
	(TAny, TOrd)            -> "ordToAny(" ++ strVal val ++ ")"
	(TAny, TArray _ _)      -> "arrayToAny(" ++ toArray val ++ ")"
	(TAny, TArrayPtr)       -> "arrayToAny(" ++ strVal val ++ ")"
	(TArrayPtr, TArray _ _) -> toArray val 
	(TArrayPtr, TAny)       -> "anyToArray(" ++ strVal val ++ ")"
	_                       -> error $ "invalid strValAs: " ++ show (typ, val)


prog :: Program -> Gen ()
prog p = do
	line ["#include \"cheader.h\""]
	mapM_ func p
	line [""]
	line ["int main() { v0(); return 0; }"]


func :: (Index, Func) -> Gen ()
func (id, Func (TFunc argTypes retType) argIds opns) = do
	let argStrs = zipWith (\t i -> strCType t ++ " " ++ strId i) argTypes argIds
	line []
	line [strCType retType, " ", strId id, "(", commaSep argStrs, ") {"]
	incIndent
	mapM_ opn opns
	decIndent
	line ["}"]


opn :: Opn -> Gen ()
opn op = case op of
	Assign id val typ -> stmt [strCType typ, " ", strId id, " = ", strValAs typ val]
	Alloc id vals typ -> alloc op
	Set id val typ    -> stmt [strId id, " = ", strValAs typ val]
	Return val typ    -> stmt ["return ", strValAs typ val]
	If val            -> line ["if (!(", strValAs TBool val, ")) {"] >> incIndent
	Loop              -> line ["for (;;) {"] >> incIndent
	End               -> decIndent >> line ["}"] 
	Break             -> stmt ["break"]
	Print vals        -> printVals vals


alloc :: Opn -> Gen ()
alloc (Alloc id vals typ) = do
	line [strCType typ, " ", strId id, "[] = {"]
	incIndent
	mapM_ (\val -> line [strValAs typ val, ", "]) vals
	decIndent
	stmt ["}"]
	

printVals :: [Val] -> Gen ()
printVals vals = case vals of
	[]     -> return ()
	[v]    -> printVal v >> stmt ["putchar('\\n')"]
	(v:vs) -> printVal v >> stmt ["fputs(\", \", stdout)"] >> printVals vs
	where printVal val = case typeOf val of
		TInt       -> stmt ["printf(\"%d\", ", strVal val, ")"]
		TBool      -> stmt ["fputs(", strVal val, " ? \"true\" : \"false\", stdout)"]
		TOrd       -> stmt ["printOrd(", strVal val, ")"]
		TArray _ _ -> stmt ["printArray(", toArray val, ")"] 
		TArrayPtr  -> stmt ["printArray(", strVal val, ")"]
		TAny       -> stmt ["printAny(", strVal val, ")"]
		_          -> error $ show val
