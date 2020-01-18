module CGen where

import Control.Monad.State
import Data.List

import qualified AST as A
import IR


data GenState
	= GenState {
		indent :: Int,
		retty  :: Type
	}

initGenState = GenState {
	indent = 0,
	retty = TInt
	}

type Gen a = StateT GenState IO a


incIndent :: Gen ()
incIndent = do
	ind <- gets indent
	modify $ \s -> s { indent = ind + 1 }


decIndent :: Gen ()
decIndent = do
	ind <- gets indent
	modify $ \s -> s { indent = ind - 1 }


getRetty :: Gen Type
getRetty = do
	r <- gets retty
	return r


setRetty :: Type -> Gen ()
setRetty typ =
	modify $ \s -> s { retty = typ }


commaSep = intercalate ", "


strId :: Ident -> String
strId id = case id of
	Var ind -> 'v' : show ind
	Arg ind -> 'a' : show ind
	Ret     -> "ret"


strType :: Type -> String
strType typ = case typ of
	TInt      -> "int"
	TBool     -> "bool"
	TString   -> "char*"
	TAny      -> "Any"
	TFunc _ _ -> "void*"
	TStaticArray t -> strType t ++ "[]"


strOp :: A.Op -> String
strOp op = case op of
	A.Plus   -> "+"
	A.Minus  -> "-"
	A.Times  -> "*"
	A.Divide -> "/"
	A.LThan  -> "<"
	A.GThan  -> ">"
	A.Mod    -> "%"
	A.EqEq   -> "=="
	A.OrOr   -> "||"
	_ -> error "strOp invalid"


anyTo :: Type -> Val -> String
anyTo typ val =
	if typeOf val == typ
	then strVal val
	else case typ of
		TInt    -> "anyToInt(" ++ strVal val ++ ")"
		TBool   -> "anyToBool(" ++ strVal val ++ ")"
		TString -> "anyToString(" ++ strVal val ++ ")"
		TAny    -> strVal val


toAny :: Val -> String
toAny val = case typeOf val of
	TInt    -> "intToAny(" ++ strVal val ++ ")"
	TBool   -> "boolToAny(" ++ strVal val ++ ")"
	TString -> "stringToAny(" ++ strVal val ++ ")"
	TAny    -> strVal val


strVal :: Val -> String
strVal val = case val of
	VInt i             -> show i
	VBool b            -> if b then "true" else "false"
	VString str        -> "\"" ++ str ++ "\""
	VIdent id _        -> strId id
	VCall id args _    -> concat [strId id, "(", commaSep (map toAny args), ")"]
	VInfix _ _ _ _     -> strInfix val
	VStaticArray _ _   -> strStaticArray val
	VSubscript arr ind -> strVal arr ++ "[" ++ strVal ind ++ "]"

strStaticArray :: Val -> String
strStaticArray (VStaticArray vals typ) =
	if typ == TAny
	then "{" ++ commaSep (map toAny vals) ++ "}"
	else "{" ++ commaSep (map strVal vals) ++ "}"
	
strInfix :: Val -> String
strInfix val@(VInfix op v1 v2 typ) = intercalate " " $ case (op, typeOf v1, typeOf v2) of
	(_, TInt, TInt) -> [strVal v1, strOp op, strVal v2]
	(_, TAny, TInt) -> [anyTo TInt v1, strOp op, strVal v2]
	(_, TInt, TAny) -> [strVal v1, strOp op, anyTo TInt v2]
	(_, TAny, TAny) -> [anyTo TInt v1, strOp op, anyTo TInt v2]
			
-- basic generation
line :: String -> Gen ()
line str = do
	indent <- gets indent
	liftIO $ putStrLn (replicate indent '\t' ++ str)
	

stmt :: String -> Gen ()
stmt str =
	line (str ++ ";")


-- generate program
prog :: Prog -> Gen ()
prog prg = do
	mapM_ line [
		"#include <stdio.h>",
		"#include <stdbool.h>",
		"#include <assert.h>",
		"",
		"typedef enum { TInt, TFloat, TBool } Type;",
		"typedef struct {",
		"\tType type;",
		"\tunion { int Int; float Float; bool  Bool; };",
		"} Any;",
		"",
		"int  anyToInt(Any any)  { assert(any.type == TInt); return any.Int; }",
		"bool anyToBool(Any any) { assert(any.type == TBool); return any.Bool; }",
		"Any  intToAny(int i)    { Any any; any.type = TInt; any.Int = i; return any; }",
		"Any  boolToAny(bool b)  { Any any; any.type = TBool; any.Bool = b; return any; }",
		"",
		"void printAny(Any any) {",
		"\tswitch (any.type) {",
		"\tcase TInt:",
		"\t\t printf(\"%d\", any.Int);",
		"\t}",
		"}",
		""
		]

	mapM_ (\(id, fn) -> func id fn) $ reverse prg

	line ""
	line "int main() {"
	incIndent
	stmt "v0()"
	stmt "return 0"
	decIndent
	line "}"


func :: Index -> Func -> Gen ()
func id (TFunc targs retty, opns) = do
	setRetty retty
	let strargs = intercalate ", " $ zipWith ($) (map strArg targs) [0..]
	line ""
	line $ strType retty ++ " " ++ strId (Var id) ++ "(" ++ strargs ++ ") {"
	incIndent
	mapM_ genOpn opns
	decIndent
	line "}"
	where
		strArg typ num = strType typ ++ " a" ++ show num


genOpn :: Opn -> Gen ()
genOpn opn = case opn of
	Assign _ _  -> assign opn
	Return val  -> getRetty >>= \typ -> case typ of
		TAny -> stmt $ "return " ++ toAny val
		_    -> stmt $ "return " ++ strVal val
	Set Ret val -> stmt $ strId Ret ++ " = " ++ toAny val
	Set id val  -> stmt $ strId id ++ " = " ++ strVal val
	Print _     -> genPrint opn
	LoopBegin   -> line "while (true) {" >> incIndent
	LoopBreak   -> stmt "break"
	LoopEnd     -> decIndent >> line "}"
	IfBegin val -> line ("if (" ++ anyTo TBool val ++ ") {") >> incIndent
	IfElse      -> decIndent >> line "} else {" >> incIndent
	IfEnd       -> decIndent >> line "}"


assign :: Opn -> Gen ()
assign (Assign id val) = stmt $ case typeOf val of
	TFunc targs retty -> strTFunc targs retty id ++ " = " ++ strVal val
	TStaticArray typ  -> intercalate " " [strType typ, strId id ++ "[]", "=", strVal val]
	_                 -> strType (typeOf val) ++ " " ++ strId id ++ " = " ++ strVal val
	where
		strTFunc targs retty id = concat [
			strType retty,
			" (*",
			strId id,
			")(",
			intercalate ", " (map strType targs),
			")"
			]


genPrint :: Opn -> Gen ()
genPrint (Print vals) =
	let (fmts, args) = fmt vals
	in stmt $ "printf(\"" ++ fmts ++ "\\n\", " ++ args ++ ")"
	where
		fmt [VBool True] = ("%s", "\"true\"")
		fmt [VBool False] = ("%s", "\"false\"")
		fmt [val] = case typeOf val of
			TInt      -> ("%d", strVal val)
			TBool     -> ("%s", strVal val ++ " ? \"true\" : \"false\"")
			TString   -> ("%s", strVal val)
			TFunc _ _ -> ("%s", "\"func\"") 
			TAny      -> ("%s", "\"any\"")
			TStaticArray _ -> ("%s", "\"array\"")
		fmt (v:vs) =
			let (f, a) = fmt [v]; (fs, as) = fmt vs
			in (f ++ ", " ++ fs, a ++ ", " ++ as)
