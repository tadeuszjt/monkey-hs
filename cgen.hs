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


strType :: Type -> String
strType typ = case typ of
	TVoid      -> "void"
	TInt       -> "int"
	TBool      -> "bool"
	TString    -> "char*"
	TOrd       -> "Ord"
	TAny       -> "Any"
	TArray _ _ -> "Array"
	TFunc _ _  -> "void*"
	_ -> error $ "unhandled type: " ++ show typ


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


ordTo :: Type -> Val -> String
ordTo typ val =
	if typeOf val == typ
	then strVal val
	else case typ of
		TInt    -> "ordToInt(" ++ strVal val ++ ")"
		TBool   -> "ordToBool(" ++ strVal val ++ ")"
		TString -> "ordToString(" ++ strVal val ++ ")"
		TOrd    -> strVal val


toOrd :: Val -> String
toOrd val = case typeOf val of
	TInt    -> "intToOrd(" ++ strVal val ++ ")"
	TBool   -> "boolToOrd(" ++ strVal val ++ ")"
	TString -> "strToOrd(" ++ strVal val ++ ")"
	TOrd    -> strVal val
	_ -> error $ "can't toOrd " ++ show val


toOrdLit :: Val -> String
toOrdLit val = case typeOf val of
	TInt  -> "{" ++ strVal val ++ ", TInt}"
	TBool -> "{" ++ strVal val ++ ", TBool}"
	TOrd  -> strVal val
	_ -> error $ "can't make ord literal: " ++ show val


toArrayLit :: Val -> String
toArrayLit val = case typeOf val of
	TArray t l -> "{" ++ strVal val ++ ", " ++ show l ++ "}"
	_          -> error $ "cannot make array literal"


toArray :: Val -> String
toArray val = case typeOf val of
	TArray t l -> "arr(" ++ strVal val ++ ", " ++ show l ++ ")"
	_          -> error "can't toArray"


toCType :: Type -> String
toCType typ = case typ of
	TInt -> "TInt"
	TBool -> "TBool"
	TString -> "TString"
	TOrd -> "TOrd"
	TArray _ _ -> "TArray"
	TAny -> "TAny"

toTypedArray :: Val -> String
toTypedArray val = case typeOf val of
	TArray t l -> "tarr(" ++ strVal val ++ ", " ++ show l ++ ", " ++ toCType t ++ ")"
	_ -> error $ "couldn't toTypedArray"


toAnyLit :: Val -> String
toAnyLit val
	| isOrd (typeOf val)   = "ordToAny(" ++ toOrd val ++ ")"
	| isArray (typeOf val) = "tarrToAny(" ++ toTypedArray val ++ ")" 
	| otherwise            = error "can't toAnyLit"


strVal :: Val -> String
strVal val = case val of
	VInt i             -> show i
	VBool b            -> if b then "true" else "false"
	VString str        -> "\"" ++ str ++ "\""
	VIdent id _        -> strId id
	VCall id args _    -> concat [strId id, "(", commaSep (map toOrd args), ")"]
	VInfix _ _ _ _     -> strInfix val
	VSubscript arr ind -> strVal arr ++ "[" ++ strVal ind ++ "]"
	_ -> error $ "strVal: " ++ show val


strInfix :: Val -> String
strInfix (VInfix op v1 v2 typ) = intercalate " " $ case (typeOf v1, typeOf v2) of
	(TOrd, TOrd) -> [ordTo TInt v1, strOp op, ordTo TInt v2]
	(TOrd, _)    -> [ordTo TInt v1, strOp op, strVal v2]
	(_, TOrd)    -> [strVal v1, strOp op, ordTo TInt v2]
	(_, _)       -> [strVal v1, strOp op, strVal v2]

			
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
		"#define _len(x) (sizeof(x) / sizeof(*(x)))",
		"",
		"typedef enum {",
		"\tTInt,",
		"\tTBool,",
		"\tTString,",
		"\tTOrd,",
		"\tTAny,",
		"\tTArray,",
		"} Type;",
		"",
		"typedef struct {",
		"\tunion { int asInt; bool asBool; char* asString; };",
		"\tType  type;",
		"} Ord;",
		"",
		"typedef struct {",
		"\tvoid* ptr;",
		"\tint   len;",
		"} Array;",
		"",
		"typedef struct {",
		"\tvoid* ptr;",
		"\tint   len;",
		"\tType  type;",
		"} TypedArray;",
		"",
		"typedef struct {",
		"\tunion {",
		"\t\tOrd        asOrd;",
		"\t\tTypedArray asArray;",
		"\t};",
		"\tType type;",
		"} Any;",
		"",
		"Array arr(void* ptr, int len) {",
		"\tArray a = {ptr, len}; return a;",
		"}",
		"",
		"TypedArray tarr(void* ptr, int len, Type type) {",
		"\tTypedArray a = {ptr, len, type}; return a;",
		"}",
		"",
		"Any ordToAny(Ord ord) {",
		"\tAny a = {ord, TOrd}; return a;",
		"}",
		"",
		"Any tarrToAny(TypedArray tarr) {",
		"\tAny a; a.asArray = tarr; a.type = TArray; return a;",
		"}",
		"",
		"int   ordToInt(Ord ord)    { assert(ord.type == TInt); return ord.asInt; }",
		"bool  ordToBool(Ord ord)   { assert(ord.type == TBool); return ord.asBool; }",
		"char* ordToString(Ord ord) { assert(ord.type == TString); return ord.asString; }",
		"Ord   intToOrd(int i)      { Ord ord = {i, TInt}; return ord; }",
		"Ord   boolToOrd(bool b)    { Ord ord = {b, TBool}; return ord; }",
		"Ord   strToOrd(char* s) { Ord ord; ord.type = TString; ord.asString = s; return ord; }",
		"",
		"void printOrd(Ord ord) {",
		"\tswitch (ord.type) {",
		"\t\tcase TInt:    printf(\"%d\", ord.asInt); break;",
		"\t\tcase TBool:   printf(\"%s\", ord.asBool ? \"true\" : \"false\"); break;",
		"\t\tcase TString: fputs(ord.asString, stdout); break;",
		"\t}",
		"}",
		"",
		"void printAny();",
		"",
		"void printTypedArray(TypedArray tarr) {",
		"\tputchar('[');",
		"\tswitch (tarr.type) {",
		"\tcase TInt:",
		"\t\tfor (int i = 0; i < tarr.len; i++)",
		"\t\t\tprintf(\"%d%s\", ((int*)tarr.ptr)[i], i < tarr.len - 1 ? \", \" : \"\");",
		"\t\tbreak;",
		"\tcase TBool:",
		"\t\tfor (int i = 0; i < tarr.len; i++)",
		"\t\t\tprintf(\"%s%s\", ((bool*)tarr.ptr)[i] ? \"true\" : \"false\",",
		"\t\t\t\ti < tarr. len - 1 ? \", \" : \"\");",
		"\t\tbreak;",
			"",
		"\tcase TString:",
		"\t\tfor (int i = 0; i < tarr.len; i++)",
		"\t\t\tprintf(\"%s%s\", ((char**)tarr.ptr)[i], i < tarr.len - 1 ? \", \" : \"\");",
		"\t\tbreak;",
		"",
		"\tcase TOrd:",
		"\t\tfor (int i = 0; i < tarr.len; i++) {",
		"\t\t\tprintOrd(((Ord*)tarr.ptr)[i]);",
		"\t\t\tfputs(i < tarr.len - 1 ? \", \" : \"\", stdout);",
		"\t\t}",
		"\t\tbreak;",
		"",
		"\tcase TAny:",
		"\t\tfor (int i = 0; i < tarr.len; i++) {",
		"\t\t\tprintAny(((Any*)tarr.ptr)[i]);",
		"\t\t\tfputs(i < tarr.len - 1 ? \", \" : \"\", stdout);",
		"\t\t}",
		"\t\tbreak;",
		"\t}",
		"\tputchar(']');",
		"}",
		"",
		"void printAny(Any any) {",
		"\tswitch (any.type) {",
		"\tcase TOrd:",
		"\t\tprintOrd(any.asOrd);",
		"\t\tbreak;",
		"",
		"\tcase TArray:",
		"\t\tprintTypedArray(any.asArray);",
		"\t\tbreak;",
		"\t}",
		"}"
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
	Alloc _ _ _ -> alloc opn
	Return val  -> getRetty >>= \typ -> case typ of
		TOrd -> stmt $ "return " ++ toOrd val
		_    -> stmt $ "return " ++ strVal val
	Set id val  -> stmt $ strId id ++ " = " ++ strVal val
	Print _     -> genPrint opn
	LoopBegin   -> line "while (true) {" >> incIndent
	LoopBreak   -> stmt "break"
	LoopEnd     -> decIndent >> line "}"
	IfBegin val -> line ("if (" ++ ordTo TBool val ++ ") {") >> incIndent
	IfElse      -> decIndent >> line "} else {" >> incIndent
	IfEnd       -> decIndent >> line "}"
	Expr val    -> stmt $ strVal val
	_ -> error $ "unhandled operation: " ++ show opn


alloc :: Opn -> Gen ()
alloc (Alloc id vals typ) = do
	line $ strType typ ++ " " ++ strId id ++ "[] = {"
	incIndent
	mapM_ line $ map (\v -> case typ of
		TArray _ _ -> toArrayLit v
		TOrd       -> toOrdLit v
		TAny       -> toAnyLit v
		_          -> strVal v
		++ ",") vals
	decIndent
	stmt "}"



assign :: Opn -> Gen ()
assign opn@(Assign id val) = case val of
	VIdent _ (TFunc targs retty) -> stmt $ strTFunc targs retty id ++ " = " ++ strVal val
	_                            -> stmt $ strType (typeOf val) ++ " " ++ strId id ++ " = " ++ strVal val
	where
		strTFunc targs retty id =
			concat [strType retty, " (*", strId id, ")(", commaSep (map strType targs), ")"]


genPrint :: Opn -> Gen ()
genPrint (Print vals) = case vals of
	[]     -> return ()
	[val]  -> genPrint' (Print [val]) >> stmt "putchar('\\n')"
	(v:vs) -> genPrint' (Print [v]) >> stmt "fputs(\", \", stdout)" >> genPrint (Print vs)
genPrint' (Print [val]) = case typeOf val of
	TInt    -> stmt $ "printf(\"%d\", " ++ strVal val ++ ")"
	TBool   -> stmt $ "fputs(" ++ strVal val ++ " ? \"true\" : \"false\", stdout)"
	TOrd    -> stmt $ "printOrd(" ++ toOrd val ++ ")"
	TString -> stmt $ "fputs(" ++ strVal val ++ ", stdout)"
	TArray typ len -> do
		stmt "putchar('[')"
		let lenStr = show len
		line $ "for (int l = " ++ lenStr ++ " - 1, i = 0; i <=l; i++) {"
		incIndent
		let elem = strVal val ++ "[i]"
		stmt . concat $ case typ of
			TInt  -> ["printf(\"%d%s\", ", elem, ", i<l ? \", \":\"\")"]
			TBool -> ["printf(\"%s%s\", ", elem, " ? \"true\" : \"false\", i<l ?\", \":\"\")"]
			TOrd  -> ["printOrd(", elem, ");", " fputs(i<l ? \", \" : \"\", stdout)"]
			TArray _ _ -> ["printf(\"%p%s\", &", elem, ", i<l ? \", \":\"\")"]
			TAny       -> ["printAny(", elem, "); fputs(i<l ? \", \" : \"\", stdout);"]

		decIndent
		line "}"
		stmt "putchar(']')"
	_ -> error $ "can't genPrint': " ++ show val
		



