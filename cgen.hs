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
	TOrd      -> "Any"
	TFunc _ _ -> "void*"
	TStaticArray t -> strType t ++ "*"


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
	TString -> "stringToOrd(" ++ strVal val ++ ")"
	TOrd    -> strVal val
	_ -> error $ "can't toOrd " ++ show val


strVal :: Val -> String
strVal val = case val of
	VInt i             -> show i
	VBool b            -> if b then "true" else "false"
	VString str        -> "\"" ++ str ++ "\""
	VIdent id _        -> strId id
	VCall id args _    -> concat [strId id, "(", commaSep (map toOrd args), ")"]
	VInfix _ _ _ _     -> strInfix val
	VStaticArray _ _   -> strStaticArray val
	VSubscript arr ind -> strVal arr ++ "[" ++ strVal ind ++ "]"


strStaticArray :: Val -> String
strStaticArray (VStaticArray vals typ) =
	if typ == TOrd
	then "{" ++ commaSep (map toOrd vals) ++ "}"
	else "{" ++ commaSep (map strVal vals) ++ "}"
	

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
		"typedef enum { OInt, OBool } TOrd;",
		"typedef struct {",
		"\tTOrd type;",
		"\tunion { int Int; bool Bool; };",
		"} Ord;",
		"",
		"int  ordToInt(Ord ord)  { assert(ord.type == OInt); return ord.Int; }",
		"bool ordToBool(Ord ord) { assert(ord.type == OBool); return ord.Bool; }",
		"Ord  intToOrd(int i)    { Ord ord = {OInt, i}; return ord; }",
		"Ord  boolToOrd(bool b)  { Ord ord = {OBool, b}; return ord; }",
		"",
		"void printOrd(Ord ord) {",
		"\tswitch (ord.type) {",
		"\t\tcase OInt:  printf(\"%d\", ord.Int); break;",
		"\t\tcase OBool: printf(\"%s\", ord.Bool ? \"true\" : \"false\"); break;",
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
		TOrd -> stmt $ "return " ++ toOrd val
		_    -> stmt $ "return " ++ strVal val
	Set Ret val -> stmt $ strId Ret ++ " = " ++ toOrd val
	Set id val  -> stmt $ strId id ++ " = " ++ strVal val
	Print _     -> genPrint opn
	LoopBegin   -> line "while (true) {" >> incIndent
	LoopBreak   -> stmt "break"
	LoopEnd     -> decIndent >> line "}"
	IfBegin val -> line ("if (" ++ ordTo TBool val ++ ") {") >> incIndent
	IfElse      -> decIndent >> line "} else {" >> incIndent
	IfEnd       -> decIndent >> line "}"
	Expr val    -> stmt $ strVal val


assign :: Opn -> Gen ()
assign opn@(Assign id val) = case val of
	VIdent _ (TFunc targs retty) -> stmt $ strTFunc targs retty id ++ " = " ++ strVal val
	VStaticArray _ _             -> assignArray opn
	_                            -> stmt $ strType (typeOf val) ++ " " ++ strId id ++ " = " ++ strVal val
	where
		strTFunc targs retty id =
			concat [strType retty, " (*", strId id, ")(", commaSep (map strType targs), ")"]

assignArray :: Opn -> Gen ()
assignArray (Assign id (VStaticArray vals typ)) = do
	line $ strType typ ++ " " ++ strId id ++ "[] = {"
	incIndent
	let strs = map (if typ == TOrd then toOrd else strVal) vals
	mapM_ (\s -> line $ s ++ ",") strs
	decIndent
	line "};"
assignArray opn =
	error $ "can't assign array: " ++ show opn


genPrint :: Opn -> Gen ()
genPrint (Print vals) = case vals of
	[]     -> return ()
	[val]  -> genPrint' (Print [val]) >> stmt "putchar('\\n')"
	(v:vs) -> genPrint' (Print [v]) >> stmt "fputs(\", \", stdout)" >> genPrint (Print vs)
	where
		genPrint' (Print [val]) = case typeOf val of
			TInt  -> stmt $ "printf(\"%d\", " ++ strVal val ++ ")"
			TBool -> stmt $ "puts(" ++ strVal val ++ " ? \"true\" : \"false\")"
			TOrd  -> stmt $ "printOrd(" ++ toOrd val ++ ")"
			TStaticArray TInt -> do
				stmt "putchar('[')"
				line $ "for (int i = 0; i < _len(" ++ strVal val ++ ")-1; i++) {"
				incIndent
				stmt $ "printf(\"%d, \", " ++ strVal val ++ "[i])"
				decIndent
				line "}" 
				stmt $ "printf(\"%d\", " ++ strVal val ++ "[_len(" ++ strVal val ++ ")-1])"
				stmt "putchar(']')"
			TStaticArray TBool -> do
				stmt "putchar('[')"
				line $ "for (int i = 0; i < _len(" ++ strVal val ++ "); i++) {"
				incIndent
				stmt $ "fputs(" ++ strVal val ++ "[i] ? \"true\" : \"false\", stdout)"
				line $ "if (i < _len(" ++ strVal val ++ ") -1) { fputs(\", \", stdout); }"
				decIndent
				line "}" 
				stmt "putchar(']')"
			_ -> error $ "can't genPrint': " ++ show val



