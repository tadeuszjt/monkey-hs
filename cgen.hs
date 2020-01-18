module CGen where

import Control.Monad.State
import Data.List

import qualified AST as A
import IR


type GenState = Int
type Gen = StateT GenState IO ()


incIndent :: Gen
incIndent = modify (+1)


decIndent :: Gen
decIndent = modify (+(-1))


commaSep = intercalate ", "


strId :: Ident -> String
strId id = case id of
	Var ind -> 'v' : show ind
	Arg ind -> 'a' : show ind
	Ret     -> "ret"


strType :: Type -> String
strType typ = case typ of
	TInt    -> "int"
	TBool   -> "bool"
	TString -> "char*"
	TAny    -> "Any"


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
anyTo typ val = case typ of
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
	VInt i              -> show i
	VBool b             -> if b then "true" else "false"
	VString str         -> "\"" ++ str ++ "\""
	VIdent id _         -> strId id
	VCall id args _     -> concat [strId id, "(", commaSep (map toAny args), ")"]
	VInfix op v1 v2 typ -> intercalate " " $ case (typeOf v1, typeOf v2) of
		(TAny, TAny) -> [anyTo typ v1, strOp op, anyTo typ v2]
		(TAny, _)    -> [anyTo typ v1, strOp op, strVal v2]
		(_, TAny)    -> [strVal v1, strOp op, anyTo typ v2]
		(_, _)       -> [strVal v1, strOp op, strVal v2]

			
-- basic generation
line :: String -> Gen
line str = do
	indent <- get
	liftIO $ putStrLn (replicate indent '\t' ++ str)
	

stmt :: String -> Gen
stmt str =
	line (str ++ ";")


-- generate program
prog :: Prog -> Gen
prog prg = do
	mapM_ line [
		"#include <stdio.h>",
		"#include <stdbool.h>",
		"#include <assert.h>",
		"",
		"typedef enum {",
		"\tTInt,",
		"\tTFloat,",
		"\tTBool,",
		"} Type;",
		"",
		"typedef struct {",
		"\tType type;",
		"\tunion {",
		"\t\tint   Int;",
		"\t\tfloat Float;",
		"\t\tbool  Bool;",
		"\t};",
		"} Any;",
		"",
		"int anyToInt(Any any) {",
		"\tassert(any.type == TInt);",
		"\treturn any.Int;",
		"}",
		"",
		"Any intToAny(int i) {",
		"\tAny any;",
		"\tany.type = TInt;",
		"\tany.Int = i;",
		"\treturn any;",
		"}",
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


func :: Index -> Func -> Gen
func id (TFunc targs retty, opns) = do
	let strargs = intercalate ", " $ zipWith ($) (map strArg targs) [0..]
	line ""
	line $ strType retty ++ " " ++ strId (Var id) ++ "(" ++ strargs ++ ") {"
	incIndent
	stmt $ (strType retty) ++ " ret"
	mapM_ genOpn opns
	stmt "return ret"
	decIndent
	line "}"
	where
		strArg typ num = strType typ ++ " a" ++ show num


genOpn :: Opn -> Gen
genOpn opn = case opn of
	Assign _ _  -> assign opn
	Set Ret val -> stmt $ strId Ret ++ " = " ++ toAny val
	Set id val  -> stmt $ strId id ++ " = " ++ strVal val
	Print _     -> genPrint opn
	LoopBegin   -> line "while (true) {" >> incIndent
	LoopBreak   -> stmt "break"
	LoopEnd     -> decIndent >> line "}"
	IfBegin val -> line ("if (" ++ strVal val ++ ") {") >> incIndent
	IfElse      -> decIndent >> line "} else {" >> incIndent
	IfEnd       -> decIndent >> line "}"


assign :: Opn -> Gen
assign (Assign id val) = stmt $ case typeOf val of
	TFunc targs retty -> strTFunc targs retty id ++ " = &" ++ strVal val
	_                 -> strType (typeOf val) ++ " " ++ strId id ++ " = " ++ strVal val
	where
		strTFunc targs retty id =
			strType retty
			++ " (*"
			++ strId id
			++ ")("
			++ intercalate ", " (map strType targs)
			++ ")"


genPrint :: Opn -> Gen
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
		fmt (v:vs) =
			let (f, a) = fmt [v]; (fs, as) = fmt vs
			in (f ++ ", " ++ fs, a ++ ", " ++ as)
