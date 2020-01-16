module CGen where

import Control.Monad.State
import Data.List

import qualified AST as A
import IR


-- indentation state
type CGenState = Int
type CGen = StateT CGenState IO ()

incIndent :: CGen
incIndent =
	modify (1+)

decIndent :: CGen
decIndent =
	modify (+(-1))


-- conversion functions
strId :: Ident -> String
strId id =
	"v" ++ show id


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


strVal :: Val -> String
strVal val = case val of
	VInt i              -> show i
	VBool b             -> if b then "true" else "false"
	VString str         -> "\"" ++ str ++ "\""
	VIdent id _         -> strId id
	VInfix op v1 v2 typ -> intercalate " " [strVal v1, strOp op, strVal v2]


-- basic generation
line :: String -> CGen
line str = do
	indent <- get
	liftIO $ putStrLn (replicate indent '\t' ++ str)
	

stmt :: String -> CGen
stmt str =
	line (str ++ ";")


-- generate program
prog :: Prog -> CGen
prog prg = do
	mapM_ line [
		"#include <stdio.h>",
		"#include <stdbool.h>",
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
		"\t\tint vInt;",
		"\t\tfloat vFloat;",
		"\t\tbool vBool;",
		"\t};",
		"} Any;"
		]

	mapM_ (\(id, fn) -> func id fn) $ reverse prg

	line ""
	line "int main() {"
	incIndent
	stmt "v0()"
	stmt "return 0"
	decIndent
	line "}"


func :: Ident -> Func -> CGen
func id (TFunc targs retty, opns) = do
	let strargs = intercalate ", " $ zipWith ($) (map strArg targs) [0..]
	line ""
	line $ strType retty ++ " " ++ strId id ++ "(" ++ strargs ++ ") {"
	incIndent
	mapM_ genOpn opns
	decIndent
	line "}"
	where
		strArg typ num = strType typ ++ " a" ++ show num


genOpn :: Opn -> CGen
genOpn opn = case opn of
	Assign _ _  -> assign opn
	Set id val  -> stmt $ strId id ++ " = " ++ strVal val
	Print _     -> genPrint opn
	Call id     -> stmt $ strId id ++ "()"
	LoopBegin   -> line "while (true) {" >> incIndent
	LoopBreak   -> stmt "break"
	LoopEnd     -> decIndent >> line "}"
	IfBegin val -> line ("if (" ++ strVal val ++ ") {") >> incIndent
	IfElse      -> decIndent >> line "} else {" >> incIndent
	IfEnd       -> decIndent >> line "}"


assign :: Opn -> CGen
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


genPrint :: Opn -> CGen
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
		fmt (v:vs) =
			let (f, a) = fmt [v]; (fs, as) = fmt vs
			in (f ++ ", " ++ fs, a ++ ", " ++ as)
