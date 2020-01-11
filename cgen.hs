module CGen where

import Control.Monad.State

import Compiler
import IR
import qualified AST as A
import Data.List


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
cgenLine :: String -> CGen
cgenLine str = do
	indent <- get
	liftIO $ putStrLn (replicate indent '\t' ++ str)
	

cgenStmt :: String -> CGen
cgenStmt str =
	cgenLine (str ++ ";")


-- generate program
cgenProg :: Prog -> CGen
cgenProg prog = do
	mapM_ cgenLine [
		"#include <stdio.h>",
		"#include <stdbool.h>"
		]

	mapM_ (\(id, fn) -> cgenFunc id fn) $ reverse prog

	cgenLine ""
	cgenLine "int main() {"
	incIndent
	cgenStmt "v0()"
	cgenStmt "return 0"
	decIndent
	cgenLine "}"


cgenFunc :: Ident -> Func -> CGen
cgenFunc id (typ, opns) = do
	cgenLine ""
	cgenLine $ "void " ++ strId id ++ "() {"
	incIndent
	mapM_ cgenOpn opns
	decIndent
	cgenLine "}"


cgenOpn :: Opn -> CGen
cgenOpn opn = case opn of
	Assign _ _  -> cgenAssign opn
	Set id val  -> cgenStmt $ strId id ++ " = " ++ strVal val
	Print _     -> cgenPrint opn
	Call id     -> cgenStmt $ strId id ++ "()"
	LoopBegin   -> cgenLine "while (true) {" >> incIndent
	LoopBreak   -> cgenStmt "break"
	LoopEnd     -> decIndent >> cgenLine "}"
	IfBegin val -> cgenLine ("if (" ++ strVal val ++ ") {") >> incIndent
	IfElse      -> decIndent >> cgenLine "} else {" >> incIndent
	IfEnd       -> decIndent >> cgenLine "}"


cgenAssign :: Opn -> CGen
cgenAssign (Assign id val) = cgenStmt $ case valType val of
	TFunc -> "void (*" ++ strId id ++ ")() = &" ++ strVal val
	_     -> strType (valType val) ++ " " ++ strId id ++ " = " ++ strVal val


cgenPrint :: Opn -> CGen
cgenPrint (Print vals) =
	let (fmts, args) = fmt vals
	in cgenStmt $ "printf(\"" ++ fmts ++ "\\n\", " ++ args ++ ")"
	where
		fmt [VBool True] = ("%s", "\"true\"")
		fmt [VBool False] = ("%s", "\"false\"")
		fmt [val] = case valType val of
			TInt    -> ("%d", strVal val)
			TBool   -> ("%s", strVal val ++ " ? \"true\" : \"false\"")
			TString -> ("%s", strVal val)
			TFunc   -> ("%s", "\"func\"") 
		fmt (v:vs) =
			let (f, a) = fmt [v]; (fs, as) = fmt vs
			in (f ++ ", " ++ fs, a ++ ", " ++ as)
