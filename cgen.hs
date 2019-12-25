module CGen where

import Control.Monad.State

import Compiler
import qualified AST as A

type CGenState = Int
type CGen = StateT CGenState IO ()

incIndent :: CGen
incIndent =
	modify (1+)


decIndent :: CGen
decIndent =
	modify (-1+)


cgenProg :: Prog -> CGen
cgenProg prog = do
	mapM_ cgenLine [
		"#include <stdio.h>",
		"#include <stdbool.h>"
		]

	cgenLine ""
	cgenLine "int main() {"

	incIndent
	mapM cgenOpn (ops prog)
	cgenLine "return 0;"
	decIndent

	cgenLine "}"



cgenLine :: String -> CGen
cgenLine str = do
	indent <- get
	liftIO $ putStrLn (replicate indent '\t' ++ str)


cgenStmt :: String -> CGen
cgenStmt str =
	cgenLine (str ++ ";")


cgenOpn :: Opn -> CGen
cgenOpn opn = case opn of
	Print _       -> cgenPrint opn
	Assign id val -> cgenStmt $ strType (typeOf val) ++ " " ++ ("v" ++ show id) ++ " = " ++ strVal val
	Set id val    -> cgenStmt $ ("v" ++ show id) ++  " = " ++ strVal val
	LoopBegin     -> cgenLine "" >> cgenLine "for (;;) {" >> incIndent
	LoopBreak     -> cgenStmt "break" >> cgenLine ""
	LoopEnd       -> decIndent >> cgenLine "}"
	IfBegin val   -> cgenLine ("if (" ++ strVal val ++ ") {") >> incIndent
	IfEnd         -> decIndent >> cgenLine "}"
	IfElse        -> cgenLine "else"


cgenPrint :: Opn -> CGen
cgenPrint (Print vals) =
	let (fs, sn) = fmt vals
	in cgenStmt $ "printf(\"" ++ fs ++ "\\n\", " ++ sn ++ ")"
	where
		fmt [val] = case typeOf val of
			TInt  -> ("%d", strVal val)
			TBool -> ("%s", strVal val ++ " ? \"true\" : \"false\"")
		fmt (x:xs) =
			let (ff, sf) = fmt [x]; (fn, sn) = fmt xs
			in (ff ++ ", " ++ fn, sf ++ ", " ++ sn)
	

strType :: Type -> String
strType typ = case typ of
	TInt -> "int"
	TBool -> "bool"


strVal :: Val -> String
strVal val = case val of
	VInt i              -> show i
	VBool True          -> "true"
	VBool False         -> "false"
	VIdent i _          -> "v" ++ show i
	VInfix op v1 v2 typ -> strVal v1 ++ " " ++ strOp op ++ " " ++ strVal v2


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
