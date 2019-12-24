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
	return ()


cgenLine :: String -> CGen
cgenLine str = do
	indent <- get
	liftIO $ putStrLn (replicate indent '\t' ++ str)


cgenStmt :: String -> CGen
cgenStmt str =
	cgenLine (str ++ ";")


cgenOpn :: Opn -> CGen
cgenOpn opn = case opn of
	Assign str val   -> cgenStmt $ strType (typeOf val) ++ " " ++ str ++ " = " ++ strVal val
	Set str val      -> cgenStmt $ str ++  " = " ++ strVal val
	Loop             -> cgenLine "" >> cgenLine "for (;;) {" >> incIndent
	LoopBreak val    -> cgenLine ("if ((" ++ strVal val ++ ") == false) break;") >> cgenLine ""
	EndLoop          -> decIndent >> cgenLine "}"

	Print val        -> case typeOf val of
		TInt  -> cgenStmt $ "printf(\"%d\\n\", " ++ strVal val ++ ")" 
		TBool -> cgenStmt $ "puts(" ++ strVal val ++ " ? \"true\" : \"false\")"


strType :: Type -> String
strType typ = case typ of
	TInt -> "int"
	TBool -> "bool"


strVal :: Val -> String
strVal val = case val of
	VInt i              -> show i
	VBool True          -> "true"
	VBool False         -> "false"
	VIdent s _          -> s
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
	A.OrOr   -> "||"
	_ -> error "strOp invalid"
