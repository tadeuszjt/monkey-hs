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
	liftIO $ putStrLn "#include <stdio.h>"
	liftIO $ putStrLn "#include <stdbool.h>"
	liftIO $ putStrLn "int main() {"
	incIndent
	mapM cgenOpn (ops prog)
	decIndent
	liftIO $ putStrLn "}"
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
	Set str (VInt i) -> cgenStmt $ str ++  " = " ++ show i
	While  _         -> cgenWhile opn
	Print val        -> case typeOf val of
		TInt  -> cgenStmt $ "printf(\"%d\\n\", " ++ strVal val ++ ")" 
		TBool -> cgenStmt $ "puts(" ++ strVal val ++ " ? \"true\" : \"false\")"


cgenWhile :: Opn -> CGen
cgenWhile (While cnd) = do
	cgenLine ""
	cgenLine $ "while (" ++ strVal cnd ++ ") {"
	incIndent
	decIndent
	cgenLine "}"
	

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
	_ -> error "strVal invalid"


strOp :: A.Op -> String
strOp op = case op of
	A.Plus   -> "+"
	A.Minus  -> "-"
	A.Times  -> "*"
	A.Divide -> "/"
	A.LThan  -> "<"
	A.Mod    -> "%"
	A.OrOr   -> "||"
	_ -> error "strOp invalid"
