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
getRetty = gets retty


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


toArrayLit :: Val -> String
toArrayLit val = case typeOf val of
	TArray t l -> "{" ++ strVal val ++ ", " ++ show l ++ ", " ++ toCType t ++ "}"


toArray :: Val -> String
toArray val = case typeOf val of
	TArray t l -> "array(" ++ strVal val ++ ", " ++ show l ++ ", " ++ toCType t ++ ")"
	TAny       -> "anyToArray(" ++ strVal val ++ ")"


toCType :: Type -> String
toCType typ = case typ of
	TInt       -> "TInt"
	TBool      -> "TBool"
	TString    -> "TString"
	TOrd       -> "TOrd"
	TArray _ _ -> "TArray"
	TAny       -> "TAny"


toAny :: Val -> String
toAny val
	| isOrd typ   = "ordToAny(" ++ toOrd val ++ ")"
	| isArray typ = "arrayToAny(" ++ toArray val ++ ")" 
	| typ == TAny = strVal val
	where
		typ = typeOf val


strVal :: Val -> String
strVal val = case val of
	VInt i             -> show i
	VBool b            -> if b then "true" else "false"
	VString str        -> "\"" ++ str ++ "\""
	VIdent id _        -> strId id
	VCall id args _    -> concat [strId id, "(", commaSep (map toOrd args), ")"]
	VInfix _ _ _ _     -> strInfix val
	VSubscript arr ind -> strSubscript val



strSubscript :: Val -> String
strSubscript (VSubscript arr ind) = concat $ case typeOf arr of
	TArray (TArray t _) _ -> ["access(", strVal arr, "[", strVal ind, "], ", strType t, ")"]
	TAny                  -> ["accessAny(", strVal arr, ", ", strVal ind, ")"]
	_                     -> [strVal arr, "[", strVal ind, "]"]


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
		"#include <cheader.h>"
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
		TAny       -> toAny v
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
	TInt        -> stmt $ "printf(\"%d\", " ++ strVal val ++ ")"
	TBool       -> stmt $ "fputs(" ++ strVal val ++ " ? \"true\" : \"false\", stdout)"
	TString     -> stmt $ "fputs(" ++ strVal val ++ ", stdout)"
	TOrd        -> stmt $ "printOrd(" ++ toOrd val ++ ")"
	TAny        -> stmt $ "printAny(" ++ toAny val ++ ")"
	TArray _  _ -> printArray val
	_           -> error $ "can't genPrint': " ++ show val


printArray :: Val -> Gen ()
printArray val = do
	let TArray typ len = typeOf val
	stmt "putchar('[')"
	line $ "for (int i = 0; i < " ++ show len ++ "; i++) {"
	incIndent
	let elemStr = strVal val ++ "[i]"
	case typ of
		TInt       -> stmt $ "printf(\"%d\", " ++ elemStr ++ ")" 
		TBool      -> stmt $ "fputs(" ++ elemStr ++ " ? \"true\" : \"false\", stdout)"
		TString    -> stmt $ "fputs(" ++ elemStr ++ ", stdout)"
		TOrd       -> stmt $ "printOrd(" ++ elemStr ++ ")"
		TAny       -> stmt $ "printAny(" ++ elemStr ++ ")"
		TArray t _ -> stmt $ "printArray(" ++ elemStr ++ ")"

	stmt $ "if (i < " ++ show (len - 1) ++ ") fputs(\", \", stdout)"
	decIndent
	line "}"
	stmt "putchar(']')"
		



