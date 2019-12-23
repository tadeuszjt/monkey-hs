module CGen where

import qualified Compiler as C

cgenProg :: C.Prog -> IO ()
cgenProg prog = do
	putStrLn "#include <stdio.h>"
	putStrLn "int main() {"
	mapM_ cgenAssign (C.ops prog)
	putStrLn "return 0;"
	putStrLn "}"


cgenAssign :: C.Opn -> IO ()
cgenAssign opn = case opn of
	C.Assign str (C.VInt i) -> putStrLn $ "int " ++ str ++ " = " ++ show i ++ ";"
	C.Set str (C.VInt i)    -> putStrLn $ str ++ " = " ++ show i ++ ";"
	C.Print (C.VInt i)      -> putStrLn $ "printf(\"%d\\n\", " ++ show i ++ ");"
	C.Print (C.VIdent str)  -> putStrLn $ "printf(\"%d\\n\", " ++ str ++ ");"
