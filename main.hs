import Control.Monad (unless, when)
import Control.Monad.State
import System.IO
import System.Environment

import qualified Parser as P
import qualified Evaluate as E
import qualified AST as S
import qualified Compiler as C
import qualified Lexer as L
import CGen

main :: IO ()
main = do
    [fname] <- getArgs
    h <- openFile fname ReadMode
    content <- hGetContents h
    let tokens = L.alexScanTokens content
    let prog = P.parseTokens tokens
    case C.compile prog of
        Left e -> printError e content fname
        Right p -> evalStateT (cgenProg p) 0
	where
		printError (L.AlexPn offset line col, str) contents fname = do
			let lns = lines contents
			mapM_ (hPutStrLn stderr) [
				"",
				fname ++ ":" ++ show line ++ ":" ++ show col ++ ": " ++ str,
				if line > 1 then '\t' : (lns !! (line - 2)) else "",
				'\t' : (lns !! (line - 1)),
				'\t' : (replicate (col - 1) '-' ++ "^"),
				""
				]
