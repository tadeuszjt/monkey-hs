import Control.Monad (unless, when)
import Control.Monad.State
import System.IO
import System.Environment

import qualified Parser as P
import qualified Evaluate as E
import qualified AST as S
import qualified Compiler as C
import qualified Lexer as L
import qualified CGen as G

main :: IO ()
main = do
    [fname] <- getArgs
    h <- openFile fname ReadMode
    content <- hGetContents h
    let tokens = L.alexScanTokens content
    let prog = P.parseTokens tokens
    case C.compile prog of
        Left e -> printError e content fname
        Right p -> evalStateT (G.prog p) G.initGenState
	where
		printError (L.AlexPn offset line col, str) contents fname = do
			let lns = lines contents
			let ln = lns !! (line - 1)
			let ntabs = length (takeWhile (=='\t') ln)
			mapM_ (hPutStrLn stderr) [
				"",
				fname ++ ":" ++ show line ++ ":" ++ show col ++ ": " ++ str,
				if line > 1 then '\t' : (lns !! (line - 2)) else "",
				'\t' : ln,
				'\t' : replicate ntabs '\t' ++ (replicate (col - 1 - ntabs*4) '-' ++ "^"),
				""
				]
