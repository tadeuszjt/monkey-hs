import Control.Monad (unless, when)
import Control.Monad.State
import System.IO (isEOF)

import qualified Parser as P
import qualified Evaluate as E
import qualified AST as S
import qualified Compiler as C
import qualified Lexer as L
import CGen

main :: IO ()
main = do
    content <- getContents
    let tokens = L.alexScanTokens content
    let prog = P.parseTokens tokens
    case C.compile prog of
        Left e -> printError e content
        Right p -> evalStateT (cgenProg p) 0
	where
		printError (L.AlexPn offset line col, str) contents = do
			let lns = lines contents
			putStrLn ""
			putStrLn $ "error (line: " ++ show line ++ ", col: " ++ show col ++ "): " ++ str
			if line > 1 then putStrLn ('\t' : (lns !! (line - 2))) else putStrLn ""
			putStrLn $ '\t' : (lns !! (line - 1))
			putStrLn $ '\t' : (replicate (col - 1) '-' ++ "^")
			putStrLn ""
