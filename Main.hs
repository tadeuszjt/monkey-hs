import Control.Monad (unless, when)
import Control.Monad.State
import System.IO
import System.Environment

import qualified Lexer as L
import qualified Parser as P
import qualified AST as S
import qualified TypeChecker as T
import qualified Compiler as C
import qualified IR as I
import qualified CGen as G

main :: IO ()
main = do
    [fname] <- getArgs
    h <- openFile fname ReadMode
    hOpns <- openFile "opns" WriteMode
    content <- hGetContents h
    let tokens = L.alexScanTokens content
    let ast = P.parseTokens tokens
    case T.compile ast of
        Left e     -> printError e content fname
        Right ast' -> case C.compile ast' of
			Left e     -> printError e content fname
			Right prog -> hPutStrLn hOpns (show prog) >> G.generate prog
    hClose h
    hClose hOpns
	where
		printError (L.AlexPn offset l c, str) contents fname = do
			let lines' = lines [if c == '\t' then ' ' else c | c <- contents ]
			let line = lines' !! (l-1)
			mapM_ (hPutStrLn stderr) [
				"",
				concat [fname, ":", show l, ":", show c, ": ", str],
				if l>=2 then lines' !! (l-2) else "",
				line,
				replicate (c-1) '-' ++ "^",
				""
				]
