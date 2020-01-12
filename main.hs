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
    prog <- P.parseTokens tokens
    case C.evalCmp (C.cmpProg prog) of
        Left str -> print ("error: " ++ str)
        Right p  -> execStateT (cgenProg p) 0 >> return ()
	
