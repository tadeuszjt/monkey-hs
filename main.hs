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
    case C.evalCmp (C.cmpProg prog) of
        Left e -> print e
        Right p -> evalStateT (cgenProg p) 0
