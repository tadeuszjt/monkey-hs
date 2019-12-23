import Control.Monad (unless, when)
import Control.Monad.State
import System.IO (isEOF)

import qualified Parser as P
import qualified Evaluate as E
import qualified AST as S
import qualified Compiler as C
import CGen

main :: IO ()
main = do
    content <- getContents
    prog <- P.parseStr content
    case C.evalCmp (C.cmpProg prog) of
        Left str -> print str
        Right p  -> cgenProg p
	
