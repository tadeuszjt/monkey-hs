import Control.Monad (unless, when)
import Control.Monad.State
import System.IO (isEOF)

import qualified Parser as P
import qualified Evaluate as E
import qualified AST as S

main :: IO ()
main = do
	content <- getContents
	prog <- P.parseStr content
	E.execEval $ E.evProg prog
