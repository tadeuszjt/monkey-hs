import Control.Monad (unless, when)
import Control.Monad.State
import System.IO (isEOF)
import qualified Parser as P
import qualified Evaluate as E
import qualified AST as S

process :: E.Env -> IO ()
process env = do
	done <- isEOF
	if done
	then do
		print env
		putStrLn "done"
	else do
		line <- getLine
		prog <- P.parseStr line
		let s = runStateT (E.evProg prog) env
		case s of
			Right (_, env') -> process env'
			Left str -> print str


main :: IO ()
main = do
	content <- getContents
	prog <- P.parseStr content
	let s = runStateT (E.evProg prog) E.emptyEnv
	print s
