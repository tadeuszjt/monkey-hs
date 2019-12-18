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
		(x:xs) <- P.parseStr line
		print x
		let s = runStateT (E.evStmt $ x) env
		print s

main :: IO ()
main = do
	process []
