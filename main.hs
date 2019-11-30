import Control.Monad (unless, when)
import Control.Monad.State
import System.IO (isEOF)
import qualified Parser as P
import qualified Evaluate as E

process :: E.Environment -> IO ()
process env = do
	done <- isEOF
	if done
	then do
		print env
		putStrLn "done"
	else do
		line <- getLine
		prog <- P.parseStr line
		print prog
		let (objParse, env') = runState (E.evalProg prog) env
		print objParse
		print env'
		process env'

main :: IO ()
main = do
	process E.emptyEnv
