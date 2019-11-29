import Control.Monad (unless, when)
import System.IO (isEOF)
import qualified Parser as P

process :: IO ()
process = do
	line <- getLine
	prog <- P.parseStr line
	putStrLn $ show prog
	main

main :: IO ()
main = do
	done <- isEOF
	if done
	then return ()
	else process
