import Control.Monad (unless)
import System.IO (isEOF)
import qualified Parser as P

main :: IO ()
main = do
	content <- getContents
	prog <- P.parseStr content
	putStr $ show prog
