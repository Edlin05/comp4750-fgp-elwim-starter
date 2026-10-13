import qualified Scanner
import qualified Parser
import Interpreter (Context, execute)
import System.Environment (getArgs)
import qualified Data.Map as Map

procFiles :: [String] -> Context -> IO()
procFiles [] _ = return ()
procFiles (fileName : rest) ctx = do
  content <- readFile fileName 
  case Scanner.scan content >>= Parser.parse of 
    Left msg -> print msg
    Right ast -> do 
      ctx' <- execute ast ctx 
      procFiles rest ctx'

main :: IO ()
main = do
  args <- getArgs
  procFiles args Map.empty
