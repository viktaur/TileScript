import TileScriptTokens
import TileScriptGrammar
import TileScriptTypes
import TileScriptEval
import System.Environment
import Control.Exception
import System.Directory
import System.IO

main :: IO ()
main = do   (programFileName : _) <- getArgs
            script <- readFile programFileName
            let lexed = (alexScanTokens script)
            putStrLn (show lexed)
            putStrLn " "
            let parsedProg = parseTileScript lexed
            putStrLn (show parsedProg)
            putStrLn (" ")
            let typecheck = typeOf [] parsedProg
            putStrLn (show typecheck)
