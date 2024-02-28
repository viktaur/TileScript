import TileScriptTokens
import TileScriptGrammar
import TileScriptTypes
import System.IO
import System.Environment
import Control.Exception

main :: IO ()
main = catch main' noParse

main' = do  (fileName : _ ) <- getArgs
            contents <- readFile fileName
            let parsedProg = (parseTileScript . alexScanTokens) contents
            putStrLn $ "parsed as: " ++ (show parsedProg)
            let typeCheckerOut = typeOf [] parsedProg
            putStrLn $ "type: " ++ (show typeCheckerOut)

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()
