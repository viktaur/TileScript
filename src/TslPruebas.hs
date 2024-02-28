import TileScriptTokens
import TileScriptGrammar
import TileScriptTypes
import TileScriptEval
import System.Environment
import Control.Exception
import System.Directory
import System.IO

main :: IO ()
main = catch main' noParse

main' = do  (n : _ ) <- getArgs
            let dir = "pruebas/problem" ++ n ++ "/"
            script <- readFile (dir ++ "pr" ++ n ++ ".tsl")
            let lexed = (alexScanTokens script)
            print(lexed)
            putStrLn ""
            let parsedProg = parseTileScript lexed
            print(parsedProg)
            putStrLn ""
            -- let typeCheckedProg = typeOf [] parsedProg 
            -- putStrLn("Type checked as: " ++ (show typeCheckedProg))
            all <- getDirectoryContents dir
            let filtered = filter fileNameIsValid all
            contents <- mapM readFile (map (dir ++ ) filtered)
            let tiles = map turnToTile contents
            let env = [(TyTile,"$" ++ show (i + 1),TsTile $ tiles !! i) | i <- [0..(length tiles - 1)]]
            print(env)
            --debug (parsedProg,env,[]) 1000
            putStr ""
            let result = evalLoop parsedProg env
            print(result)
            case result of 
                TsString str -> writeFile "./pruebas/Outputs.txt" str
                _ -> error "The ouput value of the program is not a Tile"


noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

getFileContents :: [String] -> [IO String]
getFileContents fls = map readFile fls

turnToTile :: String -> [[Int]]
turnToTile str = map read' $ lines str

read' :: String -> [Int]
read' [] = []
read' (x:xs) = read [x] : read' xs

makeFileName :: Int -> String
makeFileName n = "tile" ++ show n ++ ".tl"

fileNameIsValid :: String -> Bool
fileNameIsValid s = take 4 s == "tile"
