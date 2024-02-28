module TileScriptFunctions where

import TileScriptGrammar
import System.IO
import Data.List

-- rotate function implementation
rotate :: [[Int]] -> Int -> Exp
rotate ls 0 = TsTile ls
rotate ls n = rotate (rotate' ls) (n-1)
    where
        rotate' = transpose . reverse
-- rotate _ _ = TsTile [[]]

-- reflectX Tile function implementation
reflectXTile :: [[Int]] -> Exp
reflectXTile = TsTile . reverse
 
-- reflectY Tile function implementation
reflectYTile :: [[Int]] -> Exp
reflectYTile = TsTile . transpose . reverse . transpose

-- reflectX Base function implementation
reflectXBase :: [[Int]] -> Exp
reflectXBase = TsTile . reverse
 
-- reflectY Base function implementation
reflectYBase:: [[Int]] -> Exp
reflectYBase = TsTile . transpose . reverse . transpose


-- newTile function implementation
newTile :: Int -> Exp
newTile 0 = TsTile [[]]
newTile n = TsTile ls
    where
        ls = [[0 | i <- [0..(n-1)]] | i <- [0..(n-1)]]

-- height function implementation
height :: [[Int]] -> Exp
height ls = TsInt $ length ls

-- width function implementation
width :: [[Int]] -> Exp
width ls = TsInt $ length (head ls)


-- newBase function implementation
newBase :: Int -> Int -> Int -> Int -> [[[[Int]]]] -> Exp
newBase hi wi h w ts = TsBase $ [ [ (pickTile i j) !! (i `mod` hi) !! (j `mod` wi) | j <- [0..(wi*w - 1)] ] | i <- [0..(hi*h - 1)] ]
    where
        pickTile i j = ts !! (i `div` hi) !! (j `div` wi)

-- placeTile function implementation
placeTile :: [[Int]] -> [[Int]] -> Int -> Int -> Int -> Exp 
placeTile st bt n y x
    -- | y + n > numRows || x + n > numCols = error "Tile placement out of bounds"
    = TsTile $ [ [ place i j | j <- [0..(numCols-1)] ] | i <- [0..(numRows-1)] ]
    where
        numRows = length bt
        numCols = length $ head bt
        place i j = if (i `elem` [y..(y+n-1)]) && (j `elem` [x..(x+n-1)])
                        then st !! (i-y) !! (j-x)
                        else bt !! i !! j

-- neg function implementation
negF :: [[Int]] -> Exp
negF xs = TsTile $ map (map (\x -> if x == 1 then 0 else 1 )) xs

-- xor function implementation
xorF :: [[Int]] -> [[Int]] -> Exp
xorF xs ys = TsTile $ zipWith (zipWith (\x y -> if x + y == 1 then 1 else 0 )) xs ys


-- and function implementation
andF :: [[Int]] -> [[Int]] -> Exp
andF xs ys = TsTile $ zipWith (zipWith (\x y -> if x == 1 && y == 1 then 1 else 0 )) xs ys

-- or function implementation
orF :: [[Int]] -> [[Int]] -> Exp
orF xs ys = TsTile $ zipWith (zipWith (\x y -> if x == 1 || y == 1 then 1 else 0 )) xs ys

-- compose function implementation
compose :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> Exp
compose a b c d = TsTile $ [ [ (pickTile i j) !! (i `mod` n) !! (j `mod` n) | j <- [0..(n*2 - 1)] ] | i <- [0..(n*2 - 1)] ]
    where
        n = length a
        ts = [[a,b], [d,c]]
        pickTile i j = ts !! (i `div` n) !! (j `div` n)

-- placeBase function implementation
placeBase :: [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int -> Exp
-- placeBase bs tl h w y x = TsTile $ [ [ place i j | j <- [0..((length tl) - 1)] ] | i <- [0..((length tl) - 1)] ]
--     where
--         place i j = if (i `elem` [y..(y+h-1)]) && (j `elem` [x..(x+w-1)]) then bs !! i !! j else tl !! i !! j

placeBase bs tl h w y x
    -- | y + n > numRows || x + n > numCols = error "Tile placement out of bounds"
    = TsTile $ [ [ place i j | j <- [0..(numColsTl-1)] ] | i <- [0..(numRowsTl-1)] ]
    where
        numRowsTl = length tl
        numColsTl = length $ head tl
        place i j = if (i `elem` [y..(y+h-1)]) && (j `elem` [x..(x+w-1)])
                        then bs !! (i-y) !! (j-x)
                        else tl !! i !! j


-- scale Tile function implementation
scaleTile :: [[Int]] -> Int -> Exp
scaleTile xs n = TsTile $ [ [x | x <- y, i <- [0..(n-1)]] | y <- xs, i <- [0..(n-1)] ]

-- scale Base function implementation
scaleBase :: [[Int]] -> Int -> Exp
scaleBase xs n = TsBase $ [ [x | x <- y, i <- [0..(n-1)]] | y <- xs, i <- [0..(n-1)] ]

-- export function implementation
export :: [[Int]] -> Exp
export xss = TsString (unlines [concatMap show row | row <- xss])

-- subtile function implementation
subtile :: [[Int]] -> (Int, Int) -> Int -> Exp
subtile ls (x,y) n = TsTile $ [[ls !! i !! j | j <- [y..(y+(n-1))]] | i <- [x..(x+(n-1))]]

-- indexCell function implementation
indexCell :: [[Int]] -> Int -> Int -> Exp
indexCell ts i j = TsCell $ ts !! i !! j

-- joinHorizontal function implementation for the contents of Tile and Base. Always returns a Base
joinHorizontally :: [[Int]] -> [[Int]] -> Exp
joinHorizontally [[]] ys = TsBase  ys
joinHorizontally xs [[]] = TsBase xs
joinHorizontally xs ys = TsBase $ zipWith (++) xs ys

-- joinVertically function implementation for the contents of Tile and Base. Always returns a Base
joinVertically :: [[Int]] -> [[Int]] -> Exp
joinVertically [[]] ys = TsBase  ys
joinVertically xs [[]] = TsBase xs
joinVertically xs ys = TsBase $ xs ++ ys

repeatX :: [[Int]] -> Int -> Exp
repeatX ls n = TsBase $ map (\xs -> take (n*(length xs)) (cycle xs)) ls

repeatY :: [[Int]] -> Int -> Exp
repeatY ls n = TsBase $ take (n*(length ls)) (cycle ls) 

toBase :: [[Int]] -> Exp
toBase ls = TsBase ls

-- repeatY :: [[Int]] -> Int -> Exp

