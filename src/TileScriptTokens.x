-- Things we need to define for our lexer:
-- Types:
-- Int tokens
-- String tokens
-- Char tokens
-- Cell tokens
-- Tile tokens
-- [] <- (i.e. arrays)

-- Pre-defined functions:
-- makeTile(args) <- type of args undefined? return type is Tile: 
    -- ? -> Tile
-- read(args) <- args is of file type. return type is undefined but used as input of makeTile
    -- File -> ?
-- neg(args) <- input is of Tile type. return type is of Tile type.
    -- Tile -> Tile
-- xor(args) <- input is of Tile type. return type is of Tile type.
    -- Tile -> Tile
-- and(args) <- input is of Tile type. return type is of Tile type.
    -- Tile -> Tile
-- or(args) <- input is of Tile type. return type is of Tile type.
    -- Tile -> Tile
-- rotate(args) <- input is of Tile type. return type is of Tile type.
    -- Tile -> Tile
-- compose(args) <- input is of Tile type. return type is of Tile type.
    -- Tile -> Tile
-- range(args) <- input type is Int -> Int. return type is an array of ints. could be polymorphic not just for ints?
    -- Int -> Int -> [Int] (this could change depending whether function is polymorphic)
-- floor(args) <- input type is int. return type is int.
    -- Int -> Int
-- getRow(args) <- input is of Tile type. and will return the row of that tile so an array of cells [Cell]
    -- Tile -> [Cell]
-- getCols(args) <- input is of Tile type. and will return the columns of that tile so an array of cells [Cell]
    -- Tile -> [Cell]
-- getSize(args) <- input is of [Cell] type (i.e. the return type of getRows or getCols)? return type is an int.
    -- [Cell] -> Int

-- other things:
-- void <- the return type for functions
-- for ... in ... <- using the Python syntax for for loops
-- if ... else ...
-- func <- used to define a function
-- '('
-- ')'
-- '{'
-- '}'
-- ';'
-- '"'
-- '''

{
    module TileScriptTokens where
}

%wrapper "posn"
$digit = 0-9
$alpha = [a-zA-Z]
$quot = [\"]
$quotable = $printable 
$sinQuot = [\']
$sinDoll = [\$]

tokens :-
  $white+         ; 
  "//".*          ;

-- Types
  Tile                  { \p s -> TokenTileType p }
  Cell                  { \p s -> TokenCellType p }
  Int                   { \p s -> TokenIntType p }
  String                { \p s -> TokenStringType p }
  Char                  { \p s -> TokenCharType p }
  Bool                  { \p s -> TokenBoolType p }
  Unit                  { \p s -> TokenUnitType p }
  Base                  { \p s -> TokenBaseType p }

-- Operators and Keywords
  "&&"                  { \p s -> TokenAndOp p }
  "||"                  { \p s -> TokenOrOp p }
  "=="                  { \p s -> TokenEquality p }
  "!="                  { \p s -> TokenNotEquality p }
  "<-"                  { \p s -> TokenAssign p }
  true                  { \p s -> TokenTrue p }
  false                 { \p s -> TokenFalse p }
  for                   { \p s -> TokenFor p }
  in                    { \p s -> TokenIn p }
  if                    { \p s -> TokenIf p }
  else                  { \p s -> TokenElse p }
  \(                    { \p s -> TokenRParenthesis p }
  \)                    { \p s -> TokenLParenthesis p }
  \[                    { \p s -> TokenRSquareBracket p }
  \]                    { \p s -> TokenLSquareBracket p }
  \{                    { \p s -> TokenRCurlyBracket p }
  \}                    { \p s -> TokenLCurlyBracket p }
  \;                    { \p s -> TokenSemicolon p }
  \%                    { \p s -> TokenModulo p }
  \=                    { \p s -> TokenEquals p }
  \+                    { \p s -> TokenPlus p }
  \-                    { \p s -> TokenMinus p }
  \/                    { \p s -> TokenDivide p }
  \*                    { \p s -> TokenTimes p }
  "<="                  { \p s -> TokenLEqT p }
  ">="                  { \p s -> TokenGEqT p }
  \<                    { \p s -> TokenLT p }
  \>                    { \p s -> TokenGT p }
  \,                    { \p s -> TokenComma p }

-- Functions
  newTile               { \p s -> TokenNewTileFunc p }
  newBase               { \p s -> TokenNewBaseFunc p }
  $sinDoll $digit       { \p s -> TokenReadTileFunc p (read (tail s)) }
  neg                   { \p s -> TokenNegFunc p }
  xor                   { \p s -> TokenXorFunc p }
  and                   { \p s -> TokenAndFunc p }
  or                    { \p s -> TokenOrFunc p }
  rotate                { \p s -> TokenRotateFunc p }
  compose               { \p s -> TokenComposeFunc p }
  range                 { \p s -> TokenRangeFunc p }
  size                  { \p s -> TokenSizeFunc p }
  export                { \p s -> TokenExportFunc p }
  placeTile             { \p s -> TokenPlaceTileFunc p }
  placeBase             { \p s -> TokenPlaceBaseFunc p }
  scale                 { \p s -> TokenScaleFunc p }
  reflectY              { \p s -> TokenReflectYFunc p }
  reflectX              { \p s -> TokenReflectXFunc p }
  height                { \p s -> TokenHeightFunc p }
  width                 { \p s -> TokenWidthFunc p }
  subtile               { \p s -> TokenSubtileFunc p }
  joinY                 { \p s -> TokenJoinYFunc p }
  joinX                 { \p s -> TokenJoinXFunc p }
  repeatX               { \p s -> TokenRepeatXFunc p }
  repeatY               { \p s -> TokenRepeatYFunc p }
  toBase                { \p s -> TokenToBaseFunc p }
  "<x>"                 { \p s -> TokenRepeatXFuncSugar p }
  "<y>"                 { \p s -> TokenRepeatYFuncSugar p }
  \|                    { \p s -> TokenJoinYFuncSugar p }
  "~"                   { \p s -> TokenJoinXFuncSugar p }

  -- Variables and Literals
  $sinQuot $printable $sinQuot               { \p s -> TokenChar p (head s) }
  $quot $quotable* $quot                     { \p s -> TokenString p s }
  $digit+                                    { \p s -> TokenInt p (read s) }
  $alpha+ ($digit| $alpha | @\_)*            { \p s -> TokenVar p s } 

{
-- Each action has type :: AlexPosn -> String -> TileScriptTokens
-- The token type: 

data TileScriptTokens = 
  TokenTileType AlexPosn |
  TokenCellType AlexPosn |
  TokenIntType AlexPosn |
  TokenStringType AlexPosn |
  TokenCharType AlexPosn |
  TokenBoolType AlexPosn |
  TokenTrue AlexPosn |
  TokenFalse AlexPosn |
  TokenRSquareBracket AlexPosn |
  TokenLSquareBracket AlexPosn |
  TokenUnitType AlexPosn |
  TokenBaseType AlexPosn |
  TokenFor AlexPosn |
  TokenIn AlexPosn |
  TokenIf AlexPosn |
  TokenElse AlexPosn |
  TokenRParenthesis AlexPosn |
  TokenLParenthesis AlexPosn |
  TokenRCurlyBracket AlexPosn |
  TokenLCurlyBracket AlexPosn |
  TokenSemicolon AlexPosn |
  TokenModulo AlexPosn |
  TokenEquals AlexPosn |
  TokenPlus AlexPosn |
  TokenMinus AlexPosn |
  TokenDivide AlexPosn |
  TokenTimes AlexPosn |
  TokenLT AlexPosn |
  TokenGT AlexPosn |
  TokenLEqT AlexPosn |
  TokenGEqT AlexPosn |
  TokenComma AlexPosn |
  TokenAndOp AlexPosn |
  TokenOrOp AlexPosn |
  TokenDot AlexPosn |
  TokenAssign AlexPosn |
  TokenNewTileFunc AlexPosn |
  TokenNewBaseFunc AlexPosn |
  TokenReadTileFunc AlexPosn Int |
  TokenNegFunc AlexPosn |
  TokenXorFunc AlexPosn |
  TokenAndFunc AlexPosn | 
  TokenOrFunc AlexPosn |
  TokenRotateFunc AlexPosn |
  TokenComposeFunc AlexPosn |
  TokenRangeFunc AlexPosn | 
  TokenSizeFunc AlexPosn |
  TokenExportFunc AlexPosn |
  TokenPlaceTileFunc AlexPosn |
  TokenPlaceBaseFunc AlexPosn | 
  TokenScaleFunc AlexPosn |
  TokenReflectYFunc AlexPosn |
  TokenReflectXFunc AlexPosn |
  TokenHeightFunc AlexPosn |
  TokenWidthFunc AlexPosn |
  TokenSubtileFunc AlexPosn |
  TokenJoinYFuncSugar AlexPosn |
  TokenJoinXFuncSugar AlexPosn |
  TokenJoinXFunc AlexPosn |
  TokenJoinYFunc AlexPosn |
  TokenRepeatXFuncSugar AlexPosn |
  TokenRepeatYFuncSugar AlexPosn |
  TokenRepeatXFunc AlexPosn |
  TokenRepeatYFunc AlexPosn |
  TokenToBaseFunc AlexPosn |
  TokenString AlexPosn String |
  TokenChar AlexPosn Char |
  TokenInt AlexPosn Int |
  TokenVar AlexPosn String |
  TokenEquality AlexPosn |
  TokenNotEquality AlexPosn
  deriving (Eq,Show) 

tokenPosn :: TileScriptTokens -> String
tokenPosn (TokenTileType p) = stringify p
tokenPosn (TokenCellType p) = stringify p
tokenPosn (TokenIntType p) = stringify p
tokenPosn (TokenStringType p) = stringify p
tokenPosn (TokenCharType p) = stringify p
tokenPosn (TokenBoolType p) = stringify p
tokenPosn (TokenTrue p) = stringify p
tokenPosn (TokenFalse p) = stringify p
tokenPosn (TokenRSquareBracket p) = stringify p
tokenPosn (TokenLSquareBracket p) = stringify p
tokenPosn (TokenUnitType p) = stringify p
tokenPosn (TokenBaseType p) = stringify p
tokenPosn (TokenFor p) = stringify p
tokenPosn (TokenIn p) = stringify p
tokenPosn (TokenIf p) = stringify p
tokenPosn (TokenElse p) = stringify p
tokenPosn (TokenRParenthesis p) = stringify p
tokenPosn (TokenLParenthesis p) = stringify p
tokenPosn (TokenRCurlyBracket p) = stringify p
tokenPosn (TokenLCurlyBracket p) = stringify p
tokenPosn (TokenSemicolon p) = stringify p
tokenPosn (TokenModulo p) = stringify p
tokenPosn (TokenEquals p) = stringify p
tokenPosn (TokenPlus p) = stringify p
tokenPosn (TokenMinus p) = stringify p
tokenPosn (TokenDivide p) = stringify p
tokenPosn (TokenTimes p) = stringify p
tokenPosn (TokenLT p) = stringify p
tokenPosn (TokenGT p) = stringify p
tokenPosn (TokenLEqT p) = stringify p
tokenPosn (TokenGEqT p) = stringify p
tokenPosn (TokenComma p) = stringify p
tokenPosn (TokenAndOp p) = stringify p
tokenPosn (TokenOrOp p) = stringify p
tokenPosn (TokenDot p) = stringify p
tokenPosn (TokenAssign p) = stringify p
tokenPosn (TokenReadTileFunc p _) = stringify p
tokenPosn (TokenNegFunc p) = stringify p
tokenPosn (TokenXorFunc p) = stringify p
tokenPosn (TokenAndFunc p) = stringify p
tokenPosn (TokenOrFunc p) = stringify p
tokenPosn (TokenRotateFunc p) = stringify p
tokenPosn (TokenComposeFunc p) = stringify p
tokenPosn (TokenRangeFunc p) = stringify p
tokenPosn (TokenSizeFunc p) = stringify p
tokenPosn (TokenExportFunc p) = stringify p
tokenPosn (TokenPlaceTileFunc p) = stringify p
tokenPosn (TokenPlaceBaseFunc p) = stringify p
tokenPosn (TokenScaleFunc p) = stringify p
tokenPosn (TokenReflectYFunc p) = stringify p
tokenPosn (TokenReflectXFunc p) = stringify p
tokenPosn (TokenHeightFunc p) = stringify p
tokenPosn (TokenWidthFunc p) = stringify p
tokenPosn (TokenSubtileFunc p) = stringify p
tokenPosn (TokenJoinYFuncSugar p) = stringify p
tokenPosn (TokenJoinXFuncSugar p) = stringify p
tokenPosn (TokenJoinYFunc p) = stringify p
tokenPosn (TokenJoinXFunc p) = stringify p
tokenPosn (TokenRepeatXFunc p) = stringify p
tokenPosn (TokenRepeatYFunc p) = stringify p
tokenPosn (TokenRepeatXFuncSugar p) = stringify p
tokenPosn (TokenRepeatYFuncSugar p) = stringify p
tokenPosn (TokenToBaseFunc p) = stringify p
tokenPosn (TokenInt p _) = stringify p
tokenPosn (TokenVar p _) = stringify p
tokenPosn (TokenString p n) = stringify p
tokenPosn (TokenNewTileFunc p) = stringify p
tokenPosn (TokenNewBaseFunc p) = stringify p
tokenPosn (TokenEquality p) = stringify p
tokenPosn (TokenNotEquality p) = stringify p

stringify :: AlexPosn -> String
stringify (AlexPn a l c) = show l ++ ":" ++ show c
}
