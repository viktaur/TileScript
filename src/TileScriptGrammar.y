{
module TileScriptGrammar where
import TileScriptTokens
}

%name parseTileScript
%tokentype { TileScriptTokens }
%error { parseError }
%token
  Tile             { TokenTileType _ }
  Cell             { TokenCellType _ }
  Int              { TokenIntType _ }
  String           { TokenStringType _ }
  Char             { TokenCharType _ }
  Bool             { TokenBoolType _ }
  Unit             { TokenUnitType _ }
  Base             { TokenBaseType _ }
  true             { TokenTrue _ }
  false            { TokenFalse _ }
  for              { TokenFor _ }
  in               { TokenIn _ }
  if               { TokenIf _ }
  else             { TokenElse _ }
  '=='             { TokenEquality _ }
  '!='             { TokenNotEquality _ }
  '<-'             { TokenAssign _ }
  '('              { TokenRParenthesis _ }
  ')'              { TokenLParenthesis _ }
  '['              { TokenRSquareBracket _ }
  ']'              { TokenLSquareBracket _ }
  '{'              { TokenRCurlyBracket _ }
  '}'              { TokenLCurlyBracket _ }
  ';'              { TokenSemicolon _ }
  '%'              { TokenModulo _ }
  '='              { TokenEquals _ }
  '+'              { TokenPlus _ }
  '-'              { TokenMinus _ }
  '/'              { TokenDivide _ }
  '*'              { TokenTimes _ }
  '<'              { TokenLT _ }
  '>'              { TokenGT _ }
  '<='             { TokenLEqT _ }
  '>='             { TokenGEqT _ }
  ','              { TokenComma _ }
  "&&"             { TokenAndOp _ }
  "||"             { TokenOrOp _ }     
  newTile          { TokenNewTileFunc _ }
  newBase          { TokenNewBaseFunc _ }
  "$"              { TokenReadTileFunc _ $$ }
  neg              { TokenNegFunc _ }
  xor              { TokenXorFunc _ }
  and              { TokenAndFunc _ }
  or               { TokenOrFunc _ }
  rotate           { TokenRotateFunc _ }
  compose          { TokenComposeFunc _ }
  range            { TokenRangeFunc _ }
  size             { TokenSizeFunc _ }
  export           { TokenExportFunc _ }
  placeTile        { TokenPlaceTileFunc _ }
  placeBase        { TokenPlaceBaseFunc _ }
  scale            { TokenScaleFunc _ }
  reflectY         { TokenReflectYFunc _ }
  reflectX         { TokenReflectXFunc _ }
  height           { TokenHeightFunc _ }
  width            { TokenWidthFunc _ }
  subtile          { TokenSubtileFunc _ }
  "|"              { TokenJoinYFuncSugar _ }
  joinY            { TokenJoinYFunc _ }
  "~"              { TokenJoinXFuncSugar _ }
  joinX            { TokenJoinXFunc _ }
  repeatX          { TokenRepeatXFunc _ }
  repeatY          { TokenRepeatYFunc _ }
  toBase           { TokenToBaseFunc _ }
  "<x>"            { TokenRepeatXFuncSugar _ }
  "<y>"            { TokenRepeatYFuncSugar _ }
  charValue        { TokenChar _ $$ }      
  int              { TokenInt _ $$ }
  var              { TokenVar _ $$ }
  stringValue      { TokenString _ $$ } 

-- need to define the associativity of the tokens...
%left ';'
%right '<-' '='
%nonassoc for
%nonassoc in
%nonassoc if
%nonassoc else
%nonassoc newTile newBase toBase readTile neg xor and or rotate compose range size export placeTile placeBase scale reflectX reflectY height width subtile "|" "~" joinX joinY "<x>" "<y>" repeatX repeatY
%nonassoc "&&" "||"
%nonassoc '<' '>' '<=' '>=' '==' '!='
%left '%'
%left '^'
%left '*'
%left '/'
%left '+'
%left '-'
%left ','
%nonassoc int charValue stringValue true false var '(' ')' Tile Cell Int String Char Bool Unit Base '[' ']' 
%nonassoc '{' '}'
%%

Exp : Exp ';' Exp                                                     { TsExprSeparator $1 $3 }
    | if '(' Exp ')' '{' Exp '}' else '{' Exp '}'                     { TsIf $3 $6 $10 }
    | for '(' var in Exp ')' '{' Exp '}'                              { TsFor $3 $5 $8 }
    | Exp '<-' Exp                                                    { TsAssign $1 $3 }
    | Exp '[' Exp ']' '[' Exp ']'                                     { TsIndexCell $1 $3 $6 }
    | '(' Exp ')'                                                     { $2 }
    | Exp '+' Exp                                                     { TsAdd $1 $3}
    | Exp '*' Exp                                                     { TsMul $1 $3 }
    | Exp '-' Exp                                                     { TsSubtract $1 $3}
    | Exp '/' Exp                                                     { TsDivide $1 $3 }
    | Exp '%' Exp                                                     { TsMod $1 $3}
    | Exp '<' Exp                                                     { TsLT $1 $3}
    | Exp '>' Exp                                                     { TsGT $1 $3}
    | Exp '<=' Exp                                                    { TsLEqT $1 $3}
    | Exp '>=' Exp                                                    { TsGEqT $1 $3}
    | Type var '=' Exp                                                { TsVarDecl $1 $2 $4}
    | Exp '==' Exp                                                    { TsEquality $1 $3 }
    | Exp '!=' Exp                                                    { TsNotEquality $1 $3 }
    | Exp "&&" Exp                                                    { TsAndOperator $1 $3 }
    | Exp "||" Exp                                                    { TsOrOperator $1 $3 }
    | newTile '(' Exp ')'                                             { TsPreDefFuncNewTile $3 }     
    | newBase '(' Exp ',' Exp ',' Exp ',' Exp ',' Exp ')'             { TsPreDefFuncNewBase $3 $5 $7 $9 $11 }  
    | "$"                                                             { TsPreDefFuncReadTile $1}
    | export '(' Exp ')'                                              { TsPreDefFuncExport $3 }     
    | neg '(' Exp ')'                                                 { TsPreDefFuncNeg $3}
    | xor '(' Exp ',' Exp ')'                                         { TsPreDefFuncXOR $3 $5 }
    | and '(' Exp ',' Exp ')'                                         { TsPreDefFuncAND $3 $5 }
    | or '(' Exp ',' Exp ')'                                          { TsPreDefFuncAND $3 $5 }
    | rotate '(' Exp ',' Exp ')'                                      { TsPreDefFuncRotate $3 $5}
    | compose '(' Exp ',' Exp ',' Exp ',' Exp ')'                     { TsPreDefFuncCompose $3 $5 $7 $9}
    | range '(' Exp ')'                                               { TsPreDefFuncRange $3 }
    | size '(' Exp ')'                                                { TsPreDefFuncSize $3 }
    | placeTile '(' Exp ',' Exp ',' Exp ',' Exp ',' Exp ')'           { TsPreDefFuncPlaceTile $3 $5 $7 $9 $11 }
    | placeBase '(' Exp ',' Exp ',' Exp ',' Exp ',' Exp ',' Exp ')'   { TsPreDefFuncPlaceBase $3 $5 $7 $9 $11 $13 }
    | scale '(' Exp ',' Exp ')'                                       { TsPreDefFuncScale $3 $5 }
    | reflectY '(' Exp ')'                                            { TsPreDefFuncReflectY $3 }
    | reflectX '(' Exp ')'                                            { TsPreDefFuncReflectX $3 }
    | height '(' Exp ')'                                              { TsPreDefFuncHeight $3 }
    | width '(' Exp ')'                                               { TsPreDefFuncWidth $3 }
    | subtile '(' Exp ',' Exp ',' Exp ')'                             { TsPreDefFuncSubtile $3 $5 $7 }
    | Exp "|" Exp                                                     { TsPreDefFuncJoinY $1 $3 }
    | Exp "~" Exp                                                     { TsPreDefFuncJoinX $1 $3 }
    | joinY '(' Exp ',' Exp ')'                                       { TsPreDefFuncJoinY $3 $5 }
    | joinX '(' Exp ',' Exp ')'                                       { TsPreDefFuncJoinX $3 $5 }
    | repeatX '(' Exp ',' Exp ')'                                     { TsPreDefFuncRepeatX $3 $5 }
    | repeatY '(' Exp ',' Exp ')'                                     { TsPreDefFuncRepeatY $3 $5 }
    | toBase '(' Exp ')'                                              { TsPreDefFuncToBase $3 } 
    | Exp "<x>" Exp                                                   { TsPreDefFuncRepeatX $1 $3 }
    | Exp "<y>" Exp                                                   { TsPreDefFuncRepeatY $1 $3 }
    | int                                                             { TsInt $1 }
    | true                                                            { TsTrue }
    | false                                                           { TsFalse }
    | charValue                                                       { TsChar $1 }
    | stringValue                                                     { TsString $1 }
    | var                                                             { TsVar $1 }
    | '(' Exp ',' Exp ')'                                             { TsPair $2 $4 }
    | '[' ArrayContents ']'                                           { TsArrayDef $2 }
    | '('')'                                                          { TsUnit }


ArrayContents : Exp                     { ArrayContentsBase $1 }
              | Exp ',' ArrayContents   { ArrayContentsRec $1 $3 }
              | {- empty -}             { ArrayContentsEmpty }


Type : Tile           { TyTile }
     | Cell           { TyCell }
     | Int            { TyInt }
     | String         { TyString }
     | Char           { TyChar }
     | Bool           { TyBool }
     | Unit           { TyUnit }
     | Base           { TyBase }
     | '[' Type ']'   { TyArray $2 } 

{

parseError :: [TileScriptTokens] -> a 
parseError [] = error "Unknown Parse Error"
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Exp  = TsExprSeparator Exp Exp | TsIf Exp Exp Exp | TsFor String Exp Exp | TsAssign Exp Exp
          | TsIndexCell Exp Exp Exp | TsAdd Exp Exp | TsMul Exp Exp | TsSubtract Exp Exp | TsDivide Exp Exp 
          | TsMod Exp Exp | TsGT Exp Exp | TsLT Exp Exp | TsGEqT Exp Exp | TsLEqT Exp Exp | TsVarDecl Type String Exp
          | TsEquality Exp Exp | TsNotEquality Exp Exp | TsAndOperator Exp Exp | TsOrOperator Exp Exp
          | TsPreDefFuncNewTile Exp | TsPreDefFuncNewBase Exp Exp Exp Exp Exp | TsPreDefFuncReadTile Int | TsPreDefFuncExport Exp
          | TsPreDefFuncNeg Exp | TsPreDefFuncXOR Exp Exp | TsPreDefFuncAND Exp Exp | TsPreDefFuncOR Exp Exp 
          | TsPreDefFuncRotate Exp Exp | TsPreDefFuncCompose Exp Exp Exp Exp | TsPreDefFuncRange Exp | TsPreDefFuncSize Exp 
          | TsPreDefFuncPlaceTile Exp Exp Exp Exp Exp | TsPreDefFuncPlaceBase Exp Exp Exp Exp Exp Exp | TsPreDefFuncScale Exp Exp
          | TsPreDefFuncReflectY Exp | TsPreDefFuncReflectX Exp | TsPreDefFuncHeight Exp | TsPreDefFuncWidth Exp | TsPreDefFuncSubtile Exp Exp Exp 
          | TsPreDefFuncJoinY Exp Exp | TsPreDefFuncJoinX Exp Exp | TsPreDefFuncRepeatX Exp Exp | TsPreDefFuncRepeatY Exp Exp | TsPreDefFuncToBase Exp
          | TsInt Int | TsTrue | TsFalse | TsChar Char | TsString String | TsVar String | TsPair Exp Exp | TsCell Int
          | TsTile [[Int]] | TsBase [[Int]] | TsArrayDef ArrayContents | TsUnit     
          deriving (Show, Eq)

data ArrayContents = ArrayContentsBase Exp | ArrayContentsRec Exp ArrayContents | ArrayContentsEmpty deriving (Show,Eq)

data Type = TyTile | TyCell | TyInt | TyString | TyChar | TyBool | TyUnit | TyBase | TyPair | TyArray Type deriving (Show,Eq)

type Environment = [ (Type, String, Exp) ]

}