{
module Parser where

import AST
import Lexer
import ParserMonad

}

%tokentype { Lexeme Token }

%token

  '\\'              { Lexeme { lexemeRange = $$, lexemeToken = TokBackSlash } }
  '->'              { Lexeme { lexemeRange = $$, lexemeToken = TokArrowR } }
  '<-'              { Lexeme { lexemeRange = $$, lexemeToken = TokArrowL } }
  '|'               { Lexeme { lexemeRange = $$, lexemeToken = TokBar } }
  '('               { Lexeme { lexemeRange = $$, lexemeToken = TokParenL } }
  ')'               { Lexeme { lexemeRange = $$, lexemeToken = TokParenR } }
  '{'               { Lexeme { lexemeRange = $$, lexemeToken = TokBraceL } }
  ';'               { Lexeme { lexemeRange = $$, lexemeToken = TokSemi   } }
  '}'               { Lexeme { lexemeRange = $$, lexemeToken = TokBraceR } }
  '['               { Lexeme { lexemeRange = $$, lexemeToken = TokBracketL } }
  ']'               { Lexeme { lexemeRange = $$, lexemeToken = TokBracketR } }
  ','               { Lexeme { lexemeRange = $$, lexemeToken = TokComma } }
  '='               { Lexeme { lexemeRange = $$, lexemeToken = TokEq } }
  'do'              { Lexeme { lexemeRange = $$, lexemeToken = TokKwDo } }
  'if'              { Lexeme { lexemeRange = $$, lexemeToken = TokKwIf } }
  'case'            { Lexeme { lexemeRange = $$, lexemeToken = TokKwCase } }
  'then'            { Lexeme { lexemeRange = $$, lexemeToken = TokKwThen } }
  'else'            { Lexeme { lexemeRange = $$, lexemeToken = TokKwElse } }

  IDENT             { $$@Lexeme { lexemeToken = TokIdent } }
  OP                { $$@Lexeme { lexemeToken = TokOp } }

  -- Literals

%name decl decl
%name expr expr

%lexer { happyGetToken } { Lexeme { lexemeToken = TokEOF } }
%monad { Parser }

%%

decl :: { Decl InParser }
  : exprInfixLeft exprSimpleApp '=' expr {% mkDecl ($1 $2) $4 }

op :: { Ident InParser }
  : OP                                { Ident (lexemeText $1) }

ident :: { Ident InParser }
  : IDENT                             { Ident (lexemeText $1) }

expr :: { Expr InParser }
  : exprInfixLeft exprApp             { $1 $2 }

exprInfixLeft :: { Expr InParser -> Expr InParser }
  : exprInfixLeft exprSimpleApp op    { EInfix ($1 $2) $3 }
  |                                   { id }

exprApp :: { Expr InParser }
  : exprSimpleApp longExpr            { EApp $1 $2 }
  | exprSimpleApp                     { $1 }
  | longExpr                          { $1 }

exprSimpleApp :: { Expr InParser }
  : exprAtom                          { $1 }
  | exprSimpleApp exprAtom            { EApp $1 $2 }

longExpr :: { Expr InParser }
  : '\\' patAtoms '->' expr           { EAbs $2 $4 }
  | 'if' matches 'else' expr          { EIf $2 $4 }

exprAtom :: { Expr InParser }
  : '(' exprsComma ')'                { case $2 of
                                          [e] -> e
                                          x -> ETuple x }
  | '[' exprsComma ']'                { EList $2 }
  | '[' expr lcBranches ']'           { EListComp $2 $3 }
  | ident                             { EVar $1 }
  | 'case' expr '{' alts '}'          { ECase $2 $4 }
  | 'do' '{' guards ';' expr '}'      { EDo $3 $5 }
  | 'do' '{' expr '}'                 { EDo [] $3 }

exprsComma :: { [Expr InParser ] }
  :                                   { [] }
  | exprsComma1                       { $1 }

exprsComma1 :: { [Expr InParser ] }
  : expr                              { [$1] }
  | exprsComma1 ',' expr              { $1 ++ [$3] }

lcBranch :: { [Guard InParser] }
  : '|' guards                        { $2 }

lcBranches :: { [[Guard InParser]] }
lcBranches
  : lcBranch                          { [ $1 ] }
  | lcBranches lcBranch               { $1 ++ [$2] }

match :: { Match InParser }
  : guards 'then' expr                { Match $1 $3 }

matches :: { [Match InParser] }
  : match                             { [$1] }
  | matches '|' match                 { $1 ++ [$3] }

pat :: { Pat InParser }
  : exprInfixLeft exprSimpleApp       {% exprToPat ($1 $2) }

patAtom :: { Pat InParser }
  : exprAtom                          {% exprToPat $1 }

patAtoms :: { [Pat InParser] }
  : patAtom                           { [$1] }
  | patAtoms patAtom                  { $1 ++ [$2] }

alts :: { [Alt InParser] }
  : alt                               { [$1] }
  | alts ';' alt                      { $1 ++ [$3] }

alt :: { Alt InParser }
  : pat 'then' expr                   { Alt $1 [ Match [] $3 ] }
  | pat '|' matches                   { Alt $1 $3 }

guards :: { [Guard InParser] }
  : guard                             { [$1] }
  | guards ',' guard                  { $1 ++ [$3] }

guard :: { Guard InParser }
  : expr                              { GuardBool $1 }
  | pat '<-' expr                     { GuardPat  $1 $3 }


{


}


