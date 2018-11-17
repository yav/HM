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
  'do'              { Lexeme { lexemeRange = $$, lexemeToken = TokKwDo } }
  'if'              { Lexeme { lexemeRange = $$, lexemeToken = TokKwIf } }
  'case'            { Lexeme { lexemeRange = $$, lexemeToken = TokKwCase } }
  'then'            { Lexeme { lexemeRange = $$, lexemeToken = TokKwThen } }
  'else'            { Lexeme { lexemeRange = $$, lexemeToken = TokKwElse } }

  IDENT             { $$@Lexeme { lexemeToken = TokIdent } }
  OP                { $$@Lexeme { lexemeToken = TokOp } }

  -- Literals

%name expr expr

%lexer { happyGetToken } { Lexeme { lexemeToken = TokEOF } }
%monad { Parser }

%%

op :: { Op }
  : OP                                { OpXXX }

ident :: { Ident }
  : IDENT                             { IdentXXX }

expr :: { Expr }
  : exprInfixLeft exprApp             { $1 $2 }

exprInfixLeft :: { Expr -> Expr }
  : exprInfixLeft exprSimpleApp op    { EInfix ($1 $2) $3 }
  |                                   { id }

exprApp :: { Expr }
  : exprSimpleApp longExpr            { EApp $1 $2 }
  | exprSimpleApp                     { $1 }
  | longExpr                          { $1 }

exprSimpleApp :: { Expr }
  : exprAtom                          { $1 }
  | exprSimpleApp exprAtom            { EApp $1 $2 }

longExpr :: { Expr }
  : '\\' patAtoms '->' expr           { EAbs $2 $4 }
  | 'if' guards 'else' expr           { EIf $2 $4 }

exprAtom :: { Expr }
  : '(' exprsComma ')'                { case $2 of
                                          [e] -> e
                                          x -> ETuple x }
  | '[' exprsComma ']'                { EList $2 }
  | '[' expr lcBranches ']'           { EListComp $2 $3 }
  | ident                             { EVar $1 }
  | 'case' expr '{' alts '}'          { ECase $2 $4 }

exprsComma :: { [Expr] }
  :                                   { [] }
  | exprsComma1                       { $1 }

exprsComma1 :: { [Expr] }
  : expr                              { [$1] }
  | exprsComma1 ',' expr              { $1 ++ [$3] }

lcBranch :: { [Match] }
  : '|' matches                       { $2 }

lcBranches :: { [[Match]] }
lcBranches
  : lcBranch                          { [ $1 ] }
  | lcBranches lcBranch               { $1 ++ [$2] }

guard :: { Guard }
  : matches 'then' expr               { Guard $1 $3 }

guards :: { [Guard] }
  : guard                             { [$1] }
  | guards '|' guard                  { $1 ++ [$3] }

pat :: { Pat }
  : exprInfixLeft exprSimpleApp       {% exprToPat ($1 $2) }

patAtom :: { Pat }
  : exprAtom                          {% exprToPat $1 }

patAtoms :: { [Pat] }
  : patAtom                           { [$1] }
  | patAtoms patAtom                  { $1 ++ [$2] }

alts :: { [Alt] }
  : alt                               { [$1] }
  | alts ';' alt                      { $1 ++ [$3] }

alt :: { Alt }
  : pat 'then' expr                   { Alt $1 [ Guard [] $3 ] }
  | pat '|' guards                    { Alt $1 $3 }

matches :: { [Match] }
  : match                             { [$1] }
  | matches ',' match                 { $1 ++ [$3] }

match :: { Match }
  : expr                              { MExpr $1 }
  | pat '<-' expr                     { MMatch $1 $3 }


{


}


