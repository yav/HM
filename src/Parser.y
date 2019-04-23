{
module Parser where

import Parser.Lexer
import Parser.Monad
import Parser.AST

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

decl                               :: { Decl }
  : exprInfixLeft exprSimpleApp
      '=' expr                        {% mkDecl ($1 $2) $4 }

op :: { Name }
  : OP                                { mkUnqual $1 }

name                               :: { Name }
  : IDENT                             { mkUnqual $1 }

expr                               :: { Expr }
  : exprInfixLeft exprApp             { $1 $2 }

exprInfixLeft                      :: { Expr -> Expr }
  : exprInfixLeft exprSimpleApp op    { let left = $1 $2
                                        in psynAt left $3 . EInfix left $3 }
  |                                   { id }

exprApp                            :: { Expr }
  : exprSimpleApp longExpr            { psynAt $1 $2 (EApp $1 $2) }
  | exprSimpleApp                     { $1 }
  | longExpr                          { $1 }

exprSimpleApp                      :: { Expr }
  : exprAtom                          { $1 }
  | exprSimpleApp exprAtom            { psynAt $1 $2 (EApp $1 $2) }

longExpr                           :: { Expr }
  : '\\' patAtoms '->' expr           { psynAt $1 $4 (EAbs $2 $4) }
  | 'if' matches 'else' expr          { psynAt $1 $4 (EIf $2 $4) }

exprAtom                           :: { Expr }
  : '(' exprsComma ')'                { case $2 of
                                          [e] -> e
                                          x   -> psynAt $1 $3 (ETuple x) }
  | '[' exprsComma ']'                { psynAt $1 $3 (EList $2) }
  | '[' expr lcBranches ']'           { psynAt $1 $3 (EListComp $2 $3) }
  | name                              { psynAt $1 $1 (EVar $1) }
  | 'case' expr '{' alts '}'          { psynAt $1 $5 (ECase $2 $4) }
  | 'do' '{' stmts ';' expr '}'       { psynAt $1 $6 (EDo $3 $5) }
  | 'do' '{' expr '}'                 { psynAt $1 $4 (EDo [] $3) }

exprsComma                         :: { [Expr] }
  :                                   { [] }
  | exprsComma1                       { $1 }

exprsComma1 :: { [Expr] }
  : SepBy1(',',expr)                  { $1 }

lcBranches                         :: { [[Guard]] }
  : List1(lcBranch)                   { $1 }

lcBranch                           :: { [Guard] }
  : '|' guards                        { $2 }

matches                            :: { [Match] }
  : SepBy1('|',match)                 { $1 }

match                              :: { Match }
  : guards 'then' expr                { psynAt $1 $3 (IfMatch $1 $3) }

pat                                :: { Pat }
  : exprInfixLeft exprSimpleApp       {% exprToPat ($1 $2) }

patAtoms                           :: { [Pat] }
  : List1(patAtom)                    { $1 }

patAtom                            :: { Pat }
  : exprAtom                          {% exprToPat $1 }

alts                               :: { [Alt] }
  : SepBy1(';',alt)                   { $1 }

alt                                :: { Alt }
  : pat 'then' expr                   { psynAt $1 $3
                                            (CaseAlt $1 [ psynAt $3 $3
                                                            (IfMatch [] $3) ]) }
  | pat '|' matches                   { psynAt $1 $3 (CaseAlt $1 $3) }

guards                             :: { [Guard] }
  : SepBy1(',', guard)                { $1 }

guard                              :: { Guard }
  : expr                              { psynAt $1 $1 (GuardBool $1) }
  | pat '<-' expr                     { psynAt $1 $3 (GuardPat  $1 $3) }

stmts                              :: { [Stmt] }
  : SepBy1(',',stmt)                  { $1 }

stmt                               :: { Stmt }
  : pat '<-' expr                     { psynAt $1 $3 (StmtBind $1 $3) }
  | expr                              { psynAt $1 $1 (StmtNoBind $1) }


--------------------------------------------------------------------------------


List1(thing)                       :: { [thing] }
  : List1_(thing)                     { toList $1 }

List1_(thing)                      :: { List thing }
  : thing                             { single $1  }
  | List1_(thing) thing               { snoc $1 $2 }

SepBy1(sep,thing)                  :: { [thing] }
  : SepBy1_(sep,thing)                { toList $1  }

SepBy1_(sep,thing)                 :: { List thing }
  : thing                             { single $1  }
  | SepBy1_(sep,thing) sep thing      { snoc $1 $3 }

{


}


