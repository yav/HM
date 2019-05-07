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
  '`'               { Lexeme { lexemeRange = $$, lexemeToken = TokBackTick } }
  '='               { Lexeme { lexemeRange = $$, lexemeToken = TokEq } }
  'do'              { Lexeme { lexemeRange = $$, lexemeToken = TokKwDo } }
  'if'              { Lexeme { lexemeRange = $$, lexemeToken = TokKwIf } }
  'case'            { Lexeme { lexemeRange = $$, lexemeToken = TokKwCase } }
  'then'            { Lexeme { lexemeRange = $$, lexemeToken = TokKwThen } }
  'else'            { Lexeme { lexemeRange = $$, lexemeToken = TokKwElse } }
  'let'             { Lexeme { lexemeRange = $$, lexemeToken = TokKwLet } }
  'in'              { Lexeme { lexemeRange = $$, lexemeToken = TokKwIn } }

  IDENT             { $$@Lexeme { lexemeToken = TokIdent } }
  OP                { $$@Lexeme { lexemeToken = TokOp } }

  -- Literals

%name decl decl
%name decls decls
%name expr expr

%lexer { happyGetToken } { Lexeme { lexemeToken = TokEOF } }
%monad { Parser }

%%

decl                               :: { Decl }
  : ident '=' expr                    { psynAt $1 $3 (DDef $1 [] $3) }
  | ident patAtoms '=' expr           { psynAt $1 $4 (DDef $1 $2 $4) }
  | 'let' decls 'in' decls            { psynAt $1 (fst $4)
                                                  (DLet (snd $2) (snd $4)) }

decls                              :: { (SourceRange, [Decl]) }
  : '{' SepBy1(';', decl) '}'         { ($1 <-> $3, $2) }
  | '{' '}'                           { ($1 <-> $2, []) }


op :: { Name }
  : OP                                { mkUnqual $1 }
  | '`' IDENT '`'                     { mkUnqual $2 }


ident                              :: { Ident }
  : IDENT                             { mkIdent $1 }
  | '(' OP ')'                        { mkIdent $2 }

name                               :: { Name }
  : ident                             { Unqual $1 }

expr                               :: { Expr }
  : dexpr                             {% dexprToExpr $1 }

exprAtom                           :: { Expr }
  : dexprAtom                         {% dexprToExpr $1 }

exprSimpleApp                      :: { Expr }
  : dexprSimpleApp                    {% dexprToExpr $1 }

dexpr                              :: { DExpr }
  : dexprInfixLeft dexprApp           {% $1 $2 }

dexprInfixLeft                     :: { DExpr -> Parser DExpr }
  : dexprInfixLeft dexprSimpleApp op  { mkInfix ($1 $2) $3 }
  |                                   { pure }

dexprApp                           :: { DExpr }
  : exprSimpleApp longExpr            { Expr (psynAt $1 $2 (EApp $1 $2)) }
  | dexprSimpleApp                    { $1 }
  | longExpr                          { Expr $1 }

dexprSimpleApp                     :: { DExpr }
  : dexprAtom                         { $1 }
  | exprSimpleApp exprAtom            { Expr (psynAt $1 $2 (EApp $1 $2)) }

longExpr                           :: { Expr }
  : '\\' patAtoms '->' expr           { psynAt $1 $4 (EAbs $2 $4) }
  | 'if' matches 'else' expr          { psynAt $1 $4 (EIf $2 $4) }
  | dexprAtom 'in' expr               {% mkLet $1 $3 }

dexprAtom                          :: { DExpr }
  : '(' exprsComma ')'                { Expr (case $2 of
                                               [e] -> e
                                               x   -> psynAt $1 $3 (ETuple x)) }
  | '[' exprsComma ']'                { Expr (psynAt $1 $3 (EList $2)) }
  | '[' expr lcBranches ']'           { Expr (psynAt $1 $3 (EListComp $2 $3)) }
  | name                              { Expr (psynAt $1 $1 (EVar $1)) }
  | 'case' expr '{' alts '}'          { Expr (psynAt $1 $5 (ECase $2 $4)) }
  | 'do' '{' stmts ';' expr '}'       { Expr (psynAt $1 $6 (EDo $3 $5)) }
  | 'do' '{' expr '}'                 { Expr (psynAt $1 $4 (EDo [] $3)) }
  | 'let' decls                       { Decls $2 }

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
  : dexprInfixLeft dexprSimpleApp     {% exprToPat =<< dexprToExpr =<< $1 $2 }

patAtoms                           :: { [Pat] }
  : List1(patAtom)                    { $1 }

patAtom                            :: { Pat }
  : dexprAtom                         {% exprToPat =<< dexprToExpr $1 }

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
  : dexpr                             { mkGuard $1 }
  | pat '<-' expr                     { psynAt $1 $3 (GuardPat  $1 $3) }

stmts                              :: { [Stmt] }
  : SepBy1(',',stmt)                  { $1 }

stmt                               :: { Stmt }
  : pat '<-' expr                     { psynAt $1 $3 (StmtBind $1 $3) }
  | dexpr                             { mkStmt $1 }


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


