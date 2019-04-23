{-# Language DataKinds #-}
module SynCat (SynCat, Expr, Pat, Match, Guard, Stmt, Alt, Decl) where

data {-kind-} SynCat   = Expr | Pat | Match | Guard | Stmt | Alt | Decl

type Expr   = 'Expr
type Pat    = 'Pat
type Match  = 'Match
type Guard  = 'Guard
type Stmt   = 'Stmt
type Alt    = 'Alt
type Decl   = 'Decl

