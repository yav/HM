{-# Language TypeFamilies, ExistentialQuantification, DataKinds #-}
module AST where

import Data.Kind(Type)
import SynCat


-- | A family of types implementing all syntactic categories.
type Syntax = SynCat -> Type

-- | 'Ident' is used to name entities.
type family Ident (syn :: Syntax)

-- | 'Name' is used to refer to entities.
type family Name  (syn :: Syntax)

data Syn syn c =
    (c ~ Expr)  => EInfix (syn Expr) (Name syn) (syn Expr)
  | (c ~ Expr)  => EApp (syn Expr) (syn Expr)
  | (c ~ Expr)  => EAbs [syn Pat]  (syn Expr)
  | (c ~ Expr)  => EIf [syn Match] (syn Expr)
  | (c ~ Expr)  => ECase (syn Expr)  [syn Alt]
  | (c ~ Expr)  => EDo [syn Stmt] (syn Expr)
  | (c ~ Expr)  => EVar (Name syn)
  | (c ~ Expr)  => ETuple [syn Expr]
  | (c ~ Expr)  => EList [syn Expr]
  | (c ~ Expr)  => EListComp (syn Expr) [[syn Guard]]
  | (c ~ Expr)  => ELet [syn Decl] (syn Expr)

  | (c ~ Pat)   => PVar (Ident syn)
  | (c ~ Pat)   => PWild
  | (c ~ Pat)   => PTuple [syn Pat]
  | (c ~ Pat)   => PList [syn Pat]
  | (c ~ Pat)   => PCon (Name syn) [syn Pat]
  | (c ~ Pat)   => PInfix (syn Pat) (Name syn) (syn Pat)

  | (c ~ Match) => IfMatch [syn Guard] (syn Expr)

  | (c ~ Guard) => GuardBool (syn Expr)
  | (c ~ Guard) => GuardLet (syn Decl)
  | (c ~ Guard) => GuardPat (syn Pat)  (syn Expr)

  | (c ~ Stmt)  => StmtBind (syn Pat) (syn Expr)
  | (c ~ Stmt)  => StmtNoBind (syn Expr)
  | (c ~ Stmt)  => StmtLet (syn Decl)

  | (c ~ Alt)   => CaseAlt (syn Pat) [syn Match]

  | (c ~ Decl)  => DDef (Ident syn) [syn Pat] (syn Expr)
  | (c ~ Decl)  => DLet [syn Decl] [syn Decl]




