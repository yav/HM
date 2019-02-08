module AST where

import Data.Text(Text)

data Ident i  = Ident Text
              deriving Show

data Expr i   = EInfix (Expr i) (Ident i) (Expr i)
              | EApp (Expr i) (Expr i)
              | EAbs [Pat i] (Expr i)
              | EIf [Match i] (Expr i)
              | ECase (Expr i) [(Alt i)]
              | EDo [Guard i] (Expr i)
              | EVar (Ident i)
              | ETuple [Expr i]
              | EList [Expr i]
              | EListComp (Expr i) [[Guard i]]
              | ELet (Decl i) (Expr i)
                deriving Show

data Pat i    = PVar (Ident i)
              | PWild
              | PTuple [Pat i]
              | PList [Pat i]
              | PCon (Ident i) [Pat i]
              | PInfix (Pat i) (Ident i) (Pat i)
                deriving Show

data Match i  = Match [Guard i] (Expr i)
                -- ^ If all guards succeed, then give expr
                deriving Show

data Guard i  = GuardBool (Expr i)          -- ^ Boolean, True <- Expr
              | GuardLet  (Decl i)
              | GuardPat  (Pat i) (Expr i)  -- ^ Pattern Guard
                deriving Show

data Alt i    = Alt (Pat i) [Match i]
                deriving Show

data Decl i   = Fun (Ident i) [Pat i]{-1+-} (Expr i)
              | PBind (Pat i) (Expr i)
              | DAnd (Decl i) (Decl i)
              | DLet (Decl i) (Decl i)
                deriving Show
