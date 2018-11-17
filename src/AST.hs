module AST where

data Op     = OpXXX
data Ident  = IdentXXX

data Expr   = EInfix Expr Op Expr
            | EApp Expr Expr
            | EAbs [Pat] Expr
            | EIf [Guard] Expr
            | ECase Expr [Alt]
            | EDo [Match]
            | EVar Ident
            | ETuple [Expr]
            | EList [Expr]
            | EListComp Expr [[Match]]

data Pat    = PVar Ident
            | PWild
            | PTuple [Pat]
            | PList [Pat]
            | PCon Ident [Pat]
            | PInfix Pat Op Pat

data Guard  = Guard [Match] Expr

data Match  = MExpr Expr
            | MMatch Pat Expr

data Alt    = Alt Pat [Guard]

