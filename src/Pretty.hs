{-# Language OverloadedStrings #-}
{-# Language RankNTypes, KindSignatures, DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}
{-# Language ConstraintKinds #-}
{-# Language UndecidableInstances #-}
module Pretty where

import Text.PrettyPrint as PP hiding (sep)
import Data.Text (Text)
import qualified Data.Text as Text

import AST


class PrettySyn (syn :: Syntax) where
  ppPrecSyn :: Int -> syn c -> Doc

class Pretty t where
  ppPrec :: Int -> t -> Doc

instance Pretty Text where
  ppPrec _ = text . Text.unpack

type Printable syn =
  ( PrettySyn syn
  , Pretty (Ident syn)
  , Pretty (Name syn)
  )

ppSyn :: Printable syn => syn c -> Doc
ppSyn = ppPrecSyn 0

pp :: Pretty t => t -> Doc
pp = ppPrec 0


instance Printable syn => PrettySyn (Syn syn) where
  ppPrecSyn n syn =
    case syn of

      EApp e1 e2 -> wrapAt 2
                  $ ppPrecSyn 1 e1 <+> ppPrecSyn 2 e2

      EInfix e1 op e2 -> wrapAt 1
                       $ ppPrecSyn 1 e1 <+> pp op <+> ppPrecSyn 1 e2

      -- exprAtom
      EVar x    -> pp x
      EDo ss e  -> "do" <+> ppBraceBlock (map ppSyn ss ++ [ ppSyn e ])
      EList es  -> ppBlock "[" "," "]" (map ppSyn es)
      ETuple es -> ppBlock "(" "," ")" (map ppSyn es)
      ECase e as -> "case" <+> ppSyn e <+> "of"
                    $$ nest 2 (ppBraceBlock (map ppSyn as))
      EListComp e as -> ppBlock "[" "|" "]"
                        (ppSyn e : map (ppSepBlock "," . map ppSyn) as)

      -- longExpr
      ELet ds e -> wrapAt 1
                 $ vcat [ "let"
                        ,  nest 2 (ppBraceBlock (map ppSyn ds))
                        ,  "in" <+> ppSyn e
                        ]
      EIf ms e -> wrapAt 1
                $ "if" <+> ppSepBlock "|" (map ppSyn ms)
                   $$ nest 2 ("else" <+> ppSyn e)
      EAbs ps e -> wrapAt 1
                $ "\\" PP.<> hsep (map (ppPrecSyn 2) ps) <+> "->" <+> ppSyn e

      -- pat
      PVar x    -> pp x
      PWild     -> "_"
      PTuple ps -> parens (commaSep (map ppSyn ps))
      PList ps  -> brackets (commaSep (map ppSyn ps))
      PCon c ps -> case ps of
                     [] -> pp c
                     _  -> wrapAt 2 (pp c <+> hsep (map (ppPrecSyn 2) ps))

      PInfix p1 op p2 -> wrapAt 1 (ppPrecSyn 1 p1 <+> pp op <+> ppPrecSyn 1 p2)


      -- stmt
      StmtBind p e  -> ppSyn p <+> "<-" <+> ppSyn e
      StmtNoBind e  -> ppSyn e
      StmtLet ds    -> "let" <+> ppBraceBlock (map ppSyn ds)

      -- match
      IfMatch gs e  -> commaSep (map ppSyn gs) <+> "then" <+> ppSyn e

      -- guard
      GuardBool e   -> ppSyn e
      GuardLet ds   -> "let" <+> ppBraceBlock (map ppSyn ds)
      GuardPat p e  -> ppSyn p <+> ppSyn e

      -- case alt
      CaseAlt p ms  -> ppSyn p <+> ppSepBlock "|" (map ppSyn ms)

      -- decl
      DDef x as e  -> pp x <+> hsep (map (ppPrecSyn 2) as) <+> "=" <+> ppSyn e
      DLet ls ds   -> "let"
                      $$ nest 2 (ppBraceBlock (map ppSyn ls))
                      $$ "in "
                      $$ nest 2 (ppBraceBlock (map ppSyn ds))

    where
    wrapAt i = if n >= i then parens else id

--------------------------------------------------------------------------------

commaSep :: [Doc] -> Doc
commaSep ds = fsep (punctuate comma ds)

ppBraceBlock :: [Doc] -> Doc
ppBraceBlock = ppBlock "{" ";" "}"

ppSepBlock :: Doc -> [Doc] -> Doc
ppSepBlock s = ppBlock start s empty
  where start = text (map (const ' ') (show s))

ppBlock :: Doc -> Doc -> Doc -> [Doc] -> Doc
ppBlock open sep close ds =
  case ds of
    [] -> open <+> close
    d : more -> vcat (first : rest) $$ close
      where first = open <+> d
            rest  = [ sep <+> doc | doc <- more ]


