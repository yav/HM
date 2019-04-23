{-# Language OverloadedStrings #-}
{-# Language RankNTypes, KindSignatures, DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}
{-# Language ConstraintKinds #-}
{-# Language UndecidableInstances #-}
module Pretty where

import AST

import Text.PrettyPrint hiding (sep)

class PrettySyn (syn :: Syntax) where
  ppPrecSyn :: Int -> syn c -> Doc

class Pretty t where
  ppPrec :: Int -> t -> Doc

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
      EVar x    -> pp x
      EDo ss e  -> "do" <+> ppBraceBlock (map ppSyn ss ++ [ ppSyn e ])
      EList es  -> ppBlock "[" "," "]" (map ppSyn es)
      ETuple es -> ppBlock "(" "," ")" (map ppSyn es)

      ELet ds e -> vcat [ "let" <+> ppBraceBlock (map ppSyn ds)
                        , "in" <+> ppSyn e
                        ]


--------------------------------------------------------------------------------

ppBraceBlock :: [Doc] -> Doc
ppBraceBlock = ppBlock "{" ";" "}"

ppBlock :: Doc -> Doc -> Doc -> [Doc] -> Doc
ppBlock open sep close ds =
  case ds of
    [] -> open <+> close
    d : more -> vcat (first : rest) $$ close
      where first = open <+> d
            rest  = [ sep <+> doc | doc <- more ]


