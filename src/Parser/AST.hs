{-# Language TypeFamilies #-}
module Parser.AST
  ( ParserSyn(..)
  , Expr
  , Decl
  , Pat
  , Alt
  , Guard
  , Match
  , Stmt
  , AST.Syn(..)
  , Ident(..)
  , mkIdent
  , Name(..)
  , mkUnqual
  , psynAt
  , (<->)
  , HasRange(..)
  ) where

import Data.Text(Text)

import AlexTools(SourceRange(..),Lexeme(..))
import qualified SynCat as S
import qualified AST
import HMPanic(panic)


data ParserSyn c = ParserSyn { psynRange :: SourceRange
                             , psynNode  :: AST.Syn ParserSyn c
                             }

type Expr   = ParserSyn S.Expr
type Decl   = ParserSyn S.Decl
type Pat    = ParserSyn S.Pat
type Alt    = ParserSyn S.Alt
type Guard  = ParserSyn S.Guard
type Match  = ParserSyn S.Match
type Stmt   = ParserSyn S.Stmt

type instance AST.Ident ParserSyn = Ident
type instance AST.Name  ParserSyn = Name

data Ident = Ident { identText :: Text, identRange :: SourceRange }

data Name = Unqual Ident
          | Qual Ident Ident

class HasRange t where
  range :: t -> SourceRange

instance HasRange SourceRange where
  range = id

instance HasRange (ParserSyn c) where
  range = psynRange

instance HasRange (Lexeme t) where
  range = lexemeRange

instance HasRange Ident where
  range = identRange

instance HasRange Name where
  range nm =
    case nm of
      Unqual i -> range i
      Qual m i -> m <-> i

instance HasRange a => HasRange [a] where
  range xs = case xs of
               []     -> panic "range" ["Range of []"]
               [x]    -> range x
               x : ys -> x <-> last ys

(<->) :: (HasRange a, HasRange b) => a -> b -> SourceRange
x <-> y = SourceRange { sourceFrom = sourceFrom (range x)
                      , sourceTo   = sourceTo (range y)
                      }

psynAt ::
  (HasRange a, HasRange b) => a -> b -> AST.Syn ParserSyn c -> ParserSyn c
psynAt a b e = ParserSyn { psynRange = a <-> b, psynNode = e }

mkIdent :: Lexeme t -> Ident
mkIdent l = Ident { identText = lexemeText l, identRange = lexemeRange l }

mkUnqual :: Lexeme t -> Name
mkUnqual l = Unqual (mkIdent l)


