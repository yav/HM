{-# Language OverloadedStrings #-}
module Parser.Monad
  ( Parser
  , parseStartingAt
  , parse
  , happyGetToken
  , happyError
  , happyErrorAt
  , ParseError(..)

  , exprToPat

  -- Utilities
  , List
  , single
  , snoc
  , toList
  , DExpr(..)
  , mkInfix
  , mkGuard
  , mkStmt
  , mkLet
  , dexprToExpr
  ) where

import Control.Monad(liftM,ap)
import Control.Exception (Exception)
import Data.Text(Text)
import AlexTools(prevPos, startPos)

import Parser.Lexer
import Parser.AST
import HMPanic


newtype Parser a = Parser ([Lexeme Token] ->
                            Either ParseError (a, [Lexeme Token]))

{-| Run the given parser on the input text. We always try to parse the
whole text, starting at the input, and report an error if there was
left-overs at the end.

The given source position should correspond to the first character in
the text. -}
parseStartingAt ::
  Parser a  {- ^ Describes how to parse the input -} ->
  SourcePos {- ^ Location for the first character in the text -} ->
  Text      {- ^ Parse this text -} ->
  Either ParseError a
parseStartingAt (Parser m) p txt =
  case m (lexer input) of
    Left err -> Left err
    Right (a,ls) ->
      case ls of
        []    -> Right a
        l : _ -> Left $ ParseError $ Just $ sourceFrom $ lexemeRange l
  where
  input = Input { inputPos       = p
                , inputText      = txt
                , inputPrev      = pPos
                , inputPrevChar  =
                    if sourceLine pPos == sourceLine p then ' ' else '\n'
                }
  pPos  = prevPos p

parse :: Parser a {- ^ Describes how to parse -} ->
         Text     {- ^ Name for the input text (e.g., file name) -} ->
         Text     {- ^ The text to parse -} ->
         Either ParseError a
parse p inp = p `parseStartingAt` startPos inp




instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = Parser (\ls -> Right (a,ls))
  (<*>)  = ap

instance Monad Parser where
  Parser m >>= k = Parser (\ls -> case m ls of
                                    Left err -> Left err
                                    Right (a,ls1) ->
                                      let Parser m1 = k a
                                      in m1 ls1)

happyGetToken :: (Lexeme Token -> Parser a) -> Parser a
happyGetToken k = Parser $ \ls ->
  case ls of
    []     -> panic "happyGetToken" ["We run out of tokens.", "Missing TokEOF?"]
    t : ts -> let Parser m = k t
              in m ts

newtype ParseError = ParseError (Maybe SourcePos) -- ^ Nothing means EOF
                      deriving Show

instance Exception ParseError

happyErrorAtR :: HasRange r => r -> Parser a
happyErrorAtR x = happyErrorAt (sourceFrom (range x))

happyErrorAt :: SourcePos -> Parser a
happyErrorAt p = Parser (\_ -> Left (ParseError (Just p)))

happyError :: Parser a
happyError = Parser $ \ls ->
  Left $ ParseError
       $ case ls of
           []    -> Nothing
           t : _ -> Just $ sourceFrom $ lexemeRange t


exprToPat :: Expr -> Parser Pat
exprToPat expr =
  case psynNode expr of
    EInfix e1 op e2 ->
      mk <$> (PInfix <$> exprToPat e1 <*> pure op <*> exprToPat e2)

    EList es ->
      mk <$> (PList <$> mapM exprToPat es)

    ETuple es ->
      mk <$> (PTuple <$> mapM exprToPat es)

    EVar x ->
      case x of
        Qual {} -> bad
        Unqual i
          | identText i == "_"    -> pure (mk PWild)
          | otherwise             -> pure (mk (PVar i)) -- or Con

    EApp e1 e2 -> pApp e1 [e2]

    EListComp {}    -> bad
    EAbs {}         -> bad
    EIf {}          -> bad
    ECase {}        -> bad
    EDo {}          -> bad
    ELet {}         -> bad
  where
  bad  = happyErrorAt (sourceFrom (psynRange expr))
  mk x = ParserSyn { psynRange = psynRange expr, psynNode = x }

  pApp :: Expr -> [Expr] -> Parser Pat
  pApp f es =
    case psynNode f of
      EApp e1 e2 -> pApp e1 (e2 : es)
      EVar x     -> do ps <- mapM exprToPat es    -- underscore?
                       pure (mk (PCon x ps))
      _          -> bad


--------------------------------------------------------------------------------

type List a = [a] -> [a]

single :: a -> List a
single a = (a :)

snoc :: List a -> a -> List a
snoc xs a = xs . (a :)

toList :: List a -> [a]
toList xs = xs []


--------------------------------------------------------------------------------
data DExpr = Decls (SourceRange,[Decl]) | Expr Expr

mkLet :: DExpr -> Expr -> Parser Expr
mkLet de e =
  case de of
    Decls (r,ds) -> pure $ psynAt r e (ELet ds e)
    Expr e1      -> happyErrorAtR e1

dexprToExpr :: DExpr -> Parser Expr
dexprToExpr de =
  case de of
    Decls (r,_) -> happyErrorAtR r
    Expr e      -> pure e

mkInfix :: Parser DExpr -> Name -> DExpr -> Parser DExpr
mkInfix ml op rd =
  do l <- dexprToExpr =<< ml
     r <- dexprToExpr rd
     pure (Expr (psynAt l r (EInfix l op r)))

mkStmt :: DExpr -> Stmt
mkStmt de =
  case de of
    Decls (r,ds) -> psynAt r r (StmtLet ds)
    Expr e       -> psynAt e e (StmtNoBind e)

mkGuard :: DExpr -> Guard
mkGuard de =
  case de of
    Decls (r,ds) -> psynAt r r (GuardLet ds)
    Expr e       -> psynAt e e (GuardBool e)
