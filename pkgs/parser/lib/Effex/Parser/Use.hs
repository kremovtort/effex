{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Effex.Parser.Use where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Effex.AST.Ann
import Effex.AST.Surface.Common qualified as Surface
import Effex.AST.Surface.Use qualified as Surface
import Effex.Lexer qualified as Lexer
import Effex.Parser.Common (
  Parser,
  pIdentifier,
  pMethodIdentifier,
  sepBy1,
  single,
 )

pUseClause :: Parser (Surface.UseClause Lexer.SourceSpan)
pUseClause = do
  useTok <- single (Lexer.Keyword Lexer.Use)
  root <- pUseRoot
  _ <- single (Lexer.Punctuation Lexer.ColonColon)
  tree <- pUseTree
  tokE <- single (Lexer.Punctuation Lexer.Semicolon)
  pure
    Surface.UseClause
      { root
      , tree
      , ann = Lexer.SourceSpan{start = useTok.span.start, end = tokE.span.end}
      }
 where
  pUseRoot = do
    Megaparsec.choice
      [ Surface.UsClSelf . (.span) <$> single (Lexer.Keyword Lexer.Self)
      , Surface.UsClSupers . NonEmptyVector.fromNonEmpty . fmap (.span)
          <$> sepBy1
            (single (Lexer.Keyword Lexer.Super))
            (single (Lexer.Punctuation Lexer.ColonColon))
      , Surface.UsClPackage <$> pIdentifier
      ]

  pUseTree =
    Megaparsec.choice
      [ Megaparsec.try pUseTreeNested
      , Megaparsec.try pUseTreeBranch
      , Megaparsec.try pUseTreeLeafMethodAsFn
      , Megaparsec.try pUseTreeLeafMethod
      , Megaparsec.try pUseTreeLeafIdent
      , Megaparsec.try pUseTreeLeafWildcard
      ]

  pUseTreeBranch = do
    ident <- pIdentifier
    _ <- single (Lexer.Punctuation Lexer.ColonColon)
    tree <- pUseTree
    pure $ Surface.UseTrBranch ident tree

  pUseTreeNested = do
    _ <- single (Lexer.Punctuation Lexer.LeftBrace)
    trees <- Megaparsec.sepEndBy pUseTree (single (Lexer.Punctuation Lexer.Comma))
    _ <- single (Lexer.Punctuation Lexer.RightBrace)
    pure $ Surface.UseTrNested $ Vector.fromList trees

  pUseTreeLeaf pIdent = do
    use <- pIdent
    as <- Megaparsec.optional do
      _ <- single (Lexer.Keyword Lexer.As)
      pIdent
    pure
      Surface.UseTreeLeaf
        { use = use
        , as = as
        , ann = case as of
            Nothing -> use.ann
            Just as' -> Lexer.SourceSpan use.ann.start as'.ann.end
        }

  pUseTreeLeafMethod = do
    leaf <- pUseTreeLeaf pMethodIdentifier
    pure $ Surface.UseTrLeafMethod leaf

  pUseTreeLeafIdent = do
    leaf <- pUseTreeLeaf pIdentifier
    pure $ Surface.UseTrLeafIdent leaf

  pUseTreeLeafMethodAsFn = do
    use <- pMethodIdentifier
    _ <- single (Lexer.Keyword Lexer.As)
    as <- pIdentifier
    pure $
      Surface.UseTrLeafMethodAsFn
        Surface.UseTreeLeafMethodAsFn
          { use
          , as
          , ann = Lexer.SourceSpan use.ann.start as.ann.end
          }

  pUseTreeLeafWildcard = do
    tok <- single (Lexer.Punctuation Lexer.Star)
    pure $ Surface.UseTrLeafWildcard tok.span
