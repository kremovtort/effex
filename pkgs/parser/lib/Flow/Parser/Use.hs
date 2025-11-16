{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Flow.Parser.Use where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "vector" Data.Vector qualified as Vector

import Data.Maybe (fromJust)
import Data.Vector.NonEmpty qualified as NonEmptyVector
import Flow.AST.Ann
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Use qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (
  Parser,
  pMethodIdentifier,
  pIdentifier,
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
      , Surface.UsClSupers . fromJust . NonEmptyVector.fromList . fmap (.span)
          <$> Megaparsec.sepEndBy1
            (single (Lexer.Keyword Lexer.Super))
            (single (Lexer.Punctuation Lexer.Comma))
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
