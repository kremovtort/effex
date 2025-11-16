{-# OPTIONS_GHC -Wno-orphans #-}

module Flow.Parser.Common (
  type HasAnn,
  type Parser,
  Lexer.SourceSpan (..),
  Lexer.WithPos (..),
  type Lexer.TokenWithPos,
  Lexer.TokenStream (..),
  single,
  token,
  many1,
  sepEndBy1,
  pIdentifier,
  pNonQualifiedIdentifier,
  pRegionIdentifier,
  pMethodIdentifier,
  pPub,
) where

import "base" Data.List.NonEmpty (NonEmpty ((:|)))
import "base" Data.List.NonEmpty qualified as List (NonEmpty)
import "base" Data.List.NonEmpty qualified as List.NonEmpty
import "base" Data.String (IsString (..))
import "base" Data.Void (Void)
import "base" GHC.Records (HasField)
import "containers" Data.Set (Set)
import "containers" Data.Set qualified as Set
import "megaparsec" Text.Megaparsec (Parsec)
import "megaparsec" Text.Megaparsec qualified as Megaparsec

import Flow.AST.Surface.Common (
  Identifier (..),
  RegionIdentifier (..),
 )
import Flow.AST.Surface.Common qualified as Surface
import Flow.Lexer qualified as Lexer

type HasAnn f ann = HasField "ann" (f ann) ann

type Parser = Parsec Void Lexer.TokenStream

instance IsString (List.NonEmpty Char) where
  fromString = List.NonEmpty.fromList

single :: Lexer.Token -> Parser (Lexer.WithPos Lexer.Token)
single t = Megaparsec.satisfy ((== t) . (.value))

token ::
  Set (Megaparsec.ErrorItem (Lexer.WithPos Lexer.Token)) ->
  (Lexer.Token -> Maybe a) ->
  Parser (Lexer.WithPos a)
token expected match =
  Megaparsec.token
    ( \tw@Lexer.WithPos{value = t} -> do
        r <- match t
        pure $ tw{Lexer.value = r}
    )
    expected

many1 :: Parser a -> Parser (List.NonEmpty a)
many1 p = do
  first <- p
  rest <- Megaparsec.many p
  pure $ first :| rest

sepEndBy1 :: Parser a -> Parser sep -> Parser (List.NonEmpty a)
sepEndBy1 p sep = List.NonEmpty.fromList <$> Megaparsec.sepEndBy1 p sep

pIdentifier :: Parser (Identifier Lexer.SourceSpan)
pIdentifier = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "identifier")
      \case
        Lexer.Identifier i -> Just i
        _ -> Nothing
  pure $ Identifier{name = tok.value, ann = tok.span}

pNonQualifiedIdentifier :: Parser (Identifier Lexer.SourceSpan)
pNonQualifiedIdentifier = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "non qualified identifier")
      \case
        Lexer.Identifier i -> Just i
        _ -> Nothing
  Megaparsec.notFollowedBy (single (Lexer.Punctuation Lexer.ColonColon))
  pure $ Identifier{name = tok.value, ann = tok.span}

pRegionIdentifier :: Parser (RegionIdentifier Lexer.SourceSpan)
pRegionIdentifier = do
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "region identifier")
      \case
        Lexer.Region i -> Just i
        _ -> Nothing
  pure $ RegionIdentifier{name = tok.value, ann = tok.span}

pMethodIdentifier :: Parser (Identifier Lexer.SourceSpan)
pMethodIdentifier = do
  dotTok <- single (Lexer.Punctuation Lexer.Dot)
  tok <-
    token
      (Set.singleton $ Megaparsec.Label "method identifier")
      \case
        Lexer.Identifier i -> Just i
        _ -> Nothing
  pure
    Identifier
      { name = tok.value
      , ann = Lexer.SourceSpan dotTok.span.start tok.span.end
      }

pPub :: Parser (Surface.Pub Lexer.SourceSpan, Lexer.SourceSpan)
pPub = do
  pubTok <- single (Lexer.Keyword Lexer.Pub)
  packageWithEnd <- Megaparsec.optional do
    _ <- single (Lexer.Punctuation Lexer.LeftParen)
    package' <- token
      (Set.singleton $ Megaparsec.Label "package")
      \case
        Lexer.Identifier i
          | i == "package" -> Just ()
        _ -> Nothing
    tokE <- single (Lexer.Punctuation Lexer.RightParen)
    let ann = Lexer.SourceSpan{start = package'.span.start, end = package'.span.end}
    pure (Surface.PubPackage ann, tokE.span.end)
  case packageWithEnd of
    Just (package', end) -> pure (package', Lexer.SourceSpan{start = pubTok.span.start, end})
    Nothing -> pure (Surface.PubPub, pubTok.span)
