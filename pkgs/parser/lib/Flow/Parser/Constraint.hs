{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Flow.Parser.Constraint where

import "base" Control.Monad (unless, when)
import "base" Data.Functor (void)
import "base" Data.Maybe (fromJust)
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (
  HasAnn,
  Parser,
  many1,
  pIdentifier,
  pNonQualifiedIdentifier,
  pRegionIdentifier,
  single,
 )

pBindersAppValueLevel ::
  (HasAnn ty Lexer.SourceSpan) =>
  Parser (ty Lexer.SourceSpan) ->
  Parser (Surface.BindersAppF ty Lexer.SourceSpan)
pBindersAppValueLevel pTy = Megaparsec.label "binders app value level" do
  tokS <- single (Lexer.Punctuation Lexer.ColonColonLessThan)
  elems <- Megaparsec.sepEndBy1 pTy (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.GreaterThan)
  pure $
    Surface.BindersAppF
      { types = fromJust $ NonEmptyVector.fromList elems
      , ann = Lexer.SourceSpan tokS.span.start tokE.span.end
      }

pBindersAppTypeLevel ::
  (HasAnn ty Lexer.SourceSpan) =>
  Parser (ty Lexer.SourceSpan) ->
  Parser (Surface.BindersAppF ty Lexer.SourceSpan)
pBindersAppTypeLevel pTy = Megaparsec.label "binders app type level" do
  tokS <- single (Lexer.Punctuation Lexer.LessThan)
  elems <- Megaparsec.sepEndBy1 pTy (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.GreaterThan)
  pure
    Surface.BindersAppF
      { types = fromJust $ NonEmptyVector.fromList elems
      , ann = Lexer.SourceSpan tokS.span.start tokE.span.end
      }

pQualifierPrefix ::
  Parser (Surface.QualifierPrefixF Lexer.SourceSpan)
pQualifierPrefix =
  Megaparsec.choice
    [ Surface.QlfrPrfxSelf . (.span) <$> single (Lexer.Keyword Lexer.Self)
    , Surface.QlfrPrfxSupers . NonEmptyVector.fromNonEmpty . fmap (.span)
        <$> many1 (single (Lexer.Keyword Lexer.Super) <* single (Lexer.Punctuation Lexer.ColonColon))
    ]

pQualifiedIdentifier ::
  forall ty.
  (HasAnn ty Lexer.SourceSpan) =>
  Parser (ty Lexer.SourceSpan) ->
  Parser (Surface.QualifiedIdentifierF ty Lexer.SourceSpan)
pQualifiedIdentifier pTy = do
  qualifierPrefix <- Megaparsec.optional pQualifierPrefix
  qualifier <- Megaparsec.many (Megaparsec.try (pIdentifier <* moduleSeparator))
  typeQualifier <- Megaparsec.optional $ Megaparsec.try do
    typeName <- pIdentifier
    typeParams <- pBindersAppTypeLevel pTy
    _ <- single (Lexer.Punctuation Lexer.ColonColon)
    pure
      Surface.TypeQualifierF
        { typeName
        , typeParams
        }
  identifier <- pNonQualifiedIdentifier
  pure $
    Surface.QualifiedIdentifierF
      { qualifierPrefix
      , qualifier = NonEmptyVector.fromList qualifier
      , typeQualifier
      , identifier
      , ann =
          Lexer.SourceSpan
            { start = case qualifier of
                [] -> identifier.ann.start
                q : _ -> q.ann.start
            , end = identifier.ann.end
            }
      }
 where
  moduleSeparator = single (Lexer.Punctuation Lexer.ColonColon)

pKindTreeRoot ::
  (HasAnn ty Lexer.SourceSpan) =>
  Parser (ty Lexer.SourceSpan) ->
  Parser (Surface.KindTreeRootF ty Lexer.SourceSpan, Lexer.SourceSpan)
pKindTreeRoot pTy = do
  tokS <- single (Lexer.Punctuation Lexer.LessThan)
  trees <- Megaparsec.sepEndBy1 pKindTree (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.GreaterThan)
  pure
    ( fromJust $ NonEmptyVector.fromList trees
    , Lexer.SourceSpan
        { start = tokS.span.start
        , end = tokE.span.end
        }
    )
 where
  pKindTree =
    Megaparsec.choice
      [ Megaparsec.try pKindParams
      , pKindHole
      ]

  pKindHole = do
    tokS <- single (Lexer.Identifier "_")
    typeType <- Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.Colon)
      pTy
    pure $
      Surface.KTHoleF
        Surface.KindHoleF
          { holeAnn = tokS.span
          , typeType = typeType
          , ann =
              Lexer.SourceSpan
                { start = tokS.span.start
                , end = case typeType of
                    Just ty -> ty.ann.end
                    Nothing -> tokS.span.end
                }
          }
  pKindParams = do
    tokS <- single (Lexer.Identifier "_")
    _ <- single (Lexer.Punctuation Lexer.LessThan)
    params <- Megaparsec.sepEndBy1 pKindTree (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.GreaterThan)
    typeType <- Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.Colon)
      pTy
    pure $
      Surface.KTParamsF
        Surface.KindParamsF
          { holeAnn = tokS.span
          , params = fromJust $ NonEmptyVector.fromList params
          , typeType
          , ann =
              Lexer.SourceSpan
                { start = tokS.span.start
                , end = case typeType of
                    Just ty -> ty.ann.end
                    Nothing -> tokE.span.end
                }
          }

pBindersWoConstraints ::
  forall ty.
  (HasAnn ty Lexer.SourceSpan) =>
  Parser (ty Lexer.SourceSpan) ->
  Parser (Surface.BindersWoConstraintsF ty Lexer.SourceSpan)
pBindersWoConstraints pTy = Megaparsec.label "binders with constraints" do
  tokS <- single (Lexer.Punctuation Lexer.LessThan)
  regionBinders <- Megaparsec.sepBy pRegionIdentifier (single (Lexer.Punctuation Lexer.Comma))
  unless (null regionBinders) do
    void $ single (Lexer.Punctuation Lexer.Comma)
  typeBinders <- Megaparsec.sepBy pBinderWoConstraints (single (Lexer.Punctuation Lexer.Comma))
  when (null regionBinders && null typeBinders) do
    fail "Expected at least one region or type binder"
  _ <- Megaparsec.optional $ single (Lexer.Punctuation Lexer.Comma)
  tokE <- single (Lexer.Punctuation Lexer.GreaterThan)
  pure
    Surface.BindersF
      { regions = Vector.fromList (fmap Surface.RegionBinderWoConstraintsF regionBinders)
      , types = Vector.fromList typeBinders
      , ann = Lexer.SourceSpan{start = tokS.span.start, end = tokE.span.end}
      }
 where
  pBinderWoConstraints = do
    name <- pIdentifier
    kindShort <- Megaparsec.optional (pKindTreeRoot pTy)
    typeType <- Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.Colon)
      pTy
    pure $
      Surface.BinderWoConstraintF
        { name = name
        , kindShort = fst <$> kindShort
        , typeType = typeType
        , ann =
            Lexer.SourceSpan
              { start = name.ann.start
              , end = case typeType of
                  Just ty -> ty.ann.end
                  Nothing -> case kindShort of
                    Just (_, ann) -> ann.end
                    Nothing -> name.ann.end
              }
        }

pWhereBlockHead ::
  forall ty.
  (HasAnn ty Lexer.SourceSpan) =>
  Parser (ty Lexer.SourceSpan) ->
  Parser (Surface.WhereBlockF ty Lexer.SourceSpan)
pWhereBlockHead pTy = do
  tokS <- single (Lexer.Keyword Lexer.Where)
  clauses <- fromJust . NonEmptyVector.fromList <$> Megaparsec.sepEndBy1 (pWhereClause pTy) (single (Lexer.Punctuation Lexer.Comma))
  pure $
    Surface.WhereBlockF
      { clauses = fmap fst clauses
      , ann =
          Lexer.SourceSpan
            { start = tokS.span.start
            , end = (snd $ NonEmptyVector.last clauses).end
            }
      }

pWhereBlockNested ::
  forall ty.
  (HasAnn ty Lexer.SourceSpan) =>
  Parser (ty Lexer.SourceSpan) ->
  Parser (Surface.WhereBlockF ty Lexer.SourceSpan)
pWhereBlockNested pTy = do
  tokS <- single (Lexer.Keyword Lexer.Where)
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  clauses <- fromJust . NonEmptyVector.fromList <$> Megaparsec.sepEndBy1 (pWhereClause pTy) (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  pure $
    Surface.WhereBlockF
      { clauses = fmap fst clauses
      , ann = Lexer.SourceSpan{start = tokS.span.start, end = tokE.span.end}
      }

pWhereClause ::
  forall ty.
  (HasAnn ty Lexer.SourceSpan) =>
  Parser (ty Lexer.SourceSpan) ->
  Parser (Surface.WhereClauseF ty Lexer.SourceSpan, Lexer.SourceSpan)
pWhereClause pTy =
  Megaparsec.choice
    [ pWhereConstraint
    , pWhereAlias
    ]
 where
  pWhereConstraint = do
    ty <- pTy
    pure (Surface.WhereConstraintF ty, ty.ann)
  pWhereAlias = do
    typeDefinition <- pTypeDefinition pTy
    pure (Surface.WhereAliasF typeDefinition, typeDefinition.ann)

pTypeDefinition ::
  forall ty.
  (HasAnn ty Lexer.SourceSpan) =>
  Parser (ty Lexer.SourceSpan) ->
  Parser (Surface.TypeDefinitionF ty Lexer.SourceSpan)
pTypeDefinition pTy = do
  tokS <- single (Lexer.Keyword Lexer.Type)
  name <- pIdentifier
  typeParams <- Megaparsec.optional (pBindersWoConstraints pTy)
  _ <- single (Lexer.Punctuation Lexer.Assign)
  type_ <- pTy
  pure $
    Surface.TypeDefinitionF
      { name
      , typeParams
      , type_
      , ann =
          Lexer.SourceSpan
            { start = tokS.span.start
            , end = type_.ann.end
            }
      }
