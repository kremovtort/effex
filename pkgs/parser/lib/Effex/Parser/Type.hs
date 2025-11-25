{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Effex.Parser.Type where

import "base" Control.Monad (unless, when)
import "base" Data.Functor (void)
import "base" Data.Maybe (fromJust)
import "containers" Data.Set qualified as Set
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Effex.AST.Surface.Common qualified as Surface
import Effex.AST.Surface.Type (FnEffectsF (..))
import Effex.AST.Surface.Type qualified as Surface
import Effex.Lexer qualified as Lexer
import Effex.Parser.Common (
  Parser,
  SourceSpan (..),
  many1,
  pIdentifier,
  pNonQualifiedIdentifier,
  pRegionIdentifier,
  single,
  token,
 )

pType :: Parser (Surface.Type SourceSpan)
pType = do
  head' <-
    Megaparsec.choice
      [ pApp'
      , pNotAppable'
      ]
  tyWSuffix <- Megaparsec.optional $ pTyEqualsSuffix' head'
  case tyWSuffix of
    Just tyWSuffix' -> pure tyWSuffix'
    Nothing -> pure head'
 where
  pApp' = Megaparsec.try do
    ty <- pAppable'
    tyWSuffix <- Megaparsec.optional $ pAppSuffix' ty
    case tyWSuffix of
      Just tyWSuffix' -> pure tyWSuffix'
      Nothing -> pure ty

  pAppable' =
    Megaparsec.choice
      [ Megaparsec.label "type identifier" pQualifiedIdentifier'
      , Megaparsec.label "type parens" $ Megaparsec.try pParens'
      ]

  pNotAppable' =
    Megaparsec.choice
      [ Megaparsec.label "region" pRegion'
      , Megaparsec.label "builtin" pBuiltin'
      , Megaparsec.label "tuple" pTuple'
      , Megaparsec.label "ref app" $ Megaparsec.try pRefApp'
      , Megaparsec.label "ref" pRef'
      , Megaparsec.label "fn" pFn'
      , Megaparsec.label "effect row" pEffectRow'
      , Megaparsec.label "forall" pForall'
      ]

  pRegion' = do
    region <- pRegionIdentifier
    pure Surface.Type{ty = Surface.TyRegionF region, ann = region.ann}

  pBuiltin' = do
    (b, ann) <- pBuiltin
    pure Surface.Type{ty = Surface.TyBuiltinF b, ann = ann}

  pQualifiedIdentifier' = do
    i <- pQualifiedIdentifier
    pure Surface.Type{ty = Surface.TyIdentifierF i, ann = i.ann}

  pParens' = do
    (ty, ann) <- pParens
    pure Surface.Type{ty = Surface.TyParensF ty, ann = ann}

  pTuple' = do
    (args, ann) <- pTuple
    pure Surface.Type{ty = Surface.TyTupleF args, ann = ann}

  pRef' = do
    r <- pRef
    pure Surface.Type{ty = Surface.TyRefF r, ann = r.ann}

  pRefApp' = do
    r <- pRef
    inner <- pType
    pure Surface.Type{ty = Surface.TyRefAppF r inner, ann = inner.ann}

  pFn' = do
    fn <- pFn
    pure Surface.Type{ty = Surface.TyFnF fn, ann = fn.ann}

  pEffectRow' = do
    row <- pEffectRow
    pure Surface.Type{ty = Surface.TyEffectRowF row, ann = row.ann}

  pForall' = do
    forall' <- pForall
    pure Surface.Type{ty = Surface.TyForallF forall', ann = forall'.ann}

  pAppSuffix' ty = do
    app <- pAppSuffix ty
    pure Surface.Type{ty = Surface.TyAppF app, ann = app.ann}

  pTyEqualsSuffix' ty = do
    (left, right) <- pTyEqualsSuffix ty
    pure
      Surface.Type
        { ty = Surface.TyEquals left right
        , ann = SourceSpan{start = left.ann.start, end = right.ann.end}
        }

pBuiltin :: Parser (Surface.Builtin, SourceSpan)
pBuiltin = do
  tok <- token (Set.singleton $ Megaparsec.Label "builtin type") \case
    Lexer.Punctuation Lexer.LeftRightParen -> Just Surface.BuiltinUnit
    Lexer.Punctuation Lexer.Not -> Just Surface.BuiltinNever
    _ -> Nothing
  pure (tok.value, tok.span)

pParens :: Parser (Surface.Type SourceSpan, SourceSpan)
pParens = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  ty <- pType
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure (ty, SourceSpan{start = tokS.span.start, end = tokE.span.end})

pTuple :: Parser (NonEmptyVector (Surface.Type SourceSpan), SourceSpan)
pTuple = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  args <- Megaparsec.sepEndBy1 pType (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure
    ( fromJust $ NonEmptyVector.fromList args
    , SourceSpan{start = tokS.span.start, end = tokE.span.end}
    )

pRef :: Parser (Surface.RefF SourceSpan)
pRef = do
  tokS <- single (Lexer.Punctuation Lexer.Ampersand)
  region <- Megaparsec.optional pRegionIdentifier
  mut <- Megaparsec.optional $ do
    tok <- single (Lexer.Keyword Lexer.Mut)
    pure SourceSpan{start = tok.span.start, end = tok.span.end}
  pure $
    Surface.RefF
      { region = region
      , mutability = mut
      , ann =
          SourceSpan
            { start = tokS.span.start
            , end = case mut of
                Just mut' -> mut'.end
                Nothing -> case region of
                  Just region' -> region'.ann.end
                  Nothing -> tokS.span.end
            }
      }

pAppSuffix ::
  Surface.Type SourceSpan ->
  Parser (Surface.AppF SourceSpan)
pAppSuffix ty = do
  args <- pBindersAppTypeLevel
  pure
    Surface.AppF
      { head = ty
      , args = args
      , ann = SourceSpan{start = ty.ann.start, end = args.ann.end}
      }

pFn :: Parser (Surface.FnF SourceSpan)
pFn = do
  tokS <- single (Lexer.Keyword Lexer.Fn)
  _ <- single (Lexer.Punctuation Lexer.LeftParen)
  args <- Megaparsec.sepEndBy pType (single (Lexer.Punctuation Lexer.Comma))
  argsClose <- single (Lexer.Punctuation Lexer.RightParen)
  effectsResult <- Megaparsec.optional pFnEffectsResult
  pure $
    Surface.FnF
      { args = Vector.fromList args
      , effectsResult = effectsResult
      , ann =
          SourceSpan
            { start = tokS.span.start
            , end = case effectsResult of
                Just effectsResult' -> effectsResult'.ann.end
                Nothing -> argsClose.span.end
            }
      }

pFnEffectsResult :: Parser (Surface.FnEffectsResultF SourceSpan)
pFnEffectsResult = do
  tokArrow <- single (Lexer.Punctuation Lexer.Arrow)
  effects <- Megaparsec.optional do
    Megaparsec.choice
      [ FnEffectsRowF <$> pFnEffectRow
      , FnEffectsTypeF <$> do
          _ <- single (Lexer.Punctuation Lexer.At)
          pType
      ]
  result <- pType
  pure
    Surface.FnEffectsResultF
      { effects = effects
      , result = result
      , ann = SourceSpan{start = tokArrow.span.start, end = result.ann.end}
      }

pFnEffectRow :: Parser (Surface.FnEffectRowF SourceSpan)
pFnEffectRow = do
  tokS <- single (Lexer.Punctuation Lexer.AtLeftBracket)
  effects <- Megaparsec.sepEndBy1 effectRowElem (single (Lexer.Punctuation Lexer.Comma))
  (regions, effects', tailVars) <- collect effects [] [] []
  tokE <- single (Lexer.Punctuation Lexer.RightBracket)
  pure
    Surface.FnEffectRowF
      { regions = Vector.fromList regions
      , effects = Vector.fromList effects'
      , tailVars = Vector.fromList $ fmap fst tailVars
      , ann = SourceSpan{start = tokS.span.start, end = tokE.span.end}
      }
 where
  effectRowElem = do
    Megaparsec.choice
      [ Left . Left <$> pRegionIdentifier
      , Left . Right <$> effectAtom
      , Right <$> tailVarElem
      ]

  collect [] regionsAcc effectsAcc tailVarsAcc =
    pure (reverse regionsAcc, reverse effectsAcc, reverse tailVarsAcc)
  collect (Left (Left region) : rest) regionsAcc effectsAcc tailVarsAcc
    | not (null effectsAcc) || not (null tailVarsAcc) =
        fail "region argument cannot follow effect argument in effect row"
    | otherwise =
        collect rest (region : regionsAcc) effectsAcc tailVarsAcc
  collect (Left (Right effect) : rest) regionsAcc effectsAcc tailVarsAcc
    | not (null tailVarsAcc) =
        fail "effect argument cannot follow tail variable in effect row"
    | otherwise =
        collect rest regionsAcc (effect : effectsAcc) tailVarsAcc
  collect (Right tailVar : rest) regionsAcc effectsAcc tailVarsAcc =
    collect rest regionsAcc effectsAcc (tailVar : tailVarsAcc)

  tailVarElem :: Parser (Surface.Type SourceSpan, SourceSpan)
  tailVarElem = do
    tokDD <- single (Lexer.Punctuation Lexer.DotDot)
    ty <- pType
    pure (ty, SourceSpan{start = tokDD.span.start, end = ty.ann.end})

  effectAtom :: Parser (Surface.FnEffectAtomF SourceSpan)
  effectAtom = do
    Megaparsec.choice
      [ Megaparsec.try eAtomNameType
      , eAtomType
      ]
   where
    eAtomNameType :: Parser (Surface.FnEffectAtomF SourceSpan)
    eAtomNameType = do
      name <- pIdentifier
      _ <- single (Lexer.Punctuation Lexer.Colon)
      Surface.FnEffectAtomNameTypeF name <$> pType

    eAtomType :: Parser (Surface.FnEffectAtomF SourceSpan)
    eAtomType = Surface.FnEffectAtomTypeF <$> pType

pEffectRow :: Parser (Surface.EffectRowF SourceSpan)
pEffectRow = do
  tokS <- single (Lexer.Punctuation Lexer.AtLeftBracket)
  effects <- Megaparsec.sepEndBy1 effectRowElem (single (Lexer.Punctuation Lexer.Comma))
  (effects', tailVars) <- collect effects [] []
  tokE <- single (Lexer.Punctuation Lexer.RightBracket)
  pure $
    Surface.EffectRowF
      { effects = Vector.fromList effects'
      , tailVars = Vector.fromList $ map fst tailVars
      , ann = SourceSpan{start = tokS.span.start, end = tokE.span.end}
      }
 where
  effectRowElem = do
    Megaparsec.choice
      [ Left <$> pType
      , Right <$> tailVarElem
      ]

  tailVarElem :: Parser (Surface.Type SourceSpan, SourceSpan)
  tailVarElem = do
    tokDD <- single (Lexer.Punctuation Lexer.DotDot)
    ty <- pType
    pure (ty, SourceSpan{start = tokDD.span.start, end = ty.ann.end})

  collect [] effectsAcc tailVarsAcc =
    pure (reverse effectsAcc, reverse tailVarsAcc)
  collect (Left effect : rest) effectsAcc tailVarsAcc
    | not (null tailVarsAcc) =
        fail "effect argument cannot follow tail variable in effect row"
    | otherwise =
        collect rest (effect : effectsAcc) tailVarsAcc
  collect (Right tailVar : rest) effectsAcc tailVarsAcc =
    collect rest effectsAcc (tailVar : tailVarsAcc)

pForall :: Parser (Surface.ForallF SourceSpan)
pForall = do
  params <- pBindersWoConstraints
  result <- pType
  whereBlock <- Megaparsec.optional pWhereBlockNested
  pure
    Surface.ForallF
      { params
      , result
      , whereBlock
      , ann =
          SourceSpan
            { start = params.ann.start
            , end = case whereBlock of
                Just whereBlock' -> whereBlock'.ann.end
                Nothing -> result.ann.end
            }
      }

pTyEqualsSuffix ::
  Surface.Type SourceSpan ->
  Parser (Surface.Type SourceSpan, Surface.Type SourceSpan)
pTyEqualsSuffix left = do
  _ <- single (Lexer.Punctuation Lexer.Equal)
  right <- pType
  pure (left, right)

pBindersAppValueLevel :: Parser (Surface.TypeArgumentsF Lexer.SourceSpan)
pBindersAppValueLevel = Megaparsec.label "binders app value level" do
  tokS <- single (Lexer.Punctuation Lexer.ColonColonLessThan)
  elems <- Megaparsec.sepEndBy1 pType (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.GreaterThan)
  pure $
    Surface.TypeArgumentsF
      { types = fromJust $ NonEmptyVector.fromList elems
      , ann = Lexer.SourceSpan tokS.span.start tokE.span.end
      }

pBindersAppTypeLevel :: Parser (Surface.TypeArgumentsF Lexer.SourceSpan)
pBindersAppTypeLevel = Megaparsec.label "binders app type level" do
  tokS <- single (Lexer.Punctuation Lexer.LessThan)
  elems <- Megaparsec.sepEndBy1 pType (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.GreaterThan)
  pure
    Surface.TypeArgumentsF
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
  Parser (Surface.QualifiedIdentifierF Lexer.SourceSpan)
pQualifiedIdentifier = do
  qualifierPrefix <- Megaparsec.optional pQualifierPrefix
  qualifier <- Megaparsec.many (Megaparsec.try (pIdentifier <* moduleSeparator))
  typeQualifier <- Megaparsec.optional $ Megaparsec.try do
    typeName <- pIdentifier
    typeParams <- pBindersAppTypeLevel
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
  Parser (Surface.KindTreeRootF Lexer.SourceSpan, Lexer.SourceSpan)
pKindTreeRoot = do
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
      pType
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
      pType
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

pBindersWoConstraints :: Parser (Surface.BindersWoConstraintsF Lexer.SourceSpan)
pBindersWoConstraints = Megaparsec.label "binders with constraints" do
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
    kindShort <- Megaparsec.optional pKindTreeRoot
    typeType <- Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.Colon)
      pType
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

pWhereBlockHead :: Parser (Surface.WhereBlockF Lexer.SourceSpan)
pWhereBlockHead = do
  tokS <- single (Lexer.Keyword Lexer.Where)
  clauses <- fromJust . NonEmptyVector.fromList <$> Megaparsec.sepEndBy1 pWhereClause (single (Lexer.Punctuation Lexer.Comma))
  pure $
    Surface.WhereBlockF
      { clauses = fmap fst clauses
      , ann =
          Lexer.SourceSpan
            { start = tokS.span.start
            , end = (snd $ NonEmptyVector.last clauses).end
            }
      }

pWhereBlockNested :: Parser (Surface.WhereBlockF Lexer.SourceSpan)
pWhereBlockNested = do
  tokS <- single (Lexer.Keyword Lexer.Where)
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  clauses <- fromJust . NonEmptyVector.fromList <$> Megaparsec.sepEndBy1 pWhereClause (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  pure $
    Surface.WhereBlockF
      { clauses = fmap fst clauses
      , ann = Lexer.SourceSpan{start = tokS.span.start, end = tokE.span.end}
      }

pWhereClause :: Parser (Surface.WhereClauseF Lexer.SourceSpan, Lexer.SourceSpan)
pWhereClause =
  Megaparsec.choice
    [ pWhereConstraint
    , pWhereAlias
    ]
 where
  pWhereConstraint = do
    ty <- pType
    pure (Surface.WhereConstraintF ty, ty.ann)
  pWhereAlias = do
    typeDefinition <- pTypeDefinition
    pure (Surface.WhereAliasF typeDefinition, typeDefinition.ann)

pTypeDefinition :: Parser (Surface.TypeDefinitionF Lexer.SourceSpan)
pTypeDefinition = do
  tokS <- single (Lexer.Keyword Lexer.Type)
  name <- pIdentifier
  typeParams <- Megaparsec.optional pBindersWoConstraints
  _ <- single (Lexer.Punctuation Lexer.Assign)
  type_ <- pType
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
