{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Flow.Parser.Type where

import "base" Data.Maybe (fromJust)
import "containers" Data.Set qualified as Set
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface qualified as Surface
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Type (FnEffectsF (..))
import Flow.AST.Surface.Type qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (
  HasAnn,
  Parser,
  SourceSpan (..),
  pIdentifier,
  pRegionIdentifier,
  single,
  token,
 )
import Flow.Parser.Constraint (
  pBindersAppTypeLevel,
  pBindersWoConstraints,
  pQualifiedIdentifier,
  pWhereBlockNested,
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
    i <- pQualifiedIdentifier pType
    pure Surface.Type{ty = Surface.TyIdentifierF i, ann = i.ann}

  pParens' = do
    (ty, ann) <- pParens pType
    pure Surface.Type{ty = Surface.TyParensF ty, ann = ann}

  pTuple' = do
    (args, ann) <- pTuple pType
    pure Surface.Type{ty = Surface.TyTupleF args, ann = ann}

  pRef' = do
    r <- pRef
    pure Surface.Type{ty = Surface.TyRefF r, ann = r.ann}

  pRefApp' = do
    r <- pRef
    inner <- pType
    pure Surface.Type{ty = Surface.TyRefAppF r inner, ann = inner.ann}

  pFn' = do
    fn <- pFn pType
    pure Surface.Type{ty = Surface.TyFnF fn, ann = fn.ann}

  pEffectRow' = do
    row <- pEffectRow pType
    pure Surface.Type{ty = Surface.TyEffectRowF row, ann = row.ann}

  pForall' = do
    forall' <- pForall pType
    pure Surface.Type{ty = Surface.TyForallF forall', ann = forall'.ann}

  pAppSuffix' ty = do
    app <- pAppSuffix pType ty
    pure Surface.Type{ty = Surface.TyAppF app, ann = app.ann}

  pTyEqualsSuffix' ty = do
    (left, right) <- pTyEqualsSuffix pType ty
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

pParens ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (ty SourceSpan, SourceSpan)
pParens pTy = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  ty <- pTy
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure (ty, SourceSpan{start = tokS.span.start, end = tokE.span.end})

pTuple ::
  Parser (Surface.Type SourceSpan) ->
  Parser (NonEmptyVector (Surface.Type SourceSpan), SourceSpan)
pTuple pTy = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  args <- Megaparsec.sepEndBy1 pTy (single (Lexer.Punctuation Lexer.Comma))
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
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  ty SourceSpan ->
  Parser (Surface.AppF ty SourceSpan)
pAppSuffix pTy ty = do
  args <- pBindersAppTypeLevel pTy
  pure
    Surface.AppF
      { head = ty
      , args = args
      , ann = SourceSpan{start = ty.ann.start, end = args.ann.end}
      }

pFn ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (Surface.FnF ty SourceSpan)
pFn pTy = do
  tokS <- single (Lexer.Keyword Lexer.Fn)
  _ <- single (Lexer.Punctuation Lexer.LeftParen)
  args <- Megaparsec.sepEndBy pTy (single (Lexer.Punctuation Lexer.Comma))
  argsClose <- single (Lexer.Punctuation Lexer.RightParen)
  effectsResult <- Megaparsec.optional (pFnEffectsResult pTy)
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

pFnEffectsResult ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (Surface.FnEffectsResultF ty SourceSpan)
pFnEffectsResult pTy = do
  tokArrow <- single (Lexer.Punctuation Lexer.Arrow)
  effects <- Megaparsec.optional do
    Megaparsec.choice
      [ FnEffectsRowF <$> pFnEffectRow pTy
      , FnEffectsTypeF <$> do
          _ <- single (Lexer.Punctuation Lexer.At)
          pTy
      ]
  result <- pTy
  pure
    Surface.FnEffectsResultF
      { effects = effects
      , result = result
      , ann = SourceSpan{start = tokArrow.span.start, end = result.ann.end}
      }

pFnEffectRow ::
  forall ty.
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (Surface.FnEffectRowF ty SourceSpan)
pFnEffectRow pTy = do
  tokS <- single (Lexer.Punctuation Lexer.AtLeftBracket)
  effects <- Megaparsec.sepEndBy1 effectRowElem (single (Lexer.Punctuation Lexer.Comma))
  (regions, effects', tailVars) <- collect effects [] [] []
  tokE <- single (Lexer.Punctuation Lexer.RightBracket)
  pure
    Surface.FnEffectRowF
      { regions = Vector.fromList regions
      , effects = Vector.fromList effects'
      , tailVars = Vector.fromList tailVars
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

  tailVarElem :: Parser (ty SourceSpan, SourceSpan)
  tailVarElem = do
    tokDD <- single (Lexer.Punctuation Lexer.DotDot)
    ty <- pTy
    pure (ty, SourceSpan{start = tokDD.span.start, end = ty.ann.end})

  effectAtom :: Parser (Surface.FnEffectAtomF ty SourceSpan)
  effectAtom = do
    Megaparsec.choice
      [ Megaparsec.try eAtomNameType
      , eAtomType
      ]
   where
    eAtomNameType :: Parser (Surface.FnEffectAtomF ty SourceSpan)
    eAtomNameType = do
      name <- pIdentifier
      _ <- single (Lexer.Punctuation Lexer.Colon)
      Surface.FnEffectAtomNameTypeF name <$> pTy

    eAtomType :: Parser (Surface.FnEffectAtomF ty SourceSpan)
    eAtomType = Surface.FnEffectAtomTypeF <$> pTy

pEffectRow ::
  forall ty.
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (Surface.EffectRowF ty SourceSpan)
pEffectRow pTy = do
  tokS <- single (Lexer.Punctuation Lexer.AtLeftBracket)
  effects <- Megaparsec.sepEndBy1 effectRowElem (single (Lexer.Punctuation Lexer.Comma))
  (effects', tailVars) <- collect effects [] []
  tokE <- single (Lexer.Punctuation Lexer.RightBracket)
  pure $
    Surface.EffectRowF
      { effects = Vector.fromList effects'
      , tailVars = Vector.fromList tailVars
      , ann = SourceSpan{start = tokS.span.start, end = tokE.span.end}
      }
 where
  effectRowElem = do
    Megaparsec.choice
      [ Left <$> pTy
      , Right <$> tailVarElem
      ]

  tailVarElem :: Parser (ty SourceSpan, SourceSpan)
  tailVarElem = do
    tokDD <- single (Lexer.Punctuation Lexer.DotDot)
    ty <- pTy
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

pForall ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (Surface.ForallF ty SourceSpan)
pForall pTy = do
  params <- pBindersWoConstraints pTy
  result <- pTy
  whereBlock <- Megaparsec.optional (pWhereBlockNested pTy)
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
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  ty SourceSpan ->
  Parser (ty SourceSpan, ty SourceSpan)
pTyEqualsSuffix pTy left = do
  _ <- single (Lexer.Punctuation Lexer.Equal)
  right <- pTy
  pure (left, right)
