module Flow.Parser.Decl where

import "megaparsec" Text.Megaparsec qualified as Megaparsec

import Data.Maybe (fromJust, isJust)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NonEmptyVector
import Flow.AST.Surface.Callable qualified as Surface
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Decl qualified as Surface
import Flow.Lexer (SourceSpan (..))
import Flow.Lexer qualified as Lexer
import Flow.Parser.Callable (
  pFnDeclaration,
  pFnDefinition,
  pFnInfixDeclaration,
  pFnInfixDefinition,
  pOpDeclaration,
  pOpDefinition,
  pOpInfixDeclaration,
  pOpInfixDefinition,
 )
import Flow.Parser.Common (
  HasAnn,
  Parser,
  pPub,
  pIdentifier,
  single,
 )
import Flow.Parser.Constraint (
  pBindersWoConstraints,
  pKindTreeRoot,
  pWhereBlockHead,
  pWhereBlockNested,
 )

pStruct ::
  (HasAnn ty Lexer.SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (Surface.StructF ty SourceSpan)
pStruct pTy = do
  structTok <- single (Lexer.Keyword Lexer.Struct)
  name <- pIdentifier
  typeParams <- Megaparsec.optional (pBindersWoConstraints pTy)
  whereBlock <- Megaparsec.optional (pWhereBlockHead pTy)
  (fields, fieldsAnn) <- pFieldsDecl pTy
  let ann = Lexer.SourceSpan{start = structTok.span.start, end = fieldsAnn.end}
  pure
    Surface.StructF
      { name
      , typeParams
      , whereBlock
      , fields
      , ann
      }

pEnum ::
  (HasAnn ty Lexer.SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (Surface.EnumF ty SourceSpan)
pEnum pTy = do
  enumTok <- single (Lexer.Keyword Lexer.Enum)
  name <- pIdentifier
  typeParams <- Megaparsec.optional (pBindersWoConstraints pTy)
  whereBlock <- Megaparsec.optional (pWhereBlockHead pTy)
  (variants, variantsAnn) <- pEnumVariants
  let ann = Lexer.SourceSpan{start = enumTok.span.start, end = variantsAnn.end}
  pure
    Surface.EnumF
      { name
      , typeParams
      , whereBlock
      , variants
      , ann
      }
 where
  pEnumVariants = do
    Megaparsec.choice
      [ pEnumVariantsSimple
      , pEnumVariantsGeneralized
      ]
  pEnumVariantsSimple = do
    tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
    variants <- Megaparsec.sepEndBy1 pEnumVariant (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure
      ( Surface.EVariantsSimpleF $ fromJust $ NonEmptyVector.fromList variants
      , Lexer.SourceSpan{start = tokS.span.start, end = tokE.span.end}
      )
   where
    pEnumVariant = do
      name <- pIdentifier
      fields <- Megaparsec.optional (pFieldsDecl pTy)
      let ann =
            Lexer.SourceSpan
              { start = name.ann.start
              , end = case fields of
                  Just (_, ann') -> ann'.end
                  Nothing -> name.ann.end
              }
      pure
        Surface.EnumVariantF
          { name
          , fields = fst <$> fields
          , ann
          }

  pEnumVariantsGeneralized = do
    tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
    variants <- Megaparsec.sepEndBy1 pEnumVariantGeneralized (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure
      ( Surface.EVariantsGeneralized $ fromJust $ NonEmptyVector.fromList variants
      , Lexer.SourceSpan{start = tokS.span.start, end = tokE.span.end}
      )
   where
    pEnumVariantGeneralized = do
      name <- pIdentifier
      typeParams <- Megaparsec.optional (pBindersWoConstraints pTy)
      fields <- Megaparsec.optional (pFieldsDecl pTy)
      _ <- single (Lexer.Punctuation Lexer.Colon)
      result <- pTy
      whereBlock <- Megaparsec.optional (pWhereBlockNested pTy)
      let ann = Lexer.SourceSpan{start = name.ann.start, end = result.ann.end}
      pure
        Surface.EnumVariantGeneralizedF
          { name
          , typeParams = typeParams
          , fields = fst <$> fields
          , result
          , whereBlock
          , ann
          }

pFieldsDecl ::
  (HasAnn ty Lexer.SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (Surface.FieldsDeclF ty SourceSpan, SourceSpan)
pFieldsDecl pTy = do
  Megaparsec.choice
    [ pFieldsDeclNamed
    , pFieldsDeclTuple
    ]
 where
  pFieldsDeclNamed = do
    tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
    fields <- Megaparsec.sepEndBy pFieldDecl (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure
      ( Surface.FieldsDeclNamedF $ Vector.fromList fields
      , Lexer.SourceSpan{start = tokS.span.start, end = tokE.span.end}
      )

  pFieldDecl = do
    pub <- Megaparsec.optional pPub
    name <- pIdentifier
    _ <- single (Lexer.Punctuation Lexer.Colon)
    type_ <- pTy
    pure
      Surface.FieldDeclF
        { pub = fmap fst pub
        , name = name
        , type_ = type_
        , ann =
            Lexer.SourceSpan
              { start = case pub of
                  Just (_, ann) -> ann.start
                  Nothing -> name.ann.start
              , end = type_.ann.end
              }
        }

  pFieldsDeclTuple = do
    tokS <- single (Lexer.Punctuation Lexer.LeftParen)
    fields <- Megaparsec.sepEndBy pTy (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightParen)
    pure
      ( Surface.FieldsDeclTupleF $ Vector.fromList fields
      , Lexer.SourceSpan{start = tokS.span.start, end = tokE.span.end}
      )

pTrait ::
  (HasAnn stmt SourceSpan, HasAnn ty SourceSpan, HasAnn expr SourceSpan) =>
  Parser (stmt SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (Surface.TraitF stmt ty expr SourceSpan)
pTrait pStmt pTy pExpr = do
  tokSealed <- Megaparsec.optional (single (Lexer.Keyword Lexer.Sealed))
  tokTrait <- single (Lexer.Keyword Lexer.Trait)
  name <- pIdentifier
  typeParams <- pBindersWoConstraints pTy
  superTraits <-
    fmap (fromJust . NonEmptyVector.fromList) <$> Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.FatArrow)
      Megaparsec.sepEndBy1 pTy (single (Lexer.Punctuation Lexer.Comma))
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  traitBody <- Vector.fromList <$> Megaparsec.many pTraitItem
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  pure
    Surface.TraitF
      { sealed = isJust tokSealed
      , name
      , typeParams
      , superTraits
      , traitBody
      , ann =
          Lexer.SourceSpan
            { start = case tokSealed of
                Just tokSealed' -> tokSealed'.span.start
                Nothing -> tokTrait.span.start
            , end = tokE.span.end
            }
      }
 where
  pTraitItem = do
    pub <- Megaparsec.optional pPub
    (item, itemAnn) <- pTraitItemVariant
    pure
      Surface.TraitItemF
        { pub = fmap fst pub
        , item
        , ann =
            Lexer.SourceSpan
              { start = case pub of
                  Just (_, ann') -> ann'.start
                  Nothing -> itemAnn.start
              , end = itemAnn.end
              }
        }

  pTraitItemVariant =
    Megaparsec.choice
      [ withRegion Surface.TItemVarTypeDeclarationF <$> pTypeDeclaration pTy
      , withRegion Surface.TItemVarLetDeclarationF <$> pLetDeclaration pTy
      , withRegion Surface.TItemVarFnDeclarationF <$> Megaparsec.try (pFnDeclaration pTy)
      , withRegion Surface.TItemVarFnInfixDeclarationF <$> Megaparsec.try (pFnInfixDeclaration pTy)
      , withRegion Surface.TItemVarFnDefinitionF <$> Megaparsec.try (pFnDefinition pStmt pTy pExpr)
      , withRegion Surface.TItemVarFnInfixDefinitionF <$> Megaparsec.try (pFnInfixDefinition pStmt pTy pExpr)
      ]

  withRegion f item = (f item, item.ann)

pEffect ::
  (HasAnn stmt SourceSpan, HasAnn ty SourceSpan, HasAnn expr SourceSpan) =>
  Parser (stmt SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (Surface.EffectF stmt ty expr SourceSpan)
pEffect pStmt pTy pExpr = do
  tokSealed <- Megaparsec.optional (single (Lexer.Keyword Lexer.Sealed))
  tokEffect <- single (Lexer.Keyword Lexer.Effect)
  name <- pIdentifier
  typeParams <- Megaparsec.optional (pBindersWoConstraints pTy)
  superEffects <-
    fmap (fromJust . NonEmptyVector.fromList) <$> Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.FatArrow)
      Megaparsec.sepEndBy1 pTy (single (Lexer.Punctuation Lexer.Comma))
  whereBlock <- Megaparsec.optional (pWhereBlockHead pTy)
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  effectBody <- Vector.fromList <$> Megaparsec.many pEffectItem
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  pure
    Surface.EffectF
      { sealed = isJust tokSealed
      , name
      , typeParams
      , superEffects
      , whereBlock
      , effectBody
      , ann =
          Lexer.SourceSpan
            { start = case tokSealed of
                Just tokSealed' -> tokSealed'.span.start
                Nothing -> tokEffect.span.start
            , end = tokE.span.end
            }
      }
 where
  pEffectItem = do
    pub <- Megaparsec.optional pPub
    (item, itemAnn) <- pEffectItemVariant
    pure
      Surface.EffectItemF
        { pub = fmap fst pub
        , item
        , ann = itemAnn
        }

  pEffectItemVariant =
    Megaparsec.choice
      [ withRegion Surface.EItemVarTypeDeclarationF <$> pTypeDeclaration pTy
      , withRegion Surface.EItemVarLetDeclarationF <$> pLetDeclaration pTy
      , withRegion Surface.EItemVarOpDeclarationF <$> Megaparsec.try (pOpDeclaration pTy)
      , withRegion Surface.EItemVarOpInfixDeclarationF <$> Megaparsec.try (pOpInfixDeclaration pTy)
      , withRegion Surface.EItemVarOpDefinitionF <$> Megaparsec.try (pOpDefinition pStmt pTy pExpr)
      , withRegion Surface.EItemVarOpInfixDefinitionF <$> Megaparsec.try (pOpInfixDefinition pStmt pTy pExpr)
      ]

  withRegion f item = (f item, item.ann)

pTypeDeclaration ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (Surface.TypeDeclarationF ty SourceSpan)
pTypeDeclaration pTy = do
  tokS <- single (Lexer.Keyword Lexer.Type)
  name <- pIdentifier
  kindShort <- Megaparsec.optional (pKindTreeRoot pTy)
  type_ <- Megaparsec.optional pTy
  tokE <- single (Lexer.Punctuation Lexer.Semicolon)
  pure
    Surface.TypeDeclarationF
      { name
      , kindShort
      , type_
      , ann =
          Lexer.SourceSpan
            { start = tokS.span.start
            , end = tokE.span.end
            }
      }

pLetDeclaration ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (Surface.LetDeclarationF ty SourceSpan)
pLetDeclaration pTy = do
  letTok <- single (Lexer.Keyword Lexer.Let)
  name <- pIdentifier
  _ <- single (Lexer.Punctuation Lexer.Colon)
  type_ <- pTy
  tokE <- single (Lexer.Punctuation Lexer.Semicolon)
  pure
    Surface.LetDeclarationF
      { name
      , type_
      , ann = Lexer.SourceSpan{start = letTok.span.start, end = tokE.span.end}
      }
