module Flow.Parser.Data where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface.Common (Identifier (..))
import Flow.AST.Surface.Data qualified as Surface
import Flow.AST.Surface.Type qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser, SourceSpan (..), pIdentifier, pPub, sepEndBy1, single)
import Flow.Parser.Type (pBindersWoConstraints, pType, pWhereBlockHead, pWhereBlockNested)

pStruct ::
  Parser (Surface.StructF SourceSpan)
pStruct = do
  structTok <- single (Lexer.Keyword Lexer.Struct)
  name <- pIdentifier
  typeParams <- Megaparsec.optional pBindersWoConstraints
  whereBlock <- Megaparsec.optional pWhereBlockHead
  (fields, fieldsAnn) <- pFieldsDecl
  let ann = SourceSpan{start = structTok.span.start, end = fieldsAnn.end}
  pure
    Surface.StructF
      { name
      , typeParams
      , whereBlock
      , fields
      , ann
      }

pEnum ::
  Parser (Surface.EnumF SourceSpan)
pEnum = do
  enumTok <- single (Lexer.Keyword Lexer.Enum)
  name <- pIdentifier
  typeParams <- Megaparsec.optional pBindersWoConstraints
  whereBlock <- Megaparsec.optional pWhereBlockHead
  (variants, variantsAnn) <- pEnumVariants
  let ann = SourceSpan{start = enumTok.span.start, end = variantsAnn.end}
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
    variants <- sepEndBy1 pEnumVariant (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure
      ( Surface.EVariantsSimpleF $ NonEmptyVector.fromNonEmpty variants
      , SourceSpan{start = tokS.span.start, end = tokE.span.end}
      )
   where
    pEnumVariant = do
      name <- pIdentifier
      fields <- Megaparsec.optional pFieldsDecl
      let ann =
            SourceSpan
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
    variants <- sepEndBy1 pEnumVariantGeneralized (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure
      ( Surface.EVariantsGeneralized $ NonEmptyVector.fromNonEmpty variants
      , SourceSpan{start = tokS.span.start, end = tokE.span.end}
      )
   where
    pEnumVariantGeneralized = do
      name <- pIdentifier
      typeParams <- Megaparsec.optional pBindersWoConstraints
      fields <- Megaparsec.optional pFieldsDecl
      _ <- single (Lexer.Punctuation Lexer.Colon)
      result <- pType
      whereBlock <- Megaparsec.optional pWhereBlockNested
      let ann = SourceSpan{start = name.ann.start, end = result.ann.end}
      pure
        Surface.EnumVariantGeneralizedF
          { name
          , typeParams = typeParams
          , fields = fst <$> fields
          , result
          , whereBlock
          , ann
          }

pFieldsDecl :: Parser (Surface.FieldsDeclF SourceSpan, SourceSpan)
pFieldsDecl = do
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
    type_ <- pType
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
    fields <- Megaparsec.sepEndBy pType (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightParen)
    pure
      ( Surface.FieldsDeclTupleF $ Vector.fromList fields
      , Lexer.SourceSpan{start = tokS.span.start, end = tokE.span.end}
      )
