module Flow.Parser.Pattern where

import "base" Data.Bifunctor qualified as Bifunctor
import "base" Data.Functor ((<&>))
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector

import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Pattern qualified as Surface
import Flow.AST.Surface.Type qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (HasAnn, Parser, SourceSpan (..), pIdentifier, sepEndBy1, single)
import Flow.Parser.Literal (literal)
import Flow.Parser.Type (pBindersWoConstraints, pQualifiedIdentifier)

pPattern :: Parser (Surface.Pattern SourceSpan)
pPattern = Megaparsec.label "pattern" do
  Megaparsec.choice
    [ pPatternSimpleF pPattern <&> \patSim ->
        Surface.Pattern{pat = Surface.PatSimpleF $ fst patSim, ann = snd patSim}
    , pLiteral
    , pOr
    ]

pPatternSimple :: Parser (Surface.PatternSimple SourceSpan)
pPatternSimple =
  pPatternSimpleF pPatternSimple <&> \patSim ->
    Surface.PatternSimple
      { pat = fst patSim
      , ann = snd patSim
      }

pPatternSimpleF ::
  forall pat.
  (HasAnn pat SourceSpan) =>
  Parser (pat SourceSpan) ->
  Parser (Surface.PatternSimpleF pat SourceSpan, SourceSpan)
pPatternSimpleF pPat =
  Megaparsec.choice
    [ pWildcard
    , pTuple pPat
    , Megaparsec.try (pConsApp pPat) <&> \cons ->
        (Surface.PatSimConstructorAppF cons, cons.ann)
    , Megaparsec.try pVar <&> \var -> (Surface.PatSimVarF var, var.ann)
    , Megaparsec.try pQualifiedIdentifier <&> \cons ->
        (Surface.PatSimConstructorF cons, cons.ann)
    ]

pWildcard :: Parser (Surface.PatternSimpleF pat SourceSpan, SourceSpan)
pWildcard = do
  tok <- single (Lexer.Identifier "_")
  pure (Surface.PatSimWildcardF, tok.span)

pLiteral :: Parser (Surface.Pattern SourceSpan)
pLiteral = do
  (lit, ann) <- literal
  pure Surface.Pattern{pat = Surface.PatLiteralF lit, ann}

pVar :: Parser (Surface.PatternVarF SourceSpan)
pVar = Megaparsec.label "pattern variable" do
  ref <- Megaparsec.optional (single (Lexer.Keyword Lexer.Ref))
  mut <- Megaparsec.optional (single (Lexer.Keyword Lexer.Mut))
  name <- pIdentifier
  Megaparsec.notFollowedBy (single (Lexer.Punctuation Lexer.ColonColon))
  pure
    Surface.PatternVarF
      { ref = (.span) <$> ref
      , mut = (.span) <$> mut
      , name = name
      , ann =
          SourceSpan
            { start = case mut of
                Just mut' -> mut'.span.start
                Nothing -> name.ann.start
            , end = name.ann.end
            }
      }

pTuple ::
  (HasAnn pat SourceSpan) =>
  Parser (pat SourceSpan) ->
  Parser (Surface.PatternSimpleF pat SourceSpan, SourceSpan)
pTuple p = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  items <- sepEndBy1 p (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure
    ( Surface.PatSimTupleF (NonEmptyVector.fromNonEmpty items)
    , SourceSpan{start = tokS.span.start, end = tokE.span.end}
    )

pConsApp ::
  forall pat.
  (HasAnn pat SourceSpan) =>
  Parser (pat SourceSpan) ->
  Parser (Surface.PatternConsturctorAppF pat SourceSpan)
pConsApp pPat = do
  consName <- pQualifiedIdentifier
  typeParams <- Megaparsec.optional pBindersWoConstraints
  (fields, fieldsAnn) <- pFields
  pure
    Surface.PatternConsturctorAppF
      { name = consName
      , typeParams
      , fields
      , fieldsAnn
      , ann = Lexer.SourceSpan{start = consName.ann.start, end = fieldsAnn.end}
      }
 where
  pFields = do
    Megaparsec.choice
      [ Bifunctor.first Surface.PatFldsUnnamedF <$> pFieldsUnnamed
      , Bifunctor.first Surface.PatFldsNamedF <$> pFieldsNamed
      ]

  pFieldsUnnamed = do
    tokS <- single (Lexer.Punctuation Lexer.LeftParen)
    fields <- sepEndBy1 pFieldUnnamed (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightParen)
    pure
      ( NonEmptyVector.fromNonEmpty fields
      , SourceSpan{start = tokS.span.start, end = tokE.span.end}
      )

  pFieldUnnamed = do
    value <- pPat
    optional <- Megaparsec.optional (single (Lexer.Punctuation Lexer.Question))
    pure
      Surface.PatternFieldUnnamedF
        { value
        , optional = (.span) <$> optional
        , ann =
            SourceSpan
              { start = value.ann.start
              , end = case optional of
                  Just optional' -> optional'.span.end
                  Nothing -> value.ann.end
              }
        }

  pFieldsNamed = do
    tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
    fields <- sepEndBy1 pFieldNamed (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure
      ( NonEmptyVector.fromNonEmpty fields
      , SourceSpan
          { start = tokS.span.start
          , end = tokE.span.end
          }
      )

  pFieldNamed :: Parser (Surface.PatternFieldNamedF pat SourceSpan)
  pFieldNamed =
    Megaparsec.choice
      [ Surface.PatFldNmdValueF <$> Megaparsec.try pFieldNamedValue
      , Surface.PatFldNmdPunningF <$> pFieldNamedPunning
      ]

  pFieldNamedValue :: Parser (Surface.PatternFieldNamedValueF pat SourceSpan)
  pFieldNamedValue = do
    name <- pIdentifier
    _ <- single (Lexer.Punctuation Lexer.Assign)
    value <- pPat
    pure
      Surface.PatternFieldNamedValueF
        { name
        , value
        , ann = SourceSpan{start = name.ann.start, end = value.ann.end}
        }

  pFieldNamedPunning :: Parser (Surface.PatternFieldNamedPunningF pat SourceSpan)
  pFieldNamedPunning = do
    ref <- Megaparsec.optional (single (Lexer.Keyword Lexer.Ref))
    mut <- Megaparsec.optional (single (Lexer.Keyword Lexer.Mut))
    name <- pIdentifier
    optional <- Megaparsec.optional $ single (Lexer.Punctuation Lexer.Question)
    pure
      Surface.PatternFieldNamedPunningF
        { ref = (.span) <$> ref
        , mut = (.span) <$> mut
        , name
        , optional = (.span) <$> optional
        , ann =
            SourceSpan
              { start = name.ann.start
              , end = case optional of
                  Just optional' -> optional'.span.end
                  Nothing -> name.ann.end
              }
        }

pOr :: Parser (Surface.Pattern SourceSpan)
pOr = do
  items <-
    NonEmptyVector.fromNonEmpty
      <$> sepEndBy1
        (pPatternSimpleF pPattern)
        (single (Lexer.Punctuation Lexer.Pipe))
  pure
    Surface.Pattern
      { pat = Surface.PatOrF (fmap fst items)
      , ann =
          SourceSpan
            { start = (snd $ NonEmptyVector.head items).start
            , end = (snd $ NonEmptyVector.last items).end
            }
      }
