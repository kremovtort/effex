module Flow.Parser.With where

import "base" Data.Maybe (fromJust)
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector

import Data.List.NonEmpty qualified as List.NonEmpty
import Flow.AST.Ann (SourceSpan (..))
import Flow.AST.Surface.Common (Identifier (..))
import Flow.AST.Surface.Syntax (CodeBlockF (..))
import Flow.AST.Surface.With (
  EffHandleRhsF (..),
  EffLabelTyF (..),
  InStatementF (..),
  WithAppClauseF (..),
  WithAppF (..),
  WithBlockF (..),
  WithStatementF (..),
 )
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (HasAnn, Parser, pNonQualifiedIdentifier, sepEndBy1, single)
import Flow.Parser.Syntax (pCodeBlock)

pWithApp ::
  (HasAnn ty SourceSpan, HasAnn expr SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (WithAppF ty expr SourceSpan)
pWithApp pTy pExpr = Megaparsec.label "with in function call" do
  tokS <- single (Lexer.Keyword Lexer.With)
  (clauses, end) <- pWithAppClauses
  pure $
    WithAppF
      { clauses
      , ann = SourceSpan{start = tokS.span.start, end}
      }
 where
  pWithAppClauses = do
    _ <- single (Lexer.Punctuation Lexer.LeftBrace)
    clauses <- sepEndBy1 pWithAppClause (single (Lexer.Punctuation Lexer.Comma))
    rightBrace <- single (Lexer.Punctuation Lexer.RightBrace)
    pure (NonEmptyVector.fromNonEmpty clauses, rightBrace.span.end)

  pWithAppClause =
    Megaparsec.choice
      [ Megaparsec.try pWithAppClauseAssign
      , pWithAppClauseHandle
      ]

  pWithAppClauseAssign = do
    lhs <- pEffLabelTy pTy
    _ <- single (Lexer.Punctuation Lexer.Assign)
    rhs <- pEffLabelTy pTy
    pure $ WthApClauseAssignF lhs rhs

  pWithAppClauseHandle = do
    lhs <- sepEndBy1 (pEffLabelTy pTy) (single (Lexer.Punctuation Lexer.Comma))
    _ <- single (Lexer.Punctuation Lexer.LeftArrow)
    rhs <- pEffHandleRhs pTy pExpr
    pure $ WthApClauseHandleF (NonEmptyVector.fromNonEmpty lhs) rhs

pWithBlock ::
  (HasAnn stmt SourceSpan, HasAnn ty SourceSpan, HasAnn expr SourceSpan) =>
  Parser (stmt SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (WithBlockF stmt ty expr SourceSpan)
pWithBlock pStmt pTy pExpr = do
  tokS <- single (Lexer.Keyword Lexer.With)
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  statements <-
    Megaparsec.sepEndBy1
      (pWithStatement pTy pExpr)
      (single (Lexer.Punctuation Lexer.Semicolon))
  _ <- single (Lexer.Punctuation Lexer.RightBrace)
  _ <- single (Lexer.Keyword Lexer.In)
  block <- pCodeBlock pStmt pExpr
  pure
    WithBlockF
      { withStatements = fromJust $ NonEmptyVector.fromList $ fmap fst statements
      , block
      , ann = SourceSpan{start = tokS.span.start, end = block.ann.end}
      }

pWithStatement ::
  (HasAnn ty SourceSpan, HasAnn expr SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (WithStatementF ty expr SourceSpan, SourceSpan)
pWithStatement pTy pExpr = do
  Megaparsec.choice
    [ Megaparsec.try pWithStatementLetLabelledHandle
    , Megaparsec.try pWithStatementLetAssign
    , pWithStatementLetHandle
    , pWithStatementLabelledHandle
    ]
 where
  pWithStatementLetHandle = do
    let' <- single (Lexer.Keyword Lexer.LetBang)
    handle <- pEffHandleRhs pTy pExpr
    pure (WthStmtLetHandleF handle, SourceSpan{start = let'.span.start, end = handle.ann.end})

  pWithStatementLetLabelledHandle = do
    let' <- single (Lexer.Keyword Lexer.LetBang)
    labels <- Megaparsec.sepEndBy1 (pEffLabelTy pTy) (single (Lexer.Punctuation Lexer.Comma))
    let labels' = fromJust $ NonEmptyVector.fromList labels
    _ <- single (Lexer.Punctuation Lexer.LeftArrow)
    handle <- pEffHandleRhs pTy pExpr
    pure
      ( WthStmtLetLabelledHandleF labels' handle
      , SourceSpan
          { start = let'.span.start
          , end = handle.ann.end
          }
      )

  pWithStatementLetAssign = do
    let' <- single (Lexer.Keyword Lexer.LetBang)
    label <- pEffLabelTy pTy
    _ <- single (Lexer.Punctuation Lexer.Assign)
    label' <- pEffLabelTy pTy
    pure (WthStmtLetAssignF label label', SourceSpan{start = let'.span.start, end = label'.ann.end})

  pWithStatementLabelledHandle = do
    labels <- sepEndBy1 (pEffLabelTy pTy) (single (Lexer.Punctuation Lexer.Comma))
    _ <- single (Lexer.Punctuation Lexer.LeftArrow)
    handle <- pEffHandleRhs pTy pExpr
    pure
      ( WthStmtLabelledHandleF (NonEmptyVector.fromNonEmpty labels) handle
      , SourceSpan
          { start = (List.NonEmpty.head labels).ann.start
          , end = handle.ann.end
          }
      )

pEffLabelTy ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (EffLabelTyF ty SourceSpan)
pEffLabelTy pTy = Megaparsec.try do
  name <- pNonQualifiedIdentifier
  ty <- Megaparsec.optional do
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pTy
  let ann =
        SourceSpan
          { start = name.ann.start
          , end = case ty of
              Nothing -> name.ann.end
              Just ty' -> ty'.ann.end
          }
  pure $ EffLabelTyF name ty ann

pEffHandleRhs ::
  (HasAnn ty SourceSpan, HasAnn expr SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (EffHandleRhsF ty expr SourceSpan)
pEffHandleRhs pTy pExpr = do
  expr' <- pExpr
  in_ <- Megaparsec.optional do
    _ <- single (Lexer.Keyword Lexer.In)
    _ <- single (Lexer.Punctuation Lexer.LeftBrace)
    statements <-
      sepEndBy1
        (pInStatement pTy pExpr)
        (single (Lexer.Punctuation Lexer.Comma))
    _ <- single (Lexer.Punctuation Lexer.RightBrace)
    pure (NonEmptyVector.fromNonEmpty $ fmap fst statements, snd $ List.NonEmpty.last statements)
  pure
    EffHandleRhsF
      { expr = expr'
      , in_ = fmap fst in_
      , ann =
          SourceSpan
            { start = expr'.ann.start
            , end = case in_ of
                Just (_, ann) -> ann.end
                Nothing -> expr'.ann.end
            }
      }

pInStatement ::
  (HasAnn ty SourceSpan, HasAnn expr SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (InStatementF ty expr SourceSpan, SourceSpan)
pInStatement pTy pExpr =
  Megaparsec.choice
    [ pInStatementAssign
    , pInStatementHandle
    ]
 where
  pInStatementAssign = do
    ty <- pTy
    _ <- single (Lexer.Punctuation Lexer.Assign)
    label <- pEffLabelTy pTy
    pure (InStatementAssignF ty label, SourceSpan{start = ty.ann.start, end = label.ann.end})

  pInStatementHandle = do
    tys <- sepEndBy1 pTy (single (Lexer.Punctuation Lexer.Comma))
    _ <- single (Lexer.Punctuation Lexer.LeftArrow)
    handle <- pEffHandleRhs pTy pExpr
    pure
      ( InStatementHandleF (NonEmptyVector.fromNonEmpty tys) handle
      , SourceSpan
          { start = (List.NonEmpty.head tys).ann.start
          , end = handle.ann.end
          }
      )
