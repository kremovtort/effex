module Flow.Parser.Syntax where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Flow.AST.Surface (
  Expression (..),
  LHSExpression (..),
 )
import Flow.AST.Surface.Common (Identifier (..))
import Flow.AST.Surface.Syntax
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (
  HasAnn,
  Parser,
  SourceSpan (..),
  WithPos (..),
  pRegionIdentifier,
  pIdentifier,
  single,
  token,
 )
import Flow.Parser.Use (pUseClause)

pCodeBlock ::
  (HasAnn stmt SourceSpan, HasAnn expr SourceSpan) =>
  Parser (stmt SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (CodeBlockF stmt expr SourceSpan)
pCodeBlock pStmt pExpr = do
  tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
  region <- Megaparsec.optional do
    region <- pRegionIdentifier
    _ <- single (Lexer.Punctuation Lexer.FatArrow)
    pure region
  uses <- Megaparsec.many pUseClause
  statements <- Megaparsec.many pStmt
  result <- Megaparsec.optional pExpr
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann = SourceSpan{start = tokS.span.start, end = tokE.span.end}
  pure $
    CodeBlockF
      { region
      , uses = Vector.fromList uses
      , statements = Vector.fromList statements
      , result
      , ann
      }

pStatement ::
  ( HasAnn stmt SourceSpan
  , HasAnn lhs SourceSpan
  , HasAnn simPat SourceSpan
  , HasAnn pat SourceSpan
  , HasAnn ty SourceSpan
  , HasAnn expr SourceSpan
  ) =>
  Parser (stmt SourceSpan) ->
  Parser (lhs SourceSpan) ->
  Parser (simPat SourceSpan) ->
  Parser (pat SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (StatementF stmt lhs simPat pat ty expr SourceSpan, SourceSpan)
pStatement pStmt pLhsExpr pSimPat pPat pTy pExpr = do
  Megaparsec.choice
    [ letStatement
    , returnStatement
    , continueStatement
    , breakStatement
    , matchStatement
    , ifStatement
    , loopStatement
    , whileStatement
    , forStatement
    , Megaparsec.try assignStatement
    , Megaparsec.try expressionStatement
    ]
 where
  letStatement = do
    let' <- pLetDefinition pSimPat pTy pExpr
    pure
      ( SLetF let'
      , let'.ann
      )

  assignStatement = do
    lhs <- pLhsExpr
    _ <- single (Lexer.Punctuation Lexer.Assign)
    rhs <- pExpr
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceSpan{start = lhs.ann.start, end = semicolonTok.span.end}
    pure
      ( SAssignF $
          AssignStatementF
            { lhs = lhs
            , rhs = rhs
            , ann = ann
            }
      , ann
      )

  returnStatement = do
    returnTok <- single (Lexer.Keyword Lexer.Return)
    expr <- pExpr
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceSpan{start = returnTok.span.start, end = semicolonTok.span.end}
    pure (SReturnF expr ann, ann)

  continueStatement = do
    continueTok <- single (Lexer.Keyword Lexer.Continue)
    label <- Megaparsec.optional pIdentifier
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceSpan{start = continueTok.span.start, end = semicolonTok.span.end}
    pure (SContinueF label ann, ann)

  breakStatement = do
    breakTok <- single (Lexer.Keyword Lexer.Break)
    label <- Megaparsec.optional pIdentifier
    semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
    let ann = SourceSpan{start = breakTok.span.start, end = semicolonTok.span.end}
    pure (SBreakF label ann, ann)

  expressionStatement = do
    expr <- pExpr
    _ <- single (Lexer.Punctuation Lexer.Semicolon)
    pure (SExpressionF expr, expr.ann)

  matchStatement = do
    expr <- pMatchExpression pPat pExpr
    pure (SMatchF expr, expr.ann)

  ifStatement = do
    expr' <- pIfExpression pStmt pPat pExpr
    pure (SIfF expr', expr'.ann)

  loopStatement = do
    expr' <- pLoopExpression pStmt pExpr
    pure (SLoopF expr', expr'.ann)

  whileStatement = do
    expr' <- pWhileStatement pStmt pPat pExpr
    pure (SWhileF expr', expr'.ann)

  forStatement = do
    expr' <- pForStatement pStmt pSimPat pExpr
    pure (SForF expr', expr'.ann)

pLetDefinition ::
  (HasAnn simPat SourceSpan, HasAnn ty SourceSpan, HasAnn expr SourceSpan) =>
  Parser (simPat SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (LetDefinitionF simPat ty expr SourceSpan)
pLetDefinition pSimPat pTy pExpr = do
  letTok <- single (Lexer.Keyword Lexer.Let)
  lhs <- pSimPat
  lhsType <- Megaparsec.optional do
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pTy
  _ <- single (Lexer.Punctuation Lexer.Assign)
  rhs <- pExpr
  semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
  let ann = SourceSpan{start = letTok.span.start, end = semicolonTok.span.end}
  pure
    LetDefinitionF
      { lhs = lhs
      , lhsType = lhsType
      , rhs = rhs
      , ann = ann
      }

-- | LHSExpression parser (stub)
pLHSExpression :: Parser (Expression SourceSpan) -> Parser (LHSExpression SourceSpan)
pLHSExpression expr = do
  base <- pLHSEAtom expr
  suffixes base
 where
  suffixes acc = do
    mSuffix <- Megaparsec.optional (Megaparsec.choice [pLHSEIndexSuffix expr, pLHSEDotAccessSuffix])
    case mSuffix of
      Just applySuffix -> suffixes (applySuffix acc)
      Nothing -> pure acc

-- | Parses an index suffix: "[expr]" and returns a function to extend LHS
pLHSEIndexSuffix :: Parser (Expression SourceSpan) -> Parser (LHSExpression SourceSpan -> LHSExpression SourceSpan)
pLHSEIndexSuffix expr = do
  _ <- single (Lexer.Punctuation Lexer.LeftBracket)
  idxExpr <- expr
  tokE <- single (Lexer.Punctuation Lexer.RightBracket)
  pure $ \acc ->
    LHSExpression
      { lhsExpression = LHSEIndex acc idxExpr
      , ann = SourceSpan{start = acc.ann.start, end = tokE.span.end}
      }

-- | Parses a dot-access suffix: ".field" and returns a function to extend LHS
pLHSEDotAccessSuffix :: Parser (LHSExpression SourceSpan -> LHSExpression SourceSpan)
pLHSEDotAccessSuffix = do
  _ <- single (Lexer.Punctuation Lexer.Dot)
  field' <- pIdentifier
  pure $ \acc ->
    LHSExpression
      { lhsExpression = LHSEDotAccess acc field'
      , ann = SourceSpan{start = acc.ann.start, end = field'.ann.end}
      }

pLHSEAtom :: Parser (Expression SourceSpan) -> Parser (LHSExpression SourceSpan)
pLHSEAtom expr = do
  Megaparsec.choice
    [ pLHSEWildcard
    , pLHSEVar
    , pLHSEUnOp expr
    ]

pLHSEWildcard :: Parser (LHSExpression SourceSpan)
pLHSEWildcard = do
  tok <- single (Lexer.Identifier "_")
  pure $ LHSExpression{lhsExpression = LHSEWildcard, ann = tok.span}

pLHSEVar :: Parser (LHSExpression SourceSpan)
pLHSEVar = do
  var <- pIdentifier
  pure $ LHSExpression{lhsExpression = LHSEVar var, ann = var.ann}

pLHSEUnOp :: Parser (Expression SourceSpan) -> Parser (LHSExpression SourceSpan)
pLHSEUnOp pExpr = do
  tokS <- single (Lexer.Punctuation Lexer.Star)
  expr <- pExpr
  pure $ LHSExpression{lhsExpression = LHSEUnOp (LHSUnOpExpressionDeref expr), ann = SourceSpan{start = tokS.span.start, end = expr.ann.end}}

pMatchExpression ::
  (HasAnn pat SourceSpan, HasAnn expr SourceSpan) =>
  Parser (pat SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (MatchExpressionF pat expr SourceSpan)
pMatchExpression pPat pExpr = do
  matchTok <- single (Lexer.Keyword Lexer.Match)
  value <- pExpr
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  arms <- Megaparsec.sepEndBy1 matchArm (single (Lexer.Punctuation Lexer.Comma))
  rightBraceTok <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann = SourceSpan{start = matchTok.span.start, end = rightBraceTok.span.end}
  pure $
    MatchExpressionF
      { value = value
      , arms = fromJust $ NonEmptyVector.fromList arms
      , ann = ann
      }
 where
  matchArm = do
    pattern' <- pPat
    guard <- Megaparsec.optional do
      _ <- single (Lexer.Keyword Lexer.If)
      pExpr
    _ <- single (Lexer.Punctuation Lexer.FatArrow)
    expr' <- pExpr
    pure $
      MatchArmF
        { pattern = pattern'
        , guard = guard
        , expression = expr'
        , ann = pattern'.ann
        }

pIfExpression ::
  ( HasAnn stmt SourceSpan
  , HasAnn pat SourceSpan
  , HasAnn expr SourceSpan
  ) =>
  Parser (stmt SourceSpan) ->
  Parser (pat SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (IfExpressionF stmt pat expr SourceSpan)
pIfExpression pStmt pPat pExpr = do
  branches' <- Megaparsec.sepBy1 pIfBranch (single (Lexer.Keyword Lexer.Else))
  let branches = fromJust $ NonEmptyVector.fromList branches'
  else_ <- Megaparsec.optional do
    _ <- single (Lexer.Keyword Lexer.Else)
    pCodeBlock pStmt pExpr
  let ann =
        SourceSpan
          { start = (NonEmptyVector.head branches).ann.start
          , end = case else_ of
              Just else' -> else'.ann.end
              Nothing -> (NonEmptyVector.last branches).ann.end
          }
  pure $
    IfExpressionF
      { branches
      , else_
      , ann
      }
 where
  pIfBranch = do
    ifTok <- single (Lexer.Keyword Lexer.If)
    (condition, conditionAnn) <- pCondition pPat pExpr
    result <- pCodeBlock pStmt pExpr
    pure
      IfBranchF
        { condition
        , result
        , ann = SourceSpan{start = ifTok.span.start, end = conditionAnn.end}
        }

pCondition ::
  (HasAnn pat SourceSpan, HasAnn expr SourceSpan) =>
  Parser (pat SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (ConditionF pat expr SourceSpan, SourceSpan)
pCondition pPat pExpr =
  Megaparsec.choice
    [ pCondLet
    , pCondBool
    ]
 where
  pCondLet = do
    tokS <- single (Lexer.Keyword Lexer.Let)
    pat <- pPat
    _ <- single (Lexer.Punctuation Lexer.Assign)
    expr <- pExpr
    bool <- Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.Semicolon)
      pExpr
    let ann = case bool of
          Nothing -> SourceSpan tokS.span.start expr.ann.end
          Just b -> SourceSpan tokS.span.start b.ann.end
    pure
      ( CondLetF
          LetConditionF
            { pattern = pat
            , patternExpr = expr
            , bool
            , ann
            }
      , ann
      )

  pCondBool = pExpr <&> \expr -> (CondBoolF expr, expr.ann)

pLoopExpression ::
  (HasAnn stmt SourceSpan, HasAnn expr SourceSpan) =>
  Parser (stmt SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (LoopExpressionF stmt expr SourceSpan)
pLoopExpression pStmt pExpr = do
  label <- Megaparsec.optional do
    labelTok <- token
      (Set.singleton $ Megaparsec.Label "label")
      \case
        Lexer.Region s -> Just s
        _ -> Nothing
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pure
      ( Identifier
          { name = labelTok.value
          , ann = labelTok.span
          }
      , SourceSpan
          { start = labelTok.span.start
          , end = labelTok.span.end
          }
      )
  loopTok <- Megaparsec.try $ single (Lexer.Keyword Lexer.Loop)
  body <- pCodeBlock pStmt pExpr
  let ann =
        SourceSpan
          { start = case label of
              Just (_, ann') -> ann'.start
              Nothing -> loopTok.span.start
          , end = body.ann.end
          }
  pure $
    LoopExpressionF
      { label = fmap fst label
      , body
      , ann
      }

pWhileStatement ::
  (HasAnn stmt SourceSpan, HasAnn pat SourceSpan, HasAnn expr SourceSpan) =>
  Parser (stmt SourceSpan) ->
  Parser (pat SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (WhileStatementF stmt pat expr SourceSpan)
pWhileStatement pStmt pPat pExpr = do
  label <- Megaparsec.optional do
    labelTok <- token
      (Set.singleton $ Megaparsec.Label "label")
      \case
        Lexer.Region s -> Just s
        _ -> Nothing
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pure
      ( Identifier labelTok.value labelTok.span
      , SourceSpan
          { start = labelTok.span.start
          , end = labelTok.span.end
          }
      )
  whileTok <- Megaparsec.try $ single (Lexer.Keyword Lexer.While)
  (condition, _) <- pCondition pPat pExpr
  body <- pCodeBlock pStmt pExpr
  let ann =
        SourceSpan
          { start = case label of
              Just (_, ann') -> ann'.start
              Nothing -> whileTok.span.start
          , end = body.ann.end
          }
  pure $
    WhileStatementF
      { label = fmap fst label
      , condition
      , body
      , ann
      }

pForStatement ::
  (HasAnn stmt SourceSpan, HasAnn simPat SourceSpan, HasAnn expr SourceSpan) =>
  Parser (stmt SourceSpan) ->
  Parser (simPat SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (ForStatementF stmt simPat expr SourceSpan)
pForStatement pStmt pSimPat pExpr = do
  label <- Megaparsec.optional do
    labelTok <- token
      (Set.singleton $ Megaparsec.Label "label")
      \case
        Lexer.Region s -> Just s
        _ -> Nothing
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pure
      ( Identifier labelTok.value labelTok.span
      , SourceSpan
          { start = labelTok.span.start
          , end = labelTok.span.end
          }
      )
  forTok <- Megaparsec.try $ single (Lexer.Keyword Lexer.For)
  pattern <- pSimPat
  _ <- single (Lexer.Keyword Lexer.In)
  iterable <- pExpr
  body <- pCodeBlock pStmt pExpr
  let ann =
        SourceSpan
          { start = case label of
              Just (_, ann') -> ann'.start
              Nothing -> forTok.span.start
          , end = body.ann.end
          }
  pure $
    ForStatementF
      { label = label
      , pattern = pattern
      , iterable = iterable
      , body = body
      , ann = ann
      }
