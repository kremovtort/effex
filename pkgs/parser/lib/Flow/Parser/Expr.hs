{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Flow.Parser.Expr where

import "base" Data.List.NonEmpty (NonEmpty ((:|)))
import "base" Data.Maybe (fromJust)
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface (
  Expression (..),
  Pattern (..),
  PatternSimple (..),
  Statement (..),
  Type (..),
 )
import Flow.AST.Surface.Callable qualified as Surface
import Flow.AST.Surface.Common (Identifier (..))
import Flow.AST.Surface.Constraint (
  BindersF (..),
  QualifiedIdentifierF (..),
 )
import Flow.AST.Surface.Expr (
  AllocF (..),
  AppArgsF (..),
  AppF (..),
  ArgNamedF (..),
  EffectItemDefinitionF (..),
  ExpressionF (..),
  HandleBodyF (..),
  HandleExpressionF (..),
  HandleReturningBlockF (..),
  HandleReturningF (..),
  LambdaArgF (..),
  LambdaF (..),
  LambdaFullF (..),
  LambdaShortF (..),
 )
import Flow.AST.Surface.Literal (Literal)
import Flow.AST.Surface.Syntax (
  CodeBlockF (..),
 )
import Flow.AST.Surface.Syntax qualified as Syntax
import Flow.AST.Surface.With (WithAppF (..), WithBlockF (..))
import Flow.Lexer qualified as Lexer
import Flow.Parser.Callable (pOpDefinition, pOpInfixDefinition)
import Flow.Parser.Common (
  HasAnn,
  Parser,
  SourceSpan (..),
  WithPos (..),
  pIdentifier,
  pRegionIdentifier,
  single,
 )
import Flow.Parser.Constraint (
  pBindersAppValueLevel,
  pBindersWoConstraints,
  pQualifiedIdentifier,
  pWhereBlockHead,
 )
import Flow.Parser.Literal (literal)
import Flow.Parser.Operators (pOperators)
import Flow.Parser.Syntax (pCodeBlock, pIfExpression, pLetDefinition, pLoopExpression, pMatchExpression)
import Flow.Parser.Type (pFnEffectsResult)
import Flow.Parser.Use (pUseClause)
import Flow.Parser.With (pWithApp, pWithBlock)

pWildcard :: Parser SourceSpan
pWildcard = do
  tok <- single (Lexer.Identifier "_")
  pure tok.span

pLiteral :: Parser (Literal, SourceSpan)
pLiteral = literal

pParens ::
  Parser (expr Lexer.SourceSpan) ->
  Parser (expr Lexer.SourceSpan, Lexer.SourceSpan)
pParens expr = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  expr' <- expr
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure (expr', SourceSpan{start = tokS.span.start, end = tokE.span.end})

pIdent ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (QualifiedIdentifierF ty Lexer.SourceSpan)
pIdent = pQualifiedIdentifier

pOfTypeSuffix ::
  Parser (ty SourceSpan) ->
  expr SourceSpan ->
  Parser (expr SourceSpan, ty SourceSpan)
pOfTypeSuffix pTy expr = Megaparsec.label "of type" do
  _ <- single (Lexer.Punctuation Lexer.Colon)
  ty' <- pTy
  pure (expr, ty')

pConstructor ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (QualifiedIdentifierF ty SourceSpan)
pConstructor = pQualifiedIdentifier

pIndexSuffix ::
  Parser (expr SourceSpan) ->
  expr SourceSpan ->
  Parser (expr SourceSpan, expr SourceSpan, SourceSpan)
pIndexSuffix pExpr expr = do
  tokS <- single (Lexer.Punctuation Lexer.LeftBracket)
  idxExpr <- pExpr
  tokE <- single (Lexer.Punctuation Lexer.RightBracket)
  pure
    ( expr
    , idxExpr
    , SourceSpan
        { start = tokS.span.start
        , end = tokE.span.end
        }
    )

pDotAccessSuffix ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  expr SourceSpan ->
  Parser (expr SourceSpan, QualifiedIdentifierF ty SourceSpan, SourceSpan)
pDotAccessSuffix pTy expr = do
  tokS <- single (Lexer.Punctuation Lexer.Dot)
  field <- pQualifiedIdentifier pTy
  pure (expr, field, SourceSpan{start = tokS.span.start, end = field.ann.end})

pAppSuffix ::
  (HasAnn ty SourceSpan, HasAnn expr SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  expr SourceSpan ->
  Parser (AppF ty expr SourceSpan)
pAppSuffix pTy pExpr expr = Megaparsec.label "app suffix" do
  typeParams <- Megaparsec.optional (pBindersAppValueLevel pTy)
  (args, end) <-
    Megaparsec.choice
      [ pUnnamedArgs
      , pNamedArgs
      ]
  withApp <- Megaparsec.optional (pWithApp pTy pExpr)
  let ann =
        SourceSpan
          { start = expr.ann.start
          , end = case withApp of
              Just withApp' -> withApp'.ann.end
              Nothing -> end
          }
  pure
    AppF
      { callee = expr
      , typeParams
      , args
      , with = withApp
      , ann
      }
 where
  pUnnamedArgs =
    Megaparsec.choice
      [ do
          _ <- single (Lexer.Punctuation Lexer.LeftParen)
          args <- Megaparsec.sepEndBy pExpr (single (Lexer.Punctuation Lexer.Comma))
          tokE <- single (Lexer.Punctuation Lexer.RightParen)
          pure (AppArgsUnnamedF (Vector.fromList args), tokE.span.end)
      , do
          tokE <- single (Lexer.Punctuation Lexer.LeftRightParen)
          pure (AppArgsUnnamedF Vector.empty, tokE.span.end)
      ]

  pNamedArgs = do
    _ <- single (Lexer.Punctuation Lexer.LeftBrace)
    args <- Megaparsec.sepEndBy pNamedArg (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure (AppArgsNamedF (Vector.fromList args), tokE.span.end)

  pNamedArg = do
    name <- pIdentifier
    value <- Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.Assign)
      pExpr
    pure $
      ArgNamedF
        { name = name
        , value = value
        , ann =
            SourceSpan
              { start = name.ann.start
              , end = case value of
                  Just value' -> value'.ann.end
                  Nothing -> name.ann.end
              }
        }

pTuple ::
  Parser (Expression SourceSpan) ->
  Parser (Expression SourceSpan)
pTuple pExpr = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  expr1 <- pExpr
  _ <- single (Lexer.Punctuation Lexer.Comma)
  restExprs <- Megaparsec.sepEndBy1 pExpr (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure
    Expression
      { expr = ETupleF expr1 (fromJust $ NonEmptyVector.fromList restExprs)
      , ann = SourceSpan{start = tokS.span.start, end = tokE.span.end}
      }

pAlloc ::
  (HasAnn stmt SourceSpan, HasAnn expr SourceSpan) =>
  Parser (stmt SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (AllocF stmt expr SourceSpan)
pAlloc pStmt pExpr = do
  tokS <- single (Lexer.Keyword Lexer.Alloc)
  into <- Megaparsec.optional pRegionIdentifier
  body <- pCodeBlock pStmt pExpr
  pure $
    AllocF
      { into = into
      , body = body
      , ann = SourceSpan{start = tokS.span.start, end = body.ann.end}
      }

pLambdaShort ::
  (HasAnn ty SourceSpan, HasAnn expr SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (LambdaF stmt ty expr SourceSpan, SourceSpan)
pLambdaShort pTy pExpr = do
  (argsOpen, args) <-
    Megaparsec.choice
      [ do
          argsOpen <- single (Lexer.Punctuation Lexer.Pipe)
          args <- Megaparsec.sepBy (pLambdaArg pTy) (single (Lexer.Punctuation Lexer.Comma))
          _ <- single (Lexer.Punctuation Lexer.Pipe)
          pure (argsOpen, args)
      , do
          argsOpenClose <- single (Lexer.Punctuation Lexer.Or)
          pure (argsOpenClose, [])
      ]
  body <- pExpr
  pure
    ( LamShortF
        LambdaShortF
          { args = Vector.fromList args
          , body = body
          , ann = SourceSpan{start = argsOpen.span.start, end = body.ann.end}
          }
    , SourceSpan{start = argsOpen.span.start, end = body.ann.end}
    )

pLambdaFull ::
  (HasAnn stmt SourceSpan, HasAnn ty SourceSpan, HasAnn expr SourceSpan) =>
  Parser (stmt SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (LambdaF stmt ty expr SourceSpan, SourceSpan)
pLambdaFull pStmt pTy pExpr = do
  mBinders <- Megaparsec.optional (pBindersWoConstraints pTy)
  argsOpen <- single (Lexer.Punctuation Lexer.Pipe)
  args <- Megaparsec.sepBy (pLambdaArg pTy) (single (Lexer.Punctuation Lexer.Comma))
  _ <- single (Lexer.Punctuation Lexer.Pipe)
  effectsResult <- Megaparsec.optional (pFnEffectsResult pTy)
  whereBlock <- Megaparsec.optional (pWhereBlockHead pTy)
  body <- pCodeBlock pStmt pExpr
  let ann =
        SourceSpan
          { start = case mBinders of
              Just binders -> binders.ann.start
              Nothing -> argsOpen.span.start
          , end = body.ann.end
          }
  pure
    ( LamFullF
        LambdaFullF
          { typeParams = mBinders
          , args = Vector.fromList args
          , effectsResult = effectsResult
          , whereBlock = whereBlock
          , body = body
          , ann = ann
          }
    , ann
    )

pLambdaArg ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (LambdaArgF ty SourceSpan)
pLambdaArg pTy = do
  mut <- Megaparsec.optional (single (Lexer.Keyword Lexer.Mut))
  name <- pIdentifier
  type_ <- Megaparsec.optional do
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pTy
  pure $
    LambdaArgF
      { mut = fmap (.span) mut
      , name
      , type_
      , ann =
          SourceSpan
            { start = case mut of
                Just mut' -> mut'.span.start
                Nothing -> name.ann.start
            , end = case type_ of
                Just ty -> ty.ann.end
                Nothing -> name.ann.end
            }
      }

pHandleExpression ::
  ( HasAnn ty SourceSpan
  , HasAnn expr SourceSpan
  , HasAnn stmt SourceSpan
  , HasAnn simPat SourceSpan
  ) =>
  Parser (stmt SourceSpan) ->
  Parser (simPat SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (HandleExpressionF stmt simPat ty expr SourceSpan)
pHandleExpression pStmt pSimPat pTy pExpr = do
  tokS <- single (Lexer.Keyword Lexer.Handle)
  effects <- Megaparsec.sepEndBy1 pTy (single (Lexer.Punctuation Lexer.Comma))
  in_ <- Megaparsec.optional do
    _ <- single (Lexer.Keyword Lexer.In)
    pTy
  returning <- Megaparsec.optional do
    returningTok <- single (Lexer.Keyword Lexer.Returning)
    _ <- single (Lexer.Punctuation Lexer.LessThan)
    binder <- pIdentifier
    _ <- single (Lexer.Punctuation Lexer.GreaterThan)
    result <- pTy
    pure $
      HandleReturningF
        { binder = binder
        , result = result
        , ann = SourceSpan{start = returningTok.span.start, end = result.ann.end}
        }
  body <- pHandleBody
  pure
    HandleExpressionF
      { effects = fromJust $ NonEmptyVector.fromList effects
      , in_ = in_
      , returning = returning
      , body = body
      , ann = SourceSpan{start = tokS.span.start, end = body.ann.end}
      }
 where
  pHandleBody = do
    tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
    uses <- Megaparsec.many pUseClause
    item <- pEffectItemDefinition
    items <- Megaparsec.many pEffectItemDefinition
    returning <- Megaparsec.optional pHandleReturningBlock
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure
      HandleBodyF
        { uses = Vector.fromList uses
        , items = NonEmptyVector.fromNonEmpty (item :| items)
        , returning = returning
        , ann = SourceSpan{start = tokS.span.start, end = tokE.span.end}
        }

  pEffectItemDefinition = do
    Megaparsec.choice
      [ do
          let' <- pLetDefinition pSimPat pTy pExpr
          pure (EDefinitionLetF let', let'.ann)
      , do
          op' <- pOpDefinition pStmt pTy pExpr
          pure (EDefinitionOpF op', op'.ann)
      , do
          opInfix' <- pOpInfixDefinition pStmt pTy pExpr
          pure (EDefinitionOpInfixF opInfix', opInfix'.ann)
      ]

  pHandleReturningBlock = do
    returningTok <- single (Lexer.Keyword Lexer.Returning)
    _ <- single (Lexer.Punctuation Lexer.LeftParen)
    arg <- pIdentifier
    _ <- single (Lexer.Punctuation Lexer.Colon)
    argType <- pIdentifier
    _ <- single (Lexer.Punctuation Lexer.RightParen)
    body <- pCodeBlock pStmt pExpr
    pure $
      HandleReturningBlockF
        { arg = arg
        , argType = argType
        , body = body
        , ann = SourceSpan{start = returningTok.span.start, end = body.ann.end}
        }

pExpression ::
  Parser (Statement SourceSpan) ->
  Parser (PatternSimple SourceSpan) ->
  Parser (Pattern SourceSpan) ->
  Parser (Type SourceSpan) ->
  Parser (Expression SourceSpan) ->
  Parser (Expression SourceSpan)
pExpression pStmt pSimPat pPat pTy pExpr = do
  Megaparsec.choice
    [ pOp'
    , pWithoutOp
    ]
 where
  pWithoutOp =
    Megaparsec.choice
      [ pNotSuffixable
      , do
          head' <- pSuffixable
          pWithSuffix head'
      ]

  pWithSuffix expr =
    Megaparsec.choice
      [ do
          expr' <- Megaparsec.optional $ pContinousSuffix expr
          case expr' of
            Nothing -> pure expr
            Just expr'' -> pWithSuffix expr''
      , do
          expr' <- Megaparsec.optional $ pOfTypeSuffix' expr
          case expr' of
            Nothing -> pure expr
            Just expr'' -> pure expr''
      ]

  pSuffixable =
    Megaparsec.choice
      [ pLiteral'
      , Megaparsec.try pTuple'
      , pParens'
      , pIdent'
      , pMatch'
      , pBlock'
      , pHandle'
      , pAlloc'
      ]

  pContinousSuffix expr =
    Megaparsec.label "expression suffix" $
      Megaparsec.choice
        [ pIndexSuffix' expr
        , pDotAccessSuffix' expr
        , Megaparsec.try $ pAppSuffix' expr
        ]

  pNotSuffixable =
    Megaparsec.choice
      [ pWith'
      , pIf'
      , pLoop'
      , pLambda'
      ]

  pLiteral' = do
    (lit, region) <- pLiteral
    pure $ Expression (ELiteral lit) region

  pParens' = do
    (expr, region) <- pParens pExpr
    pure $ Expression (EParens expr) region

  pIdent' = do
    ident <- pIdent pTy
    pure $ Expression (EIdent ident) ident.ann

  pOp' = pOperators pWithoutOp

  pWith' = do
    with <- pWithBlock pStmt pTy pExpr
    pure $ Expression (EWithBlockF with) with.ann

  pTuple' = pTuple pExpr

  pMatch' = do
    match <- pMatchExpression pPat pExpr
    pure $ Expression (EMatchF match) match.ann

  pIf' = do
    if' <- pIfExpression pStmt pPat pExpr
    pure $ Expression (EIfF if') if'.ann

  pLoop' = do
    loop <- pLoopExpression pStmt pExpr
    pure $ Expression (ELoopF loop) loop.ann

  pBlock' = do
    block <- pCodeBlock pStmt pExpr
    pure $ Expression (EBlockF block) block.ann

  pAlloc' = do
    alloc <- pAlloc pStmt pExpr
    pure $ Expression (EAllocF alloc) alloc.ann

  pLambda' = do
    (lambda, ann) <- Megaparsec.choice [Megaparsec.try (pLambdaShort pTy pExpr), pLambdaFull pStmt pTy pExpr]
    pure $ Expression (ELambdaF lambda) ann

  pHandle' = do
    handle <- pHandleExpression pStmt pSimPat pTy pExpr
    pure $ Expression (EHandleF handle) handle.ann

  pOfTypeSuffix' expr = do
    (expr', ty) <- pOfTypeSuffix pTy expr
    pure $
      Expression
        (EOfType expr' ty)
        SourceSpan{start = expr.ann.start, end = ty.ann.end}

  pIndexSuffix' expr = do
    (expr', exprIdx, ann) <- pIndexSuffix pExpr expr
    pure $ Expression (EIndex expr' exprIdx) ann

  pDotAccessSuffix' expr = do
    (expr', field, ann) <- pDotAccessSuffix pTy expr
    pure $ Expression (EDotAccess expr' field) ann

  pAppSuffix' expr = Megaparsec.label "application" do
    app <- pAppSuffix pTy pExpr expr
    pure $ Expression (EAppF app) app.ann
