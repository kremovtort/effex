{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Effex.Parser.Core where

import "megaparsec" Text.Megaparsec qualified as Megaparsec

import Control.Monad.Combinators.Expr qualified as Expr
import Data.Functor (void, (<&>))
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NonEmptyVector
import Effex.AST.Ann (SourceSpan (..))
import Effex.AST.Surface.Common (
  Identifier (Identifier, ann, name),
  RegionIdentifier (ann),
 )
import Effex.AST.Surface.Common qualified as Surface
import Effex.AST.Surface.Core
import Effex.AST.Surface.Literal (Literal)
import Effex.AST.Surface.Operators (
  BinOp (..),
  UnOp (..),
 )
import Effex.AST.Surface.Pattern (Pattern (ann))
import Effex.AST.Surface.Type (
  BindersF (ann),
  FnEffectsResultF (ann),
  QualifiedIdentifierF (ann),
  Type (ann),
  WhereBlockF (ann),
 )
import Effex.Lexer qualified as Lexer
import Effex.Parser.Common (
  HasAnn,
  Parser,
  WithPos,
  pIdentifier,
  pNonQualifiedIdentifier,
  pPub,
  pRegionIdentifier,
  sepBy1,
  sepEndBy1,
  single,
  token,
 )
import Effex.Parser.Literal (literal)
import Effex.Parser.Pattern (pPattern, pPatternSimple)
import Effex.Parser.Type (
  pBindersAppValueLevel,
  pBindersWoConstraints,
  pFnEffectsResult,
  pKindTreeRoot,
  pQualifiedIdentifier,
  pType,
  pWhereBlockHead,
 )
import Effex.Parser.Use (pUseClause)

pStatement :: Parser (Statement SourceSpan)
pStatement = do
  (stmt, ann) <-
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
  pure Statement{stmt, ann}
 where
  letStatement = do
    let' <- pLetDefinition
    pure
      ( SLetF let'
      , let'.ann
      )

  assignStatement = do
    lhs <- pLHSExpression
    _ <- single (Lexer.Punctuation Lexer.Assign)
    rhs <- pExpression
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
    expr <- pExpression
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
    expr <- pExpression
    _ <- single (Lexer.Punctuation Lexer.Semicolon)
    pure (SExpressionF expr, expr.ann)

  matchStatement = do
    expr <- pMatchExpression
    pure (SMatchF expr, expr.ann)

  ifStatement = do
    expr' <- pIfExpression
    pure (SIfF expr', expr'.ann)

  loopStatement = do
    expr' <- pLoopExpression
    pure (SLoopF expr', expr'.ann)

  whileStatement = do
    expr' <- pWhileStatement
    pure (SWhileF expr', expr'.ann)

  forStatement = do
    expr' <- pForStatement
    pure (SForF expr', expr'.ann)

pLetDefinition :: Parser (LetDefinitionF SourceSpan)
pLetDefinition = do
  letTok <- single (Lexer.Keyword Lexer.Let)
  lhs <- pPatternSimple
  lhsType <- Megaparsec.optional do
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pType
  _ <- single (Lexer.Punctuation Lexer.Assign)
  rhs <- pExpression
  semicolonTok <- single (Lexer.Punctuation Lexer.Semicolon)
  let ann = SourceSpan{start = letTok.span.start, end = semicolonTok.span.end}
  pure
    LetDefinitionF
      { lhs = lhs
      , lhsType = lhsType
      , rhs = rhs
      , ann = ann
      }

pWhileStatement :: Parser (WhileStatementF SourceSpan)
pWhileStatement = do
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
  (condition, _) <- pCondition
  body <- pCodeBlock
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

pForStatement :: Parser (ForStatementF SourceSpan)
pForStatement = do
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
  pattern <- pPatternSimple
  _ <- single (Lexer.Keyword Lexer.In)
  iterable <- pExpression
  body <- pCodeBlock
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

pLHSExpression :: Parser (LHSExpression SourceSpan)
pLHSExpression = do
  base <- pLHSEAtom
  suffixes base
 where
  suffixes acc = do
    mSuffix <- Megaparsec.optional (Megaparsec.choice [pLHSEIndexSuffix, pLHSEDotAccessSuffix])
    case mSuffix of
      Just applySuffix -> suffixes (applySuffix acc)
      Nothing -> pure acc

pLHSEIndexSuffix :: Parser (LHSExpression SourceSpan -> LHSExpression SourceSpan)
pLHSEIndexSuffix = do
  _ <- single (Lexer.Punctuation Lexer.LeftBracket)
  idxExpr <- pExpression
  tokE <- single (Lexer.Punctuation Lexer.RightBracket)
  pure $ \acc ->
    LHSExpression
      { lhs = LHSEIndex acc idxExpr
      , ann = SourceSpan{start = acc.ann.start, end = tokE.span.end}
      }

-- | Parses a dot-access suffix: ".field" and returns a function to extend LHS
pLHSEDotAccessSuffix :: Parser (LHSExpression SourceSpan -> LHSExpression SourceSpan)
pLHSEDotAccessSuffix = do
  _ <- single (Lexer.Punctuation Lexer.Dot)
  field' <- pIdentifier
  pure $ \acc ->
    LHSExpression
      { lhs = LHSEDotAccess acc field'
      , ann = SourceSpan{start = acc.ann.start, end = field'.ann.end}
      }

pLHSEAtom :: Parser (LHSExpression SourceSpan)
pLHSEAtom = do
  Megaparsec.choice
    [ pLHSEWildcard
    , pLHSEVar
    , pLHSEUnOp
    ]

pLHSEWildcard :: Parser (LHSExpression SourceSpan)
pLHSEWildcard = do
  tok <- single (Lexer.Identifier "_")
  pure $ LHSExpression{lhs = LHSEWildcard, ann = tok.span}

pLHSEVar :: Parser (LHSExpression SourceSpan)
pLHSEVar = do
  var <- pIdentifier
  pure $ LHSExpression{lhs = LHSEVar var, ann = var.ann}

pLHSEUnOp :: Parser (LHSExpression SourceSpan)
pLHSEUnOp = do
  tokS <- single (Lexer.Punctuation Lexer.Star)
  expr <- pExpression
  pure $ LHSExpression{lhs = LHSEUnOp (LHSUnOpExpressionDeref expr), ann = SourceSpan{start = tokS.span.start, end = expr.ann.end}}

pExpression :: Parser (Expression SourceSpan)
pExpression = do
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
        , Megaparsec.try $ pFnAppSuffix' expr
        ]

  pNotSuffixable =
    Megaparsec.choice
      [ pWith'
      , pIf'
      , pLoop'
      , pLambda'
      ]

  pLiteral' = do
    (lit, region) <- pLiteralExpression
    pure $ Expression (ELiteral lit) region

  pParens' = do
    (expr, region) <- pParensExpression
    pure $ Expression (EParens expr) region

  pIdent' = do
    ident <- pIdent
    pure $ Expression (EIdent ident) ident.ann

  pOp' = pOperators pWithoutOp

  pWith' = do
    with <- pWithBlock
    pure $ Expression (EWithBlockF with) with.ann

  pTuple' = pTupleExpression

  pMatch' = do
    match <- pMatchExpression
    pure $ Expression (EMatchF match) match.ann

  pIf' = do
    if' <- pIfExpression
    pure $ Expression (EIfF if') if'.ann

  pLoop' = do
    loop <- pLoopExpression
    pure $ Expression (ELoopF loop) loop.ann

  pBlock' = do
    block <- pCodeBlock
    pure $ Expression (EBlockF block) block.ann

  pAlloc' = do
    alloc <- pAlloc
    pure $ Expression (EAllocF alloc) alloc.ann

  pLambda' = do
    (lambda, ann) <- Megaparsec.choice [Megaparsec.try pLambdaShort, pLambdaFull]
    pure $ Expression (ELambdaF lambda) ann

  pHandle' = do
    handle <- pHandleExpression
    pure $ Expression (EHandleF handle) handle.ann

  pOfTypeSuffix' expr = do
    (expr', ty) <- pOfTypeSuffix expr
    pure $
      Expression
        (EOfType expr' ty)
        SourceSpan{start = expr.ann.start, end = ty.ann.end}

  pIndexSuffix' expr = do
    (expr', exprIdx, ann) <- pIndexSuffix expr
    pure $ Expression (EIndex expr' exprIdx) ann

  pDotAccessSuffix' expr = do
    (expr', field, ann) <- pDotAccessSuffix expr
    pure $ Expression (EDotAccess expr' field) ann

  pFnAppSuffix' expr = Megaparsec.label "application" do
    app <- pFnAppSuffix expr
    pure $ Expression (EAppF app) app.ann

pLiteralExpression :: Parser (Literal, SourceSpan)
pLiteralExpression = literal

pParensExpression :: Parser (Expression Lexer.SourceSpan, Lexer.SourceSpan)
pParensExpression = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  expr' <- pExpression
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure (expr', SourceSpan{start = tokS.span.start, end = tokE.span.end})

pIdent :: Parser (QualifiedIdentifierF Lexer.SourceSpan)
pIdent = pQualifiedIdentifier

pOfTypeSuffix ::
  Expression SourceSpan ->
  Parser (Expression SourceSpan, Type SourceSpan)
pOfTypeSuffix expr = Megaparsec.label "of type" do
  _ <- single (Lexer.Punctuation Lexer.Colon)
  ty' <- pType
  pure (expr, ty')

pConstructor :: Parser (QualifiedIdentifierF SourceSpan)
pConstructor = pQualifiedIdentifier

pIndexSuffix ::
  Expression SourceSpan ->
  Parser (Expression SourceSpan, Expression SourceSpan, SourceSpan)
pIndexSuffix expr = do
  tokS <- single (Lexer.Punctuation Lexer.LeftBracket)
  idxExpr <- pExpression
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
  Expression SourceSpan ->
  Parser (Expression SourceSpan, QualifiedIdentifierF SourceSpan, SourceSpan)
pDotAccessSuffix expr = do
  tokS <- single (Lexer.Punctuation Lexer.Dot)
  field <- pQualifiedIdentifier
  pure (expr, field, SourceSpan{start = tokS.span.start, end = field.ann.end})

pFnAppSuffix :: Expression SourceSpan -> Parser (FnAppF SourceSpan)
pFnAppSuffix expr = Megaparsec.label "app suffix" do
  typeParams <- Megaparsec.optional pBindersAppValueLevel
  (args, end) <-
    Megaparsec.choice
      [ pUnnamedArgs
      , pNamedArgs
      ]
  withApp <- Megaparsec.optional pWithApp
  let ann =
        SourceSpan
          { start = expr.ann.start
          , end = case withApp of
              Just withApp' -> withApp'.ann.end
              Nothing -> end
          }
  pure
    FnAppF
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
          args <- Megaparsec.sepEndBy pExpression (single (Lexer.Punctuation Lexer.Comma))
          tokE <- single (Lexer.Punctuation Lexer.RightParen)
          pure (FnAppArgsUnnamedF (Vector.fromList args), tokE.span.end)
      , do
          tokE <- single (Lexer.Punctuation Lexer.LeftRightParen)
          pure (FnAppArgsUnnamedF Vector.empty, tokE.span.end)
      ]

  pNamedArgs = do
    _ <- single (Lexer.Punctuation Lexer.LeftBrace)
    args <- Megaparsec.sepEndBy pNamedArg (single (Lexer.Punctuation Lexer.Comma))
    tokE <- single (Lexer.Punctuation Lexer.RightBrace)
    pure (FnAppArgsNamedF (Vector.fromList args), tokE.span.end)

  pNamedArg = do
    name <- pIdentifier
    value <- Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.Assign)
      pExpression
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

pTupleExpression :: Parser (Expression SourceSpan)
pTupleExpression = do
  tokS <- single (Lexer.Punctuation Lexer.LeftParen)
  expr1 <- pExpression
  _ <- single (Lexer.Punctuation Lexer.Comma)
  restExprs <- sepEndBy1 pExpression (single (Lexer.Punctuation Lexer.Comma))
  tokE <- single (Lexer.Punctuation Lexer.RightParen)
  pure
    Expression
      { expr = ETupleF expr1 (NonEmptyVector.fromNonEmpty restExprs)
      , ann = SourceSpan{start = tokS.span.start, end = tokE.span.end}
      }

pAlloc :: Parser (AllocF SourceSpan)
pAlloc = do
  tokS <- single (Lexer.Keyword Lexer.Alloc)
  into <- Megaparsec.optional pRegionIdentifier
  body <- pCodeBlock
  pure $
    AllocF
      { into = into
      , body = body
      , ann = SourceSpan{start = tokS.span.start, end = body.ann.end}
      }

pLambdaShort :: Parser (LambdaF SourceSpan, SourceSpan)
pLambdaShort = do
  (argsOpen, args) <-
    Megaparsec.choice
      [ do
          argsOpen <- single (Lexer.Punctuation Lexer.Pipe)
          args <- Megaparsec.sepBy pLambdaArg (single (Lexer.Punctuation Lexer.Comma))
          _ <- single (Lexer.Punctuation Lexer.Pipe)
          pure (argsOpen, args)
      , do
          argsOpenClose <- single (Lexer.Punctuation Lexer.Or)
          pure (argsOpenClose, [])
      ]
  body <- pExpression
  pure
    ( LamShortF
        LambdaShortF
          { args = Vector.fromList args
          , body = body
          , ann = SourceSpan{start = argsOpen.span.start, end = body.ann.end}
          }
    , SourceSpan{start = argsOpen.span.start, end = body.ann.end}
    )

pLambdaFull :: Parser (LambdaF SourceSpan, SourceSpan)
pLambdaFull = do
  mBinders <- Megaparsec.optional pBindersWoConstraints
  argsOpen <- single (Lexer.Punctuation Lexer.Pipe)
  args <- Megaparsec.sepBy pLambdaArg (single (Lexer.Punctuation Lexer.Comma))
  _ <- single (Lexer.Punctuation Lexer.Pipe)
  effectsResult <- Megaparsec.optional pFnEffectsResult
  whereBlock <- Megaparsec.optional pWhereBlockHead
  body <- pCodeBlock
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
  Parser (LambdaArgF SourceSpan)
pLambdaArg = do
  mut <- Megaparsec.optional (single (Lexer.Keyword Lexer.Mut))
  name <- pIdentifier
  type_ <- Megaparsec.optional do
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pType
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

pHandleExpression :: Parser (HandleExpressionF SourceSpan)
pHandleExpression = do
  tokS <- single (Lexer.Keyword Lexer.Handle)
  effects <- sepEndBy1 pType (single (Lexer.Punctuation Lexer.Comma))
  in_ <- Megaparsec.optional do
    _ <- single (Lexer.Keyword Lexer.In)
    pType
  returning <- Megaparsec.optional do
    returningTok <- single (Lexer.Keyword Lexer.Returning)
    _ <- single (Lexer.Punctuation Lexer.LessThan)
    binder <- pIdentifier
    _ <- single (Lexer.Punctuation Lexer.GreaterThan)
    result <- pType
    pure $
      HandleReturningF
        { binder = binder
        , result = result
        , ann = SourceSpan{start = returningTok.span.start, end = result.ann.end}
        }
  body <- pHandleBody
  pure
    HandleExpressionF
      { effects = NonEmptyVector.fromNonEmpty effects
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
        , items = NonEmptyVector.fromNonEmpty (item List.NonEmpty.:| items)
        , returning = returning
        , ann = SourceSpan{start = tokS.span.start, end = tokE.span.end}
        }

  pEffectItemDefinition = do
    Megaparsec.choice
      [ do
          let' <- pLetDefinition
          pure (EDefinitionLetF let', let'.ann)
      , do
          op' <- pOpDefinition
          pure (EDefinitionOpF op', op'.ann)
      , do
          opInfix' <- pOpInfixDefinition
          pure (EDefinitionOpInfixF opInfix', opInfix'.ann)
      ]

  pHandleReturningBlock = do
    returningTok <- single (Lexer.Keyword Lexer.Returning)
    _ <- single (Lexer.Punctuation Lexer.LeftParen)
    arg <- pIdentifier
    _ <- single (Lexer.Punctuation Lexer.Colon)
    argType <- pIdentifier
    _ <- single (Lexer.Punctuation Lexer.RightParen)
    body <- pCodeBlock
    pure $
      HandleReturningBlockF
        { arg = arg
        , argType = argType
        , body = body
        , ann = SourceSpan{start = returningTok.span.start, end = body.ann.end}
        }

pCondition ::
  Parser (ConditionF SourceSpan, SourceSpan)
pCondition =
  Megaparsec.choice
    [ pCondLet
    , pCondBool
    ]
 where
  pCondLet = do
    tokS <- single (Lexer.Keyword Lexer.Let)
    pat <- pPattern
    _ <- single (Lexer.Punctuation Lexer.Assign)
    expr <- pExpression
    bool' <- Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.Semicolon)
      pExpression
    let ann = case bool' of
          Nothing -> SourceSpan tokS.span.start expr.ann.end
          Just b -> SourceSpan tokS.span.start b.ann.end
    pure
      ( CondLetF
          LetConditionF
            { pattern = pat
            , patternExpr = expr
            , bool = bool'
            , ann
            }
      , ann
      )

  pCondBool = pExpression <&> \expr -> (CondBoolF expr, expr.ann)

pCodeBlock ::
  Parser (CodeBlockF SourceSpan)
pCodeBlock = do
  tokS <- single (Lexer.Punctuation Lexer.LeftBrace)
  region <- Megaparsec.optional do
    region <- pRegionIdentifier
    _ <- single (Lexer.Punctuation Lexer.FatArrow)
    pure region
  uses <- Megaparsec.many pUseClause
  statements <- Megaparsec.many pStatement
  result <- Megaparsec.optional pExpression
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

pWithApp :: Parser (FnWithAppF SourceSpan)
pWithApp = Megaparsec.label "with in function call" do
  tokS <- single (Lexer.Keyword Lexer.With)
  (clauses, end) <- pWithAppClauses
  pure $
    FnWithAppF
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
    lhs <- pEffLabelTy
    _ <- single (Lexer.Punctuation Lexer.Assign)
    rhs <- pEffLabelTy
    pure $ WthApClauseAssignF lhs rhs

  pWithAppClauseHandle = do
    lhs <- sepEndBy1 pEffLabelTy (single (Lexer.Punctuation Lexer.Comma))
    _ <- single (Lexer.Punctuation Lexer.LeftArrow)
    rhs <- pEffHandleRhs
    pure $ WthApClauseHandleF (NonEmptyVector.fromNonEmpty lhs) rhs

pWithBlock :: Parser (WithBlockF SourceSpan)
pWithBlock = do
  tokS <- single (Lexer.Keyword Lexer.With)
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  statements <-
    sepEndBy1
      pWithStatement
      (single (Lexer.Punctuation Lexer.Semicolon))
  _ <- single (Lexer.Punctuation Lexer.RightBrace)
  _ <- single (Lexer.Keyword Lexer.In)
  block <- pCodeBlock
  pure
    WithBlockF
      { withStatements = NonEmptyVector.fromNonEmpty $ fmap fst statements
      , block
      , ann = SourceSpan{start = tokS.span.start, end = block.ann.end}
      }

pWithStatement :: Parser (WithStatementF SourceSpan, SourceSpan)
pWithStatement = do
  Megaparsec.choice
    [ Megaparsec.try pWithStatementLetLabelledHandle
    , Megaparsec.try pWithStatementLetAssign
    , pWithStatementLetHandle
    , pWithStatementLabelledHandle
    ]
 where
  pWithStatementLetHandle = do
    let' <- single (Lexer.Keyword Lexer.LetBang)
    handle <- pEffHandleRhs
    pure (WthStmtLetHandleF handle, SourceSpan{start = let'.span.start, end = handle.ann.end})

  pWithStatementLetLabelledHandle = do
    let' <- single (Lexer.Keyword Lexer.LetBang)
    labels <- sepEndBy1 pEffLabelTy (single (Lexer.Punctuation Lexer.Comma))
    let labels' = NonEmptyVector.fromNonEmpty labels
    _ <- single (Lexer.Punctuation Lexer.LeftArrow)
    handle <- pEffHandleRhs
    pure
      ( WthStmtLetLabelledHandleF labels' handle
      , SourceSpan
          { start = let'.span.start
          , end = handle.ann.end
          }
      )

  pWithStatementLetAssign = do
    let' <- single (Lexer.Keyword Lexer.LetBang)
    label <- pEffLabelTy
    _ <- single (Lexer.Punctuation Lexer.Assign)
    label' <- pEffLabelTy
    pure (WthStmtLetAssignF label label', SourceSpan{start = let'.span.start, end = label'.ann.end})

  pWithStatementLabelledHandle = do
    labels <- sepEndBy1 pEffLabelTy (single (Lexer.Punctuation Lexer.Comma))
    _ <- single (Lexer.Punctuation Lexer.LeftArrow)
    handle <- pEffHandleRhs
    pure
      ( WthStmtLabelledHandleF (NonEmptyVector.fromNonEmpty labels) handle
      , SourceSpan
          { start = (List.NonEmpty.head labels).ann.start
          , end = handle.ann.end
          }
      )

pEffLabelTy ::
  Parser (EffLabelTyF SourceSpan)
pEffLabelTy = Megaparsec.try do
  name <- pNonQualifiedIdentifier
  ty <- Megaparsec.optional do
    _ <- single (Lexer.Punctuation Lexer.Colon)
    pType
  let ann =
        SourceSpan
          { start = name.ann.start
          , end = case ty of
              Nothing -> name.ann.end
              Just ty' -> ty'.ann.end
          }
  pure $ EffLabelTyF name ty ann

pEffHandleRhs :: Parser (EffHandleRhsF SourceSpan)
pEffHandleRhs = do
  expr' <- pExpression
  in_ <- Megaparsec.optional do
    _ <- single (Lexer.Keyword Lexer.In)
    _ <- single (Lexer.Punctuation Lexer.LeftBrace)
    statements <-
      sepEndBy1
        pInStatement
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

pInStatement :: Parser (InStatementF SourceSpan, SourceSpan)
pInStatement =
  Megaparsec.choice
    [ pInStatementAssign
    , pInStatementHandle
    ]
 where
  pInStatementAssign = do
    ty <- pType
    _ <- single (Lexer.Punctuation Lexer.Assign)
    label <- pEffLabelTy
    pure (InStatementAssignF ty label, SourceSpan{start = ty.ann.start, end = label.ann.end})

  pInStatementHandle = do
    tys <- sepEndBy1 pType (single (Lexer.Punctuation Lexer.Comma))
    _ <- single (Lexer.Punctuation Lexer.LeftArrow)
    handle <- pEffHandleRhs
    pure
      ( InStatementHandleF (NonEmptyVector.fromNonEmpty tys) handle
      , SourceSpan
          { start = (List.NonEmpty.head tys).ann.start
          , end = handle.ann.end
          }
      )

pRecieverHeader :: Parser (ReceiverHeaderF SourceSpan)
pRecieverHeader = do
  typeParams <- Megaparsec.optional pBindersWoConstraints
  name <- pIdentifier
  _ <- single (Lexer.Punctuation Lexer.Colon)
  type_ <- pType
  pure
    ReceiverHeaderF
      { typeParams = typeParams
      , name = name
      , type_ = type_
      , ann =
          SourceSpan
            { start = case typeParams of
                Just typeParams' -> typeParams'.ann.start
                Nothing -> name.ann.start
            , end = type_.ann.end
            }
      }

pCallableHeader ::
  (HasAnn name SourceSpan) =>
  Parser (WithPos ()) -> -- fn | op
  Parser (reciever SourceSpan) ->
  Parser (name SourceSpan) ->
  Parser (CallableHeader reciever name SourceSpan)
pCallableHeader pKind pReciever pName = do
  kindTok <- pKind
  receiver <- pReciever
  name <- pName
  typeParams <- Megaparsec.optional pBindersWoConstraints
  _ <- single (Lexer.Punctuation Lexer.LeftParen)
  args <- Vector.fromList <$> pArgs
  argsEnd <- single (Lexer.Punctuation Lexer.RightParen)
  effectsResult <- Megaparsec.optional pFnEffectsResult
  whereBlock <- Megaparsec.optional pWhereBlockHead
  pure
    CallableHeader
      { receiver = receiver
      , name = name
      , typeParams = typeParams
      , args = args
      , effectsResult = effectsResult
      , whereBlock = whereBlock
      , ann =
          SourceSpan
            { start = kindTok.span.start
            , end = case whereBlock of
                Just whereBlock' -> whereBlock'.ann.end
                Nothing -> case effectsResult of
                  Just effectsResult' -> effectsResult'.ann.end
                  Nothing -> argsEnd.span.end
            }
      }
 where
  pArgs = Megaparsec.sepEndBy1 pArg (single (Lexer.Punctuation Lexer.Comma))

  pArg = do
    mut <- Megaparsec.optional (single (Lexer.Keyword Lexer.Mut))
    name <- pIdentifier
    _ <- single (Lexer.Punctuation Lexer.Colon)
    type_ <- pType
    pure $
      ArgF
        { mut = fmap (.span) mut
        , name = name
        , type_ = type_
        , ann =
            SourceSpan
              { start = case mut of
                  Just mut' -> mut'.span.start
                  Nothing -> name.ann.start
              , end = type_.ann.end
              }
        }

pCallable ::
  forall kind reciever name body.
  (HasAnn name SourceSpan) =>
  Parser (WithPos ()) -> -- fn | op
  Parser (reciever SourceSpan) ->
  Parser (name SourceSpan) ->
  Parser (body SourceSpan, Maybe SourceSpan) ->
  Parser (CallableF kind reciever name body SourceSpan)
pCallable pKind pReciever pName pBody = do
  header <- pCallableHeader pKind pReciever pName
  (body, bodyAnn) <- pBody
  pure
    CallableF
      { header
      , body
      , ann =
          SourceSpan
            { start = header.ann.start
            , end = case bodyAnn of
                Just bodyAnn' -> bodyAnn'.end
                Nothing -> header.ann.end
            }
      }

pFnDeclaration :: Parser (FnDeclarationF SourceSpan)
pFnDeclaration =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Fn))
    (pure Surface.UnitF)
    pIdentifier
    pBodySemicolon

pFnInfixDeclaration :: Parser (FnInfixDeclarationF SourceSpan)
pFnInfixDeclaration =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Fn))
    pRecieverHeader
    pIdentifier
    pBodySemicolon

pFnDefinition :: Parser (FnDefinitionF SourceSpan)
pFnDefinition =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Fn))
    (pure Surface.UnitF)
    pIdentifier
    (pCodeBlock <&> \block -> (block, Just block.ann))

pFnInfixDefinition :: Parser (FnInfixDefinitionF SourceSpan)
pFnInfixDefinition =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Fn))
    pRecieverHeader
    pIdentifier
    (pCodeBlock <&> \block -> (block, Just block.ann))

pOpDeclaration :: Parser (OpDeclarationF SourceSpan)
pOpDeclaration =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Op))
    (pure Surface.UnitF)
    pIdentifier
    pBodySemicolon

pOpInfixDeclaration :: Parser (OpInfixDeclarationF SourceSpan)
pOpInfixDeclaration =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Op))
    pRecieverHeader
    pIdentifier
    pBodySemicolon

pOpDefinition :: Parser (OpDefinitionF SourceSpan)
pOpDefinition =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Op))
    (pure Surface.UnitF)
    pQualifiedIdentifier
    (pCodeBlock <&> \block -> (block, Just block.ann))

pOpInfixDefinition :: Parser (OpInfixDefinitionF SourceSpan)
pOpInfixDefinition =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Op))
    pRecieverHeader
    pQualifiedIdentifier
    (pCodeBlock <&> \block -> (block, Just block.ann))

pBodySemicolon :: Parser (Surface.UnitF ann, Maybe SourceSpan)
pBodySemicolon =
  single (Lexer.Punctuation Lexer.Semicolon) <&> \semicolon ->
    (Surface.UnitF, Just semicolon.span)

pMatchExpression :: Parser (MatchExpressionF SourceSpan)
pMatchExpression = do
  matchTok <- single (Lexer.Keyword Lexer.Match)
  value <- pExpression
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  arms <- sepEndBy1 matchArm (single (Lexer.Punctuation Lexer.Comma))
  rightBraceTok <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann = SourceSpan{start = matchTok.span.start, end = rightBraceTok.span.end}
  pure $
    MatchExpressionF
      { value = value
      , arms = NonEmptyVector.fromNonEmpty arms
      , ann = ann
      }
 where
  matchArm = do
    pattern' <- pPattern
    guard <- Megaparsec.optional do
      _ <- single (Lexer.Keyword Lexer.If)
      pExpression
    _ <- single (Lexer.Punctuation Lexer.FatArrow)
    expr' <- pExpression
    pure $
      MatchArmF
        { pattern = pattern'
        , guard = guard
        , expression = expr'
        , ann = pattern'.ann
        }

pIfExpression :: Parser (IfExpressionF SourceSpan)
pIfExpression = do
  branches' <- sepBy1 pIfBranch (single (Lexer.Keyword Lexer.Else))
  let branches = NonEmptyVector.fromNonEmpty branches'
  else_ <- Megaparsec.optional do
    _ <- single (Lexer.Keyword Lexer.Else)
    pCodeBlock
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
    (condition, conditionAnn) <- pCondition
    result <- pCodeBlock
    pure
      IfBranchF
        { condition
        , result
        , ann = SourceSpan{start = ifTok.span.start, end = conditionAnn.end}
        }

pLoopExpression ::
  Parser (LoopExpressionF SourceSpan)
pLoopExpression = do
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
  body <- pCodeBlock
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

pOperators ::
  Parser (Expression SourceSpan) ->
  Parser (Expression SourceSpan)
pOperators pExpr = Expr.makeExprParser pExpr operators

operators ::
  [[Expr.Operator Parser (Expression SourceSpan)]]
operators =
  [ [deref, not', neg, takeMutRef, takeRef]
  , [mul, divOp, modOp]
  , [add, sub]
  , [concatOp]
  , [shiftLeft, shiftRight]
  , [bitwiseAnd, bitwiseOr]
  , [equal, notEqual, lessThan, lessThanOrEqual, greaterThan, greaterThanOrEqual]
  , [boolAnd]
  , [boolOr]
  ]
 where
  deref =
    Expr.Prefix $
      single (Lexer.Punctuation Lexer.Star)
        <&> \tok -> mkUnOp UnOpDeref tok.span

  not' =
    Expr.Prefix $
      single (Lexer.Punctuation Lexer.Not)
        <&> \tok -> mkUnOp UnOpNot tok.span

  neg =
    Expr.Prefix $
      single (Lexer.Punctuation Lexer.Minus) <&> \tok -> mkUnOp UnOpNeg tok.span

  takeRef = Expr.Prefix $ Megaparsec.try do
    tok <- single (Lexer.Punctuation Lexer.Ampersand)
    mRegion <- Megaparsec.optional pRegionIdentifier
    pure case mRegion of
      Nothing -> mkUnOp (UnOpTakeRef Nothing) tok.span
      Just region ->
        mkUnOp
          (UnOpTakeRef (Just region))
          (SourceSpan tok.span.start region.ann.end)

  takeMutRef = Expr.Prefix $ Megaparsec.try do
    tok <- single (Lexer.Punctuation Lexer.Ampersand)
    mRegion <- Megaparsec.optional pRegionIdentifier
    mutTok <- single (Lexer.Keyword Lexer.Mut)
    let region = SourceSpan tok.span.start mutTok.span.end
    pure $ mkUnOp (UnOpTakeMutRef mRegion) region

  mul = binaryL Lexer.Star BinOpMul
  divOp = binaryL Lexer.Slash BinOpDiv
  modOp = binaryL Lexer.Percent BinOpMod

  add = binaryL Lexer.Plus BinOpAdd
  sub = binaryL Lexer.Minus BinOpSub

  concatOp = binaryL Lexer.Concat BinOpConcat

  shiftLeft = binaryL Lexer.ShiftLeft BinOpBitwiseShiftLeft
  shiftRight = binaryL Lexer.ShiftRight BinOpBitwiseShiftRight

  bitwiseAnd = binaryL Lexer.Ampersand BinOpBitwiseAnd
  bitwiseOr = binaryL Lexer.Pipe BinOpBitwiseOr

  equal = binaryN Lexer.Equal BinOpEqual
  notEqual = binaryN Lexer.NotEqual BinOpNotEqual
  lessThan = binaryN Lexer.LessThan BinOpLessThan
  lessThanOrEqual = binaryN Lexer.LessThanOrEqual BinOpLessThanOrEqual
  greaterThan = binaryN Lexer.GreaterThan BinOpGreaterThan
  greaterThanOrEqual = binaryN Lexer.GreaterThanOrEqual BinOpGreaterThanOrEqual

  boolAnd = binaryR Lexer.And BinOpAnd
  boolOr = binaryR Lexer.Or BinOpOr

  binaryL token' ctor =
    Expr.InfixL $
      single (Lexer.Punctuation token') <&> \tok lhs rhs ->
        mkBinOp ctor tok.span lhs rhs

  binaryR token' ctor =
    Expr.InfixR $
      single (Lexer.Punctuation token') <&> \tok lhs rhs ->
        mkBinOp ctor tok.span lhs rhs

  binaryN token' ctor =
    Expr.InfixN $
      single (Lexer.Punctuation token') <&> \tok lhs rhs ->
        mkBinOp ctor tok.span lhs rhs

  mkBinOp ctor opRegion left right =
    let region =
          SourceSpan
            { start = left.ann.start
            , end = right.ann.end
            }
    in  Expression
          { expr =
              EBinOpF
                BinOpExpression
                  { op = ctor opRegion
                  , left = left
                  , right = right
                  , ann = region
                  }
          , ann = region
          }

  mkUnOp mkOp opAnn operand =
    Expression
      { expr = EUnOpF UnOpExpression{op = mkOp opAnn, operand}
      , ann = SourceSpan opAnn.start operand.ann.end
      }

pTrait :: Parser (TraitF SourceSpan)
pTrait = do
  tokSealed <- Megaparsec.optional (single (Lexer.Keyword Lexer.Sealed))
  tokTrait <- single (Lexer.Keyword Lexer.Trait)
  name <- pIdentifier
  typeParams <- pBindersWoConstraints
  superTraits <-
    fmap NonEmptyVector.fromNonEmpty <$> Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.FatArrow)
      sepEndBy1 pType (single (Lexer.Punctuation Lexer.Comma))
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  traitBody <- Vector.fromList <$> Megaparsec.many pTraitItem
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  pure
    TraitF
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
      TraitItemF
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
      [ withRegion TItemVarTypeDeclarationF <$> pTypeDeclaration
      , withRegion TItemVarLetDeclarationF <$> pLetDeclaration
      , withRegion TItemVarFnDeclarationF <$> Megaparsec.try pFnDeclaration
      , withRegion TItemVarFnInfixDeclarationF <$> Megaparsec.try pFnInfixDeclaration
      , withRegion TItemVarFnDefinitionF <$> Megaparsec.try pFnDefinition
      , withRegion TItemVarFnInfixDefinitionF <$> Megaparsec.try pFnInfixDefinition
      ]

  withRegion f item = (f item, item.ann)

pEffect :: Parser (EffectF SourceSpan)
pEffect = do
  tokSealed <- Megaparsec.optional (single (Lexer.Keyword Lexer.Sealed))
  tokEffect <- single (Lexer.Keyword Lexer.Effect)
  name <- pIdentifier
  typeParams <- Megaparsec.optional pBindersWoConstraints
  superEffects <-
    fmap NonEmptyVector.fromNonEmpty <$> Megaparsec.optional do
      _ <- single (Lexer.Punctuation Lexer.FatArrow)
      sepEndBy1 pType (single (Lexer.Punctuation Lexer.Comma))
  whereBlock <- Megaparsec.optional pWhereBlockHead
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  effectBody <- Vector.fromList <$> Megaparsec.many pEffectItem
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  pure
    EffectF
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
      EffectItemF
        { pub = fmap fst pub
        , item
        , ann = itemAnn
        }

  pEffectItemVariant =
    Megaparsec.choice
      [ withRegion EItemVarTypeDeclarationF <$> pTypeDeclaration
      , withRegion EItemVarLetDeclarationF <$> pLetDeclaration
      , withRegion EItemVarOpDeclarationF <$> Megaparsec.try pOpDeclaration
      , withRegion EItemVarOpInfixDeclarationF <$> Megaparsec.try pOpInfixDeclaration
      , withRegion EItemVarOpDefinitionF <$> Megaparsec.try pOpDefinition
      , withRegion EItemVarOpInfixDefinitionF <$> Megaparsec.try pOpInfixDefinition
      ]

  withRegion f item = (f item, item.ann)

pTypeDeclaration :: Parser (TypeDeclarationF SourceSpan)
pTypeDeclaration = do
  tokS <- single (Lexer.Keyword Lexer.Type)
  name <- pIdentifier
  kindShort <- Megaparsec.optional pKindTreeRoot
  type_ <- Megaparsec.optional pType
  tokE <- single (Lexer.Punctuation Lexer.Semicolon)
  pure
    TypeDeclarationF
      { name
      , kindShort
      , type_
      , ann =
          Lexer.SourceSpan
            { start = tokS.span.start
            , end = tokE.span.end
            }
      }

pLetDeclaration :: Parser (LetDeclarationF SourceSpan)
pLetDeclaration = do
  letTok <- single (Lexer.Keyword Lexer.Let)
  name <- pIdentifier
  _ <- single (Lexer.Punctuation Lexer.Colon)
  type_ <- pType
  tokE <- single (Lexer.Punctuation Lexer.Semicolon)
  pure
    LetDeclarationF
      { name
      , type_
      , ann = Lexer.SourceSpan{start = letTok.span.start, end = tokE.span.end}
      }
