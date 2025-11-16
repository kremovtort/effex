{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Flow.Parser.Callable where

import "base" Data.Functor (void, (<&>))
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface.Callable (
  ArgF (..),
  CallableF (..),
  CallableHeader (..),
  FnDeclarationF,
  FnDefinitionF,
  FnInfixDeclarationF,
  FnInfixDefinitionF,
  OpDeclarationF,
  OpDefinitionF,
  OpInfixDeclarationF,
  OpInfixDefinitionF,
  ReceiverHeaderF (..),
 )
import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Syntax qualified as Surface
import Flow.AST.Surface.Type qualified as Surface
import Flow.Lexer (SourceSpan (..), WithPos (..))
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (HasAnn, Parser, pIdentifier, single)
import Flow.Parser.Constraint (
  pQualifiedIdentifier,
  pBindersWoConstraints,
  pWhereBlockHead,
 )
import Flow.Parser.Syntax (pCodeBlock)
import Flow.Parser.Type (pFnEffectsResult)

pRecieverHeader ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (ReceiverHeaderF ty SourceSpan)
pRecieverHeader pTy = do
  typeParams <- Megaparsec.optional (pBindersWoConstraints pTy)
  name <- pIdentifier
  _ <- single (Lexer.Punctuation Lexer.Colon)
  type_ <- pTy
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
  (HasAnn ty SourceSpan, HasAnn name SourceSpan) =>
  Parser (WithPos ()) -> -- fn | op
  Parser (reciever SourceSpan) ->
  Parser (name SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (CallableHeader reciever name ty SourceSpan)
pCallableHeader pKind pReciever pName pTy = do
  kindTok <- pKind
  receiver <- pReciever
  name <- pName
  typeParams <- Megaparsec.optional (pBindersWoConstraints pTy)
  _ <- single (Lexer.Punctuation Lexer.LeftParen)
  args <- Vector.fromList <$> pArgs
  argsEnd <- single (Lexer.Punctuation Lexer.RightParen)
  effectsResult <- Megaparsec.optional (pFnEffectsResult pTy)
  whereBlock <- Megaparsec.optional (pWhereBlockHead pTy)
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
    type_ <- pTy
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
  forall kind reciever name body ty.
  (HasAnn ty SourceSpan, HasAnn name SourceSpan) =>
  Parser (WithPos ()) -> -- fn | op
  Parser (reciever SourceSpan) ->
  Parser (name SourceSpan) ->
  Parser (body SourceSpan, Maybe SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (CallableF kind reciever name body ty SourceSpan)
pCallable pKind pReciever pName pBody pTy = do
  header <- pCallableHeader pKind pReciever pName pTy
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

pFnDeclaration ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (FnDeclarationF ty SourceSpan)
pFnDeclaration =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Fn))
    (pure Surface.UnitF)
    pIdentifier
    pBodySemicolon

pFnInfixDeclaration ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (FnInfixDeclarationF ty SourceSpan)
pFnInfixDeclaration pTy =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Fn))
    (pRecieverHeader pTy)
    pIdentifier
    pBodySemicolon
    pTy

pFnDefinition ::
  (HasAnn ty SourceSpan, HasAnn expr SourceSpan, HasAnn stmt SourceSpan) =>
  Parser (stmt SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (FnDefinitionF stmt ty expr SourceSpan)
pFnDefinition pStmt pTy pExpr =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Fn))
    (pure Surface.UnitF)
    pIdentifier
    (pCodeBlock pStmt pExpr <&> \block -> (block, Just block.ann))
    pTy

pFnInfixDefinition ::
  (HasAnn ty SourceSpan, HasAnn expr SourceSpan, HasAnn stmt SourceSpan) =>
  Parser (stmt SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (FnInfixDefinitionF stmt ty expr SourceSpan)
pFnInfixDefinition pStmt pTy pExpr =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Fn))
    (pRecieverHeader pTy)
    pIdentifier
    (pCodeBlock pStmt pExpr <&> \block -> (block, Just block.ann))
    pTy

pOpDeclaration ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (OpDeclarationF ty SourceSpan)
pOpDeclaration =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Op))
    (pure Surface.UnitF)
    pIdentifier
    pBodySemicolon

pOpInfixDeclaration ::
  (HasAnn ty SourceSpan) =>
  Parser (ty SourceSpan) ->
  Parser (OpInfixDeclarationF ty SourceSpan)
pOpInfixDeclaration pTy =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Op))
    (pRecieverHeader pTy)
    pIdentifier
    pBodySemicolon
    pTy

pOpDefinition ::
  (HasAnn ty SourceSpan, HasAnn expr SourceSpan, HasAnn stmt SourceSpan) =>
  Parser (stmt SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (OpDefinitionF stmt ty expr SourceSpan)
pOpDefinition pStmt pTy pExpr =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Op))
    (pure Surface.UnitF)
    (pQualifiedIdentifier pTy)
    (pCodeBlock pStmt pExpr <&> \block -> (block, Just block.ann))
    pTy

pOpInfixDefinition ::
  (HasAnn ty SourceSpan, HasAnn expr SourceSpan, HasAnn stmt SourceSpan) =>
  Parser (stmt SourceSpan) ->
  Parser (ty SourceSpan) ->
  Parser (expr SourceSpan) ->
  Parser (OpInfixDefinitionF stmt ty expr SourceSpan)
pOpInfixDefinition pStmt pTy pExpr =
  pCallable
    (void <$> single (Lexer.Keyword Lexer.Op))
    (pRecieverHeader pTy)
    (pQualifiedIdentifier pTy)
    (pCodeBlock pStmt pExpr <&> \block -> (block, Just block.ann))
    pTy

pBodySemicolon :: Parser (Surface.UnitF ann, Maybe SourceSpan)
pBodySemicolon =
  single (Lexer.Punctuation Lexer.Semicolon) <&> \semicolon ->
    (Surface.UnitF, Just semicolon.span)
