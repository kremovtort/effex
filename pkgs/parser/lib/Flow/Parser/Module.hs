{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Flow.Parser.Module where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface.Callable qualified as Surface
import Flow.AST.Surface.Constraint qualified as Surface
import Flow.AST.Surface.Decl qualified as Surface
import Flow.AST.Surface.Module qualified as Surface
import Flow.AST.Surface.Syntax qualified as Surface
import Flow.AST.Surface.Use qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Callable (pFnDefinition, pFnInfixDefinition)
import Flow.Parser.Common (HasAnn, Parser, pIdentifier, pPub, single)
import Flow.Parser.Constraint (pTypeDefinition)
import Flow.Parser.Decl (pEffect, pEnum, pStruct, pTrait)
import Flow.Parser.Syntax (pLetDefinition)
import Flow.Parser.Use (pUseClause)

pModDefinitionBody ::
  ( HasAnn mod Lexer.SourceSpan
  , HasAnn stmt Lexer.SourceSpan
  , HasAnn simPat Lexer.SourceSpan
  , HasAnn ty Lexer.SourceSpan
  , HasAnn expr Lexer.SourceSpan
  ) =>
  Parser (mod Lexer.SourceSpan) ->
  Parser (stmt Lexer.SourceSpan) ->
  Parser (simPat Lexer.SourceSpan) ->
  Parser (ty Lexer.SourceSpan) ->
  Parser (expr Lexer.SourceSpan) ->
  Parser (Surface.ModDefinitionBodyF mod stmt simPat ty expr Lexer.SourceSpan)
pModDefinitionBody pMod' pStmt pSimPat pTy pExpr = do
  items <- Megaparsec.many (pModuleItem pMod' pStmt pSimPat pTy pExpr)
  pure $
    Surface.ModDefinitionBodyF
      { items = Vector.fromList items
      }

pModuleItem ::
  ( HasAnn mod Lexer.SourceSpan
  , HasAnn stmt Lexer.SourceSpan
  , HasAnn simPat Lexer.SourceSpan
  , HasAnn ty Lexer.SourceSpan
  , HasAnn expr Lexer.SourceSpan
  ) =>
  Parser (mod Lexer.SourceSpan) ->
  Parser (stmt Lexer.SourceSpan) ->
  Parser (simPat Lexer.SourceSpan) ->
  Parser (ty Lexer.SourceSpan) ->
  Parser (expr Lexer.SourceSpan) ->
  Parser (Surface.ModuleItemF mod stmt simPat ty expr Lexer.SourceSpan)
pModuleItem pMod' pStmt pSimPat pTy pExpr = do
  pub <- Megaparsec.optional pPub
  item <- pModuleItemVariant
  let ann =
        Lexer.SourceSpan
          { start = case pub of
              Just (_, region) -> region.start
              Nothing -> (snd item).start
          , end = (snd item).end
          }
  pure
    Surface.ModuleItemF
      { pub = fmap fst pub
      , item = fst item
      , ann
      }
 where
  pModuleItemVariant = do
    Megaparsec.choice
      [ withRegion Surface.ModItemModF <$> pMod'
      , withRegion Surface.ModItemStructF <$> pStruct pTy
      , withRegion Surface.ModItemEnumF <$> pEnum pTy
      , withRegion Surface.ModItemTypeAliasF <$> do
          pTypeDefinition pTy <* single (Lexer.Punctuation Lexer.Semicolon)
      , withRegion Surface.ModItemLetF <$> pLetDefinition pSimPat pTy pExpr
      , withRegion Surface.ModItemTraitF <$> pTrait pStmt pTy pExpr
      , withRegion Surface.ModItemEffectF <$> pEffect pStmt pTy pExpr
      , withRegion Surface.ModItemUseF <$> pPubUse
      , withRegion Surface.ModItemFnF <$> do
          Megaparsec.try (pFnDefinition pStmt pTy pExpr)
      , withRegion Surface.ModItemFnInfixF <$> do
          Megaparsec.try (pFnInfixDefinition pStmt pTy pExpr)
      ]

  withRegion f item = (f item, item.ann)

pMod ::
  ( HasAnn mod Lexer.SourceSpan
  , HasAnn stmt Lexer.SourceSpan
  , HasAnn simPat Lexer.SourceSpan
  , HasAnn ty Lexer.SourceSpan
  , HasAnn expr Lexer.SourceSpan
  ) =>
  Parser (mod Lexer.SourceSpan) ->
  Parser (stmt Lexer.SourceSpan) ->
  Parser (simPat Lexer.SourceSpan) ->
  Parser (ty Lexer.SourceSpan) ->
  Parser (expr Lexer.SourceSpan) ->
  Parser (Surface.ModF mod stmt simPat ty expr Lexer.SourceSpan, Lexer.SourceSpan)
pMod pMod' pStmt pSimPat pTy pExpr =
  Megaparsec.choice
    [ Megaparsec.try pModDeclaration
    , pModDefinition pMod' pStmt pSimPat pTy pExpr
    ]

pModDeclaration ::
  Parser (Surface.ModF mod lhsExpr simPat ty expr Lexer.SourceSpan, Lexer.SourceSpan)
pModDeclaration = do
  modTok <- single (Lexer.Keyword Lexer.Mod)
  ident <- pIdentifier
  tokE <- single (Lexer.Punctuation Lexer.Semicolon)
  pure (Surface.ModDeclarationF ident, Lexer.SourceSpan{start = modTok.span.start, end = tokE.span.end})

pModDefinition ::
  ( HasAnn mod Lexer.SourceSpan
  , HasAnn stmt Lexer.SourceSpan
  , HasAnn simPat Lexer.SourceSpan
  , HasAnn ty Lexer.SourceSpan
  , HasAnn expr Lexer.SourceSpan
  ) =>
  Parser (mod Lexer.SourceSpan) ->
  Parser (stmt Lexer.SourceSpan) ->
  Parser (simPat Lexer.SourceSpan) ->
  Parser (ty Lexer.SourceSpan) ->
  Parser (expr Lexer.SourceSpan) ->
  Parser (Surface.ModF mod stmt simPat ty expr Lexer.SourceSpan, Lexer.SourceSpan)
pModDefinition pMod' pStmt pSimPat pTy pExpr = do
  modTok <- single (Lexer.Keyword Lexer.Mod)
  ident <- pIdentifier
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  body <- pModDefinitionBody pMod' pStmt pSimPat pTy pExpr
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann = Lexer.SourceSpan{start = modTok.span.start, end = tokE.span.end}
  pure (Surface.ModDefinitionF ident body, ann)

pPubUse :: Parser (Surface.UseClause Lexer.SourceSpan)
pPubUse = pUseClause
