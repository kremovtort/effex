module Flow.Parser.Mod where

import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "vector" Data.Vector qualified as Vector

import Flow.AST.Ann (SourceSpan (..))
import Flow.AST.Surface.Core qualified as Surface
import Flow.AST.Surface.Data qualified as Surface
import Flow.AST.Surface.Mod qualified as Surface
import Flow.AST.Surface.Type qualified as Surface
import Flow.AST.Surface.Use qualified as Surface
import Flow.Lexer qualified as Lexer
import Flow.Parser.Common (Parser, pIdentifier, pPub, single)
import Flow.Parser.Core
import Flow.Parser.Data
import Flow.Parser.Type
import Flow.Parser.Use (pUseClause)

pModDefinitionBody :: Parser (Surface.ModDefinitionBodyF SourceSpan)
pModDefinitionBody = do
  items <- Megaparsec.many pModuleItem
  pure $
    Surface.ModDefinitionBodyF
      { items = Vector.fromList items
      }

pModuleItem :: Parser (Surface.ModuleItemF SourceSpan)
pModuleItem = do
  pub <- Megaparsec.optional pPub
  item <- pModuleItemVariant
  let ann =
        SourceSpan
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
      [ withRegion Surface.ModItemModF <$> pMod
      , withRegion Surface.ModItemStructF <$> pStruct
      , withRegion Surface.ModItemEnumF <$> pEnum
      , withRegion Surface.ModItemTypeAliasF <$> do
          pTypeDefinition <* single (Lexer.Punctuation Lexer.Semicolon)
      , withRegion Surface.ModItemLetF <$> pLetDefinition
      , withRegion Surface.ModItemTraitF <$> pTrait
      , withRegion Surface.ModItemEffectF <$> pEffect
      , withRegion Surface.ModItemUseF <$> pPubUse
      , withRegion Surface.ModItemFnF <$> do
          Megaparsec.try pFnDefinition
      , withRegion Surface.ModItemFnInfixF <$> do
          Megaparsec.try pFnInfixDefinition
      ]

  withRegion f item = (f item, item.ann)

pMod :: Parser (Surface.Mod SourceSpan)
pMod = do
  mod' <-
    Megaparsec.choice
      [ Megaparsec.try pModDeclaration
      , pModDefinition
      ]
  pure Surface.Mod{mod = fst mod', ann = snd mod'}

pModDeclaration :: Parser (Surface.ModF SourceSpan, SourceSpan)
pModDeclaration = do
  modTok <- single (Lexer.Keyword Lexer.Mod)
  ident <- pIdentifier
  tokE <- single (Lexer.Punctuation Lexer.Semicolon)
  pure (Surface.ModDeclarationF ident, SourceSpan{start = modTok.span.start, end = tokE.span.end})

pModDefinition ::
  Parser (Surface.ModF SourceSpan, SourceSpan)
pModDefinition = do
  modTok <- single (Lexer.Keyword Lexer.Mod)
  ident <- pIdentifier
  _ <- single (Lexer.Punctuation Lexer.LeftBrace)
  body <- pModDefinitionBody
  tokE <- single (Lexer.Punctuation Lexer.RightBrace)
  let ann = SourceSpan{start = modTok.span.start, end = tokE.span.end}
  pure (Surface.ModDefinitionF ident body, ann)

pPubUse :: Parser (Surface.UseClause SourceSpan)
pPubUse = pUseClause
