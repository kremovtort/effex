module Effex.Core.ModuleSpec (spec) where

import Prelude

import "base" Data.Foldable (toList)
import "base" Data.Function ((&))
import "base" GHC.Stack (HasCallStack)
import "containers" Data.Sequence (Seq)
import "containers" Data.Sequence qualified as Seq
import "effectful" Effectful (runPureEff)
import "effectful" Effectful.Error.Static (runErrorNoCallStack)
import "generic-lens" Data.Generics.Labels ()
import "hspec" Test.Hspec (Spec, describe, expectationFailure, it)
import "lens" Control.Lens ((.~))
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "prettyprinter" Prettyprinter (defaultLayoutOptions, layoutSmart)
import "prettyprinter-ansi-terminal" Prettyprinter.Render.Terminal (renderStrict)
import "text" Data.Text (Text)
import "text" Data.Text qualified as Text
import "tree-diff" Data.TreeDiff.Class (ToExpr (..), ediff)
import "tree-diff" Data.TreeDiff.Pretty (ansiWlBgEditExprCompact)
import "vector" Data.Vector qualified as Vector

import Effex.AST.Ann (SourceSpan (..))
import Effex.AST.Surface (ModDefinitionBody)
import Effex.AST.Surface.Module (ModDefinitionBodyF (..))
import Effex.Core.Module (
  BasicAnn (..),
  DuplicateModuleDeclarationError (..),
  ModuleDeclaration (..),
  ModuleDefinition (..),
  ModuleId (..),
  PackageId (..),
  Visibility (..),
  collectModuleDefinitionModules,
 )
import Effex.Lexer qualified as Lexer
import Effex.Parser (pModDefinitionBody)

pShowDiff :: (Eq a, ToExpr a) => a -> a -> Text
pShowDiff a b =
  renderStrict $
    layoutSmart defaultLayoutOptions $
      ansiWlBgEditExprCompact $
        ediff a b

shouldBe :: (Eq a, ToExpr a, HasCallStack) => a -> a -> IO ()
shouldBe a b
  | a == b = pure ()
  | otherwise = do
      expectationFailure $ Text.unpack $ "Equation failed:\n" <> pShowDiff a b

spec :: Spec
spec = describe "collectModuleDefinitionModules" do
  it "collects declarations" do
    body <-
      parseModuleBody
        """
        pub mod foo;
        pub(package) mod bar;
        mod baz;
        """
    let modDef =
          ModuleDefinition
            { visibility = VisibilityAll
            , identifier = rootModuleId
            , body = body
            , declAnn = Nothing
            }
        result = runCollectModuleDefinitionModules modDef
    case result of
      Left err -> expectationFailure $ show err
      Right (defs, decls) -> do
        defs `shouldBe` Seq.fromList []
        (modDeclWithDummyAnn <$> decls)
          `shouldBe` Seq.fromList
            [ ModuleDeclaration
                { visibility = VisibilityAll
                , identifier = mkModuleId ["foo"]
                , ann = dummyAnn
                }
            , ModuleDeclaration
                { visibility = VisibilityPackage
                , identifier = mkModuleId ["bar"]
                , ann = dummyAnn
                }
            , ModuleDeclaration
                { visibility = VisibilityPackage
                , identifier = mkModuleId ["baz"]
                , ann = dummyAnn
                }
            ]

  it "fails on duplicate module declarations" do
    body <-
      parseModuleBody
        """
        mod foo {}
        mod foo;
        """
    let modDef =
          ModuleDefinition
            { visibility = VisibilityAll
            , identifier = rootModuleId
            , body = body
            , declAnn = Nothing
            }
    case runCollectModuleDefinitionModules modDef of
      Left (DuplicateModuleDeclarationError moduleId _) ->
        moduleId `shouldBe` mkModuleId ["foo"]
      other ->
        expectationFailure $
          "expected duplicate declaration error, but got: " <> show other

  it "collects declarations from nested module definitions" do
    body <-
      parseModuleBody
        """
        mod outer {
          pub mod inner;
          mod nested {
            pub mod deep;
          }
          mod leaf;
        }
        pub mod top;
        """
    let modDef =
          ModuleDefinition
            { visibility = VisibilityAll
            , identifier = rootModuleId
            , body = body
            , declAnn = Nothing
            }
        result = runCollectModuleDefinitionModules modDef
    case result of
      Left err -> expectationFailure $ show err
      Right (defs, decls) -> do
        let defs' = modDefWithDummy <$> defs
        defs'
          `shouldBe` Seq.fromList
            [ ModuleDefinition
                { visibility = VisibilityPackage
                , identifier = mkModuleId ["outer"]
                , body = dummyBody
                , declAnn = Nothing
                }
            , ModuleDefinition
                { visibility = VisibilityModule (mkModuleId ["outer"])
                , identifier = mkModuleId ["outer", "nested"]
                , body = dummyBody
                , declAnn = Nothing
                }
            ]
        (modDeclWithDummyAnn <$> decls)
          `shouldBe` Seq.fromList
            [ ModuleDeclaration
                { visibility = VisibilityAll
                , identifier = mkModuleId ["top"]
                , ann = dummyAnn
                }
            , ModuleDeclaration
                { visibility = VisibilityPackage
                , identifier = mkModuleId ["outer", "inner"]
                , ann = dummyAnn
                }
            , ModuleDeclaration
                { visibility = VisibilityModule (mkModuleId ["outer"])
                , identifier = mkModuleId ["outer", "leaf"]
                , ann = dummyAnn
                }
            , ModuleDeclaration
                { visibility = VisibilityModule (mkModuleId ["outer"])
                , identifier = mkModuleId ["outer", "nested", "deep"]
                , ann = dummyAnn
                }
            ]

testPackageId :: PackageId
testPackageId = PackageId "test"

rootModuleId :: ModuleId
rootModuleId = mkModuleId []

mkModuleId :: [Text] -> ModuleId
mkModuleId parts =
  ModuleId
    { package = testPackageId
    , path = Vector.fromList parts
    }

runCollectModuleDefinitionModules ::
  (HasCallStack) =>
  ModuleDefinition ->
  Either DuplicateModuleDeclarationError (Seq ModuleDefinition, Seq ModuleDeclaration)
runCollectModuleDefinitionModules modDef =
  runPureEff $ runErrorNoCallStack $ collectModuleDefinitionModules modDef

parseModuleBody :: (HasCallStack) => Text -> IO (ModDefinitionBody BasicAnn)
parseModuleBody source = do
  tokens <- case Megaparsec.runParser Lexer.tokensWithPos testPath source of
    Left err -> parseFailure "lexer" err
    Right toks -> pure toks
  let tokenStream =
        Lexer.TokenStream
          { tokens = toList tokens
          , inputPos = Megaparsec.initialPos testPath
          , input = source
          }
  case Megaparsec.runParser pModDefinitionBody testPath tokenStream of
    Left err -> parseFailure "parser" err
    Right body -> pure $ annotate body
 where
  annotate :: ModDefinitionBody SourceSpan -> ModDefinitionBody BasicAnn
  annotate = fmap \span' -> BasicAnn{filePath = testPath, span = span'}

  parseFailure stage err = do
    expectationFailure $
      "Failed during " <> stage <> ":\n" <> Megaparsec.errorBundlePretty err
    fail "parseModuleBody: unreachable"

testPath :: FilePath
testPath = "<collect-module-spec>"

modDeclWithDummyAnn :: ModuleDeclaration -> ModuleDeclaration
modDeclWithDummyAnn modDecl = modDecl & #ann .~ dummyAnn

modDefWithDummy :: ModuleDefinition -> ModuleDefinition
modDefWithDummy modDef =
  modDef
    & #body .~ dummyBody
    & #declAnn .~ Nothing

dummyBody :: ModDefinitionBody BasicAnn
dummyBody = ModDefinitionBodyF{items = Vector.empty}

dummyAnn :: BasicAnn
dummyAnn =
  BasicAnn
    { filePath = testPath
    , span = SourceSpan{start = Megaparsec.initialPos testPath, end = Megaparsec.initialPos testPath}
    }
