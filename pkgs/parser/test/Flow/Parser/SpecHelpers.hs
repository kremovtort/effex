module Effex.Parser.SpecHelpers where

import "base" Data.Foldable (toList)
import "base" Data.Functor (void)
import "base" Data.Void (Void)
import "base" GHC.Stack (HasCallStack)
import "hspec" Test.Hspec (Expectation, expectationFailure)
import "megaparsec" Text.Megaparsec (parse)
import "megaparsec" Text.Megaparsec qualified as Megaparsec
import "prettyprinter" Prettyprinter (defaultLayoutOptions, layoutSmart)
import "prettyprinter-ansi-terminal" Prettyprinter.Render.Terminal (renderStrict)
import "text" Data.Text (Text)
import "text" Data.Text qualified as Text
import "tree-diff" Data.TreeDiff.Class (ToExpr (..), ediff)
import "tree-diff" Data.TreeDiff.Pretty (ansiWlBgEditExprCompact, ansiWlExpr)

import Effex.Lexer qualified as Lexer
import Effex.Parser.Common (Parser)

pShow :: (ToExpr a) => a -> Text
pShow = renderStrict . layoutSmart defaultLayoutOptions . ansiWlExpr . toExpr

pShowDiff :: (Eq a, ToExpr a) => a -> a -> Text
pShowDiff a b =
  renderStrict $
    layoutSmart defaultLayoutOptions $
      ansiWlBgEditExprCompact $
        ediff a b

shouldBeParsed ::
  (HasCallStack) =>
  ((HasCallStack) => f () -> Expectation) ->
  Either
    (Megaparsec.ParseErrorBundle Lexer.TokenStream Void)
    (f ()) ->
  IO ()
shouldBeParsed expectation parseResult = do
  case parseResult of
    Left e ->
      expectationFailure $ "Parser failed to parse:\n" <> Megaparsec.errorBundlePretty e
    Right v ->
      expectation v

shouldBeParsedDebug ::
  (HasCallStack) =>
  ((HasCallStack) => f () -> Expectation) ->
  Either
    (Megaparsec.ParseErrorBundle Lexer.TokenStream Void)
    (f ()) ->
  IO ()
shouldBeParsedDebug expectation parseResult = do
  case parseResult of
    Left e ->
      expectationFailure $ "Parser failed to parse:\n" <> show e
    Right v ->
      expectation v

shouldBeFailed ::
  (Eq (f ()), ToExpr (f ()), HasCallStack) =>
  Either
    (Megaparsec.ParseErrorBundle Lexer.TokenStream Void)
    (f ()) ->
  IO ()
shouldBeFailed parseResult = do
  case parseResult of
    Left _ ->
      pure ()
    Right v ->
      expectationFailure $ "Parser parsed but should have failed:\n" <> Text.unpack (pShow v)

shouldBe :: (Eq a, ToExpr a, HasCallStack) => a -> a -> IO ()
shouldBe a b
  | a == b = pure ()
  | otherwise = do
      expectationFailure $ Text.unpack $ "Equation failed:\n" <> pShowDiff a b

lexTokens :: Text -> IO Lexer.TokenStream
lexTokens s = case parse Lexer.tokensWithPos "<lex>" s of
  Left e -> do
    fail
      ("Lexer failed to tokenize:\n" <> Megaparsec.errorBundlePretty e)
  Right v ->
    pure
      Lexer.TokenStream
        { tokens = toList v
        , inputPos = Megaparsec.initialPos "<lex>"
        , input = s
        }

testParser ::
  ( HasCallStack
  , Functor f
  , Eq (f ())
  , Show (f ())
  , ToExpr (f ())
  ) =>
  Text ->
  Parser (f Lexer.SourceSpan) ->
  ( (HasCallStack) =>
    Either
      (Megaparsec.ParseErrorBundle Lexer.TokenStream Void)
      (f ()) ->
    Expectation
  ) ->
  IO ()
testParser txt parser expectation = do
  toks <- lexTokens txt
  expectation (parse (void <$> (parser <* Megaparsec.eof)) "<parser>" toks)
