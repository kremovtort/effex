module Flow.Parser.LHSExpressionSpec (spec) where

import "hspec" Test.Hspec (Spec, describe, it)
import "text" Data.Text (Text)

import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Core qualified as Surface
import Flow.AST.Surface.Type qualified as Surface
import Flow.Parser.Core (pLHSExpression)
import Flow.Parser.SpecHelpers (shouldBe, shouldBeParsed, testParser)

lhsVar :: Text -> Surface.LHSExpression ()
lhsVar name =
  Surface.LHSExpression
    { lhs = Surface.LHSEVar Surface.Identifier{name, ann = ()}
    , ann = ()
    }

lhsIndex :: Surface.LHSExpression () -> Surface.Expression () -> Surface.LHSExpression ()
lhsIndex lhs rhs =
  Surface.LHSExpression
    { lhs = Surface.LHSEIndex lhs rhs
    , ann = ()
    }

lhsDot :: Surface.LHSExpression () -> Text -> Surface.LHSExpression ()
lhsDot lhs field =
  Surface.LHSExpression
    { lhs =
        Surface.LHSEDotAccess
          lhs
          Surface.Identifier
            { name = field
            , ann = ()
            }
    , ann = ()
    }

lhsDeref :: Surface.Expression () -> Surface.LHSExpression ()
lhsDeref expr =
  Surface.LHSExpression
    { lhs = Surface.LHSEUnOp (Surface.LHSUnOpExpressionDeref expr)
    , ann = ()
    }

exprVar :: Text -> Surface.Expression ()
exprVar name =
  Surface.Expression
    { expr =
        Surface.EIdent
          ( Surface.QualifiedIdentifierF
              { qualifierPrefix = Nothing
              , qualifier = Nothing
              , typeQualifier = Nothing
              , identifier = Surface.Identifier{name, ann = ()}
              , ann = ()
              }
          )
    , ann = ()
    }

spec :: Spec
spec = describe "LHSExpression parser (minimal subset)" do
  it "parses variable x" do
    testParser "x" pLHSExpression $ shouldBeParsed (`shouldBe` lhsVar "x")

  it "parses index a[b]" do
    let expected = lhsIndex (lhsVar "a") (exprVar "b")
    testParser "a[b]" pLHSExpression $ shouldBeParsed (`shouldBe` expected)

  it "parses dot access a.b" do
    let expected = lhsDot (lhsVar "a") "b"
    testParser "a.b" pLHSExpression $ shouldBeParsed (`shouldBe` expected)

  it "parses multiple dot accesses a.b.c" do
    let expected = lhsDot (lhsDot (lhsVar "a") "b") "c"
    testParser "a.b.c" pLHSExpression $ shouldBeParsed (`shouldBe` expected)

  it "parses deref *a" do
    let expected = lhsDeref (exprVar "a")
    testParser "*a" pLHSExpression $ shouldBeParsed (`shouldBe` expected)
