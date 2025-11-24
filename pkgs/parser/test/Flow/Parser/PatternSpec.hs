module Flow.Parser.PatternSpec (spec) where

import "base" Data.Functor ((<&>))
import "base" Data.List.NonEmpty qualified as ListNE
import "base" Data.Maybe (fromJust)
import "base" GHC.Stack (HasCallStack)
import "hspec" Test.Hspec (Spec, describe, it)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NE
import "text" Data.Text (Text)
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Literal qualified as Surface
import Flow.AST.Surface.Pattern qualified as Surface
import Flow.AST.Surface.Type qualified as Surface
import Flow.Parser.Pattern (pPattern, pPatternSimple)
import Flow.Parser.SpecHelpers (shouldBe, shouldBeParsed, testParser)

anyType :: Surface.Identifier () -> Surface.QualifiedIdentifierF ()
anyType ident =
  Surface.QualifiedIdentifierF
    { qualifierPrefix = Nothing
    , qualifier = Nothing
    , typeQualifier = Nothing
    , identifier = ident
    , ann = ()
    }

mkVar :: Text -> Surface.Identifier ()
mkVar name = Surface.Identifier{name, ann = ()}

wrapSimple ::
  Surface.PatternSimpleF Surface.PatternSimple () ->
  Surface.PatternSimple ()
wrapSimple simple =
  Surface.PatternSimple
    { pat = simple
    , ann = ()
    }

wrapPattern ::
  Surface.PatternF () ->
  Surface.Pattern ()
wrapPattern pattern =
  Surface.Pattern
    { pat = pattern
    , ann = ()
    }

wildcardPattern :: Surface.PatternSimple ()
wildcardPattern = wrapSimple Surface.PatSimWildcardF

literalBoolPattern :: Bool -> Surface.Pattern ()
literalBoolPattern value =
  wrapPattern (Surface.PatLiteralF (Surface.LitBool value))

literalIntPattern :: Integer -> Surface.Pattern ()
literalIntPattern value =
  wrapPattern (Surface.PatLiteralF (Surface.LitInteger value))

varPattern :: Text -> Surface.Pattern ()
varPattern name =
  wrapPattern $
    Surface.PatSimpleF $
      Surface.PatSimVarF
        ( Surface.PatternVarF
            { ref = Nothing
            , mut = Nothing
            , name = mkVar name
            , ann = ()
            }
        )

tuplePattern :: [Surface.Pattern ()] -> Surface.Pattern ()
tuplePattern ps =
  wrapPattern $
    Surface.PatSimpleF $
      Surface.PatSimTupleF (requireVector "tuplePattern" ps)

constructorPattern ::
  Text ->
  Maybe [Surface.Identifier ()] ->
  Surface.PatternFieldsF Surface.Pattern () ->
  Surface.Pattern ()
constructorPattern name params fields =
  wrapPattern $
    Surface.PatSimpleF
      ( Surface.PatSimConstructorAppF
          Surface.PatternConsturctorAppF
            { name = anyType Surface.Identifier{name, ann = ()}
            , typeParams =
                params <&> \params' ->
                  Surface.BindersF
                    { regions = mempty
                    , types =
                        Vector.fromList $
                          params' <&> \param'' ->
                            Surface.BinderWoConstraintF
                              { name = param''
                              , kindShort = Nothing
                              , typeType = Nothing
                              , ann = ()
                              }
                    , ann = ()
                    }
            , fields
            , fieldsAnn = ()
            , ann = ()
            }
      )

fieldsTuple ::
  [Surface.Pattern ()] ->
  Surface.PatternFieldsF Surface.Pattern ()
fieldsTuple ps =
  Surface.PatFldsUnnamedF
    ( requireVector
        "fieldsTuple"
        ( ps <&> \p ->
            Surface.PatternFieldUnnamedF
              { value = p
              , optional = Nothing
              , ann = ()
              }
        )
    )

fieldsNamed ::
  [Surface.PatternFieldNamedF Surface.Pattern ()] ->
  Surface.PatternFieldsF Surface.Pattern ()
fieldsNamed ps = Surface.PatFldsNamedF (fromJust $ NE.fromList ps)

mkFieldNamed :: Text -> Surface.Pattern () -> Surface.PatternFieldNamedF Surface.Pattern ()
mkFieldNamed name value =
  Surface.PatFldNmdValueF $
    Surface.PatternFieldNamedValueF
      { name = mkVar name
      , value = value
      , ann = ()
      }

mkFieldNamedPun :: Text -> Surface.PatternFieldNamedF Surface.Pattern ()
mkFieldNamedPun name =
  Surface.PatFldNmdPunningF $
    Surface.PatternFieldNamedPunningF
      { ref = Nothing
      , mut = Nothing
      , name = mkVar name
      , optional = Nothing
      , ann = ()
      }

requireList :: (HasCallStack) => String -> [a] -> ListNE.NonEmpty a
requireList label [] = error (label <> ": expected non-empty list")
requireList _label (x : xs) = x ListNE.:| xs

requireVector :: (HasCallStack) => String -> [a] -> NE.NonEmptyVector a
requireVector label = NE.fromNonEmpty . requireList label

spec :: Spec
spec = describe "Pattern parser (minimal subset)" do
  it "parses wildcard _" do
    testParser "_" pPatternSimple $ shouldBeParsed (`shouldBe` wildcardPattern)

  it "parses literal true" do
    testParser "true" pPattern $ shouldBeParsed (`shouldBe` literalBoolPattern True)

  it "parses variable x" do
    testParser "x" pPattern $ shouldBeParsed (`shouldBe` varPattern "x")

  it "parses tuple (x, y)" do
    testParser "(x, y)" pPattern $ shouldBeParsed (`shouldBe` tuplePattern [varPattern "x", varPattern "y"])

  it "parses constructor without arguments None" do
    let expected = varPattern "None"
    testParser "None" pPattern $ shouldBeParsed (`shouldBe` expected)

  it "parses constructor Some(1)" do
    let expected =
          constructorPattern
            "Some"
            Nothing
            (fieldsTuple [literalIntPattern 1])
    testParser "Some(1)" pPattern $ shouldBeParsed (`shouldBe` expected)

  it "parses constructor with named fields Cons { head = 1, tail = xs }" do
    let expected =
          constructorPattern
            "Cons"
            Nothing
            ( fieldsNamed
                [ mkFieldNamed "head" (literalIntPattern 1)
                , mkFieldNamed "tail" (varPattern "xs")
                ]
            )
    testParser "Cons { head = 1, tail = xs }" pPattern $ shouldBeParsed (`shouldBe` expected)

  it "parses constructor with fields punning Cons { head, tail }" do
    let expected =
          constructorPattern
            "Cons"
            Nothing
            (fieldsNamed [mkFieldNamedPun "head", mkFieldNamedPun "tail"])
    testParser "Cons { head, tail }" pPattern $ shouldBeParsed (`shouldBe` expected)
