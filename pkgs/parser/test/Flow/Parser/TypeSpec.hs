module Flow.Parser.TypeSpec (spec) where

import "base" Data.Maybe (fromJust)
import "hspec" Test.Hspec (Spec, describe, it)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NonEmptyVector
import "text" Data.Text (Text)
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Type qualified as Surface
import Flow.Parser.SpecHelpers (shouldBe, shouldBeParsed, testParser)
import Flow.Parser.Type qualified as PType

anyType :: Surface.Identifier () -> Surface.QualifiedIdentifierF ()
anyType ident =
  Surface.QualifiedIdentifierF
    { qualifierPrefix = Nothing
    , qualifier = Nothing
    , typeQualifier = Nothing
    , identifier = ident
    , ann = ()
    }

simpleType :: Text -> Surface.Type ()
simpleType name =
  Surface.Type
    { ty =
        Surface.TyIdentifierF
          ( anyType
              Surface.Identifier
                { name = name
                , ann = ()
                }
          )
    , ann = ()
    }

ref :: Maybe (Surface.RegionIdentifier ()) -> Bool -> Surface.Type () -> Surface.Type ()
ref mregion mut inner =
  Surface.Type
    { ty =
        Surface.TyRefAppF
          ( Surface.RefF
              { region = mregion
              , mutability = if mut then Just () else Nothing
              , ann = ()
              }
          )
          inner
    , ann = ()
    }

fnType :: [Surface.Type ()] -> Maybe (Surface.FnEffectsResultF ()) -> Surface.Type ()
fnType args effRes =
  Surface.Type
    { ty =
        Surface.TyFnF
          Surface.FnF
            { args = Vector.fromList args
            , effectsResult = effRes
            , ann = ()
            }
    , ann = ()
    }

fnEffRes :: Maybe (Surface.FnEffectsF ()) -> Surface.Type () -> Surface.FnEffectsResultF ()
fnEffRes effects result =
  Surface.FnEffectsResultF
    { effects = effects
    , result = result
    , ann = ()
    }

fnEffectRow :: [Surface.FnEffectAtomF ()] -> Maybe (Surface.Type ()) -> Surface.FnEffectsF ()
fnEffectRow atoms tailVar =
  Surface.FnEffectsRowF
    Surface.FnEffectRowF
      { regions = Vector.empty
      , effects = Vector.fromList atoms
      , tailVars = maybe Vector.empty Vector.singleton tailVar
      , ann = ()
      }

effectRow :: [Surface.Type ()] -> Maybe (Surface.Type ()) -> Surface.Type ()
effectRow effects tailVar =
  Surface.Type
    { ty =
        Surface.TyEffectRowF
          Surface.EffectRowF
            { effects = Vector.fromList effects
            , tailVars = maybe Vector.empty Vector.singleton tailVar
            , ann = ()
            }
    , ann = ()
    }

spec :: Spec
spec = describe "Type parser (minimal subset)" do
  it "parses bool" do
    testParser "bool" PType.pType $ shouldBeParsed (`shouldBe` simpleType "bool")

  it "parses simple identifier Option" do
    testParser "Option" PType.pType $ shouldBeParsed (`shouldBe` simpleType "Option")

  it "parses application Option<i32>" do
    let headT = simpleType "Option"
        i32T = simpleType "i32"
        app =
          Surface.Type
            { ty =
                Surface.TyAppF
                  Surface.AppF
                    { head = headT
                    , args =
                        Surface.TypeArgumentsF
                          { types = NonEmptyVector.singleton i32T
                          , ann = ()
                          }
                    , ann = ()
                    }
            , ann = ()
            }
    testParser "Option<i32>" PType.pType $ shouldBeParsed (`shouldBe` app)

  it "parses tuple (i32, string)" do
    let tup =
          Surface.Type
            { ty =
                Surface.TyTupleF
                  ( fromJust $
                      NonEmptyVector.fromList [simpleType "i32", simpleType "string"]
                  )
            , ann = ()
            }
    testParser "(i32, string)" PType.pType $ shouldBeParsed (`shouldBe` tup)

  describe "parses applied refs" do
    let tT = simpleType "T"
        s = Surface.RegionIdentifier{name = "s", ann = ()}

    it "&T" do
      testParser "&T" PType.pType $ shouldBeParsed (`shouldBe` ref Nothing False tT)

    it "&mut T" do
      testParser "&mut T" PType.pType $ shouldBeParsed (`shouldBe` ref Nothing True tT)

    it "&'s T" do
      testParser "&'s T" PType.pType $ shouldBeParsed (`shouldBe` ref (Just s) False tT)

    it "&'s mut T" do
      testParser "&'s mut T" PType.pType $ shouldBeParsed (`shouldBe` ref (Just s) True tT)

  it "parses fn type fn(i32) -> i32" do
    let t =
          fnType
            [simpleType "i32"]
            (Just (fnEffRes Nothing (simpleType "i32")))
    testParser "fn(i32) -> i32" PType.pType $ shouldBeParsed (`shouldBe` t)

  it "parses fn with effect row fn(i32) -> @[IO] i32" do
    let ioType = simpleType "IO"
        row = fnEffectRow [Surface.FnEffectAtomTypeF ioType] Nothing
        t =
          fnType
            [simpleType "i32"]
            (Just (fnEffRes (Just row) (simpleType "i32")))
    testParser "fn(i32) -> @[IO] i32" PType.pType $ shouldBeParsed (`shouldBe` t)

  it "parses short effect row fn(i32) -> @R i32" do
    let row = simpleType "R"
        t =
          fnType
            [simpleType "i32"]
            ( Just
                ( fnEffRes
                    (Just (Surface.FnEffectsTypeF row))
                    (simpleType "i32")
                )
            )
    testParser "fn(i32) -> @R i32" PType.pType $ shouldBeParsed (`shouldBe` t)

  it "parses effect row @[IO]" do
    let ioType = simpleType "IO"
        row = effectRow [ioType] Nothing
    testParser "@[IO]" PType.pType $ shouldBeParsed (`shouldBe` row)
