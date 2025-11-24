module Flow.Parser.ModuleSpec (spec) where

import "base" Data.Maybe (fromJust)
import "hspec" Test.Hspec (Spec, describe, it)
import "nonempty-vector" Data.Vector.NonEmpty qualified as NE
import "text" Data.Text (Text)
import "text" Data.Text qualified as Text
import "vector" Data.Vector qualified as Vector

import Flow.AST.Surface.Common qualified as Surface
import Flow.AST.Surface.Core qualified as Surface
import Flow.AST.Surface.Data qualified as Surface
import Flow.AST.Surface.Literal qualified as Surface
import Flow.AST.Surface.Mod qualified as Surface
import Flow.AST.Surface.Pattern qualified as Surface
import Flow.AST.Surface.Type qualified as Surface
import Flow.AST.Surface.Use qualified as Surface
import Flow.Parser.Mod (pModDefinitionBody)
import Flow.Parser.SpecHelpers (shouldBe, shouldBeParsed, testParser)

modIdent :: Text -> Surface.Identifier ()
modIdent name = Surface.Identifier{name, ann = ()}

simpleVar :: Text -> Surface.Identifier ()
simpleVar name = Surface.Identifier{name, ann = ()}

simpleType :: Text -> Surface.Type ()
simpleType name =
  Surface.Type
    { ty =
        Surface.TyIdentifierF
          Surface.QualifiedIdentifierF
            { qualifierPrefix = Nothing
            , qualifier = Nothing
            , typeQualifier = Nothing
            , identifier = Surface.Identifier{name, ann = ()}
            , ann = ()
            }
    , ann = ()
    }

moduleBody :: [Surface.ModuleItemF ()] -> Surface.ModDefinitionBodyF ()
moduleBody items =
  Surface.ModDefinitionBodyF
    { items = Vector.fromList items
    }

modDecl :: Text -> Surface.ModuleItemF ()
modDecl name =
  Surface.ModuleItemF
    { pub = Nothing
    , item = Surface.ModItemModF (Surface.Mod (Surface.ModDeclarationF (modIdent name)) ())
    , ann = ()
    }

modDef :: Text -> [Surface.ModuleItemF ()] -> Surface.ModuleItemF ()
modDef name items =
  Surface.ModuleItemF
    { pub = Nothing
    , item =
        Surface.ModItemModF
          ( Surface.Mod
              { mod =
                  Surface.ModDefinitionF
                    (modIdent name)
                    Surface.ModDefinitionBodyF{items = Vector.fromList items}
              , ann = ()
              }
          )
    , ann = ()
    }

useClauseLeaf :: [Text] -> Surface.UseClause ()
useClauseLeaf path =
  case path of
    [] -> error "empty path"
    root : rest ->
      let
        buildTree [] = undefined
        buildTree [segment] =
          Surface.UseTrLeafIdent
            Surface.UseTreeLeaf
              { use = Surface.Identifier{name = segment, ann = ()}
              , as = Nothing
              , ann = ()
              }
        buildTree (segment : segments) =
          Surface.UseTrBranch (modIdent segment) (buildTree segments)
      in
        Surface.UseClause
          { root = Surface.UsClPackage (modIdent root)
          , tree = buildTree rest
          , ann = ()
          }

useClauseAs :: [Text] -> Text -> Surface.UseClause ()
useClauseAs path alias =
  case path of
    [] -> error "empty path"
    root : rest ->
      let
        build [] =
          Surface.UseTrLeafIdent
            Surface.UseTreeLeaf
              { use = Surface.Identifier{name = root, ann = ()}
              , as = Just $ Surface.Identifier{name = alias, ann = ()}
              , ann = ()
              }
        build [segment] =
          Surface.UseTrLeafIdent
            Surface.UseTreeLeaf
              { use = Surface.Identifier{name = segment, ann = ()}
              , as = Just $ Surface.Identifier{name = alias, ann = ()}
              , ann = ()
              }
        build (segment : segments) = Surface.UseTrBranch (modIdent segment) (build segments)
      in
        Surface.UseClause
          { root = Surface.UsClPackage (modIdent root)
          , tree = build rest
          , ann = ()
          }

structItem :: Maybe (Surface.Pub ()) -> Text -> Surface.ModuleItemF ()
structItem pub' name =
  Surface.ModuleItemF
    { pub = pub'
    , item =
        Surface.ModItemStructF
          Surface.StructF
            { name = Surface.Identifier{name, ann = ()}
            , typeParams = Nothing
            , whereBlock = Nothing
            , fields = Surface.FieldsDeclNamedF mempty
            , ann = ()
            }
    , ann = ()
    }

enumItem :: Maybe (Surface.Pub ()) -> Text -> [Text] -> Surface.ModuleItemF ()
enumItem pub' name variants =
  Surface.ModuleItemF
    { pub = pub'
    , item =
        Surface.ModItemEnumF
          Surface.EnumF
            { name = Surface.Identifier{name, ann = ()}
            , typeParams = Nothing
            , whereBlock = Nothing
            , variants =
                Surface.EVariantsSimpleF
                  ( case NE.fromList
                      ( fmap
                          ( \vname ->
                              Surface.EnumVariantF
                                { name =
                                    Surface.Identifier
                                      { name = vname
                                      , ann = ()
                                      }
                                , fields = Nothing
                                , ann = ()
                                }
                          )
                          variants
                      ) of
                      Nothing -> error "enumItem: expected non-empty variants"
                      Just ne -> ne
                  )
            , ann = ()
            }
    , ann = ()
    }

typeAliasItem :: Maybe (Surface.Pub ()) -> Text -> Surface.Type () -> Surface.ModuleItemF ()
typeAliasItem pub' name ty =
  Surface.ModuleItemF
    { pub = pub'
    , item =
        Surface.ModItemTypeAliasF
          Surface.TypeDefinitionF
            { name = Surface.Identifier{name, ann = ()}
            , typeParams =
                Just $
                  Surface.BindersF
                    { regions = mempty
                    , types =
                        Vector.fromList
                          [ Surface.BinderWoConstraintF
                              { name =
                                  Surface.Identifier
                                    { name = "X"
                                    , ann = ()
                                    }
                              , kindShort = Nothing
                              , typeType = Nothing
                              , ann = ()
                              }
                          , Surface.BinderWoConstraintF
                              { name =
                                  Surface.Identifier
                                    { name = "Y"
                                    , ann = ()
                                    }
                              , kindShort = Nothing
                              , typeType = Nothing
                              , ann = ()
                              }
                          ]
                    , ann = ()
                    }
            , type_ = ty
            , ann = ()
            }
    , ann = ()
    }

fnItem ::
  Maybe (Surface.Pub ()) ->
  Text ->
  [(Bool, Text, Surface.Type ())] ->
  Maybe (Surface.Type ()) ->
  Maybe (Surface.Type ()) ->
  Surface.ModuleItemF ()
fnItem pub' name args effects result =
  Surface.ModuleItemF
    { pub = pub'
    , item =
        Surface.ModItemFnF
          Surface.CallableF
            { header =
                Surface.CallableHeader
                  { receiver = Surface.UnitF
                  , name = simpleVar name
                  , typeParams = Nothing
                  , args = Vector.fromList (map buildArg args)
                  , effectsResult = do
                      result' <- result
                      pure
                        Surface.FnEffectsResultF
                          { effects = fmap Surface.FnEffectsTypeF effects
                          , result = result'
                          , ann = ()
                          }
                  , whereBlock = Nothing
                  , ann = ()
                  }
            , body =
                Surface.CodeBlockF
                  { region = Nothing
                  , uses = mempty
                  , statements = mempty
                  , result = Nothing
                  , ann = ()
                  }
            , ann = ()
            }
    , ann = ()
    }
 where
  buildArg (mut, name', ty) =
    Surface.ArgF
      { mut = if mut then Just () else Nothing
      , name = simpleVar name'
      , type_ = ty
      , ann = ()
      }

letItem :: Maybe (Surface.Pub ()) -> Text -> Surface.Type () -> Surface.Expression () -> Surface.ModuleItemF ()
letItem pub' name ty expr =
  Surface.ModuleItemF
    { pub = pub'
    , item =
        Surface.ModItemLetF
          Surface.LetDefinitionF
            { lhs =
                Surface.PatternSimple
                  { pat =
                      Surface.PatSimVarF
                        ( Surface.PatternVarF
                            { ref = Nothing
                            , mut = Nothing
                            , name = simpleVar name
                            , ann = ()
                            }
                        )
                  , ann = ()
                  }
            , lhsType = Just ty
            , rhs = expr
            , ann = ()
            }
    , ann = ()
    }

tupleType :: [Surface.Type ()] -> Surface.Type ()
tupleType tys = Surface.Type{ty = Surface.TyTupleF (fromJust $ NE.fromList tys), ann = ()}

literalInt :: Integer -> Surface.Expression ()
literalInt n = Surface.Expression{expr = Surface.ELiteral (Surface.LitInteger n), ann = ()}

nonPub :: Maybe (Surface.Pub ())
nonPub = Nothing

spec :: Spec
spec = describe "Module parser (minimal subset)" do
  it "parses mod declaration 'mod m;'" do
    testParser "mod m;" pModDefinitionBody $ shouldBeParsed (`shouldBe` moduleBody [modDecl "m"])

  it "parses empty mod definition 'mod m { }'" do
    testParser "mod m { }" pModDefinitionBody $ shouldBeParsed (`shouldBe` moduleBody [modDef "m" []])

  it "parses use leaf 'use std::io;'" do
    let expected =
          moduleBody
            [ Surface.ModuleItemF
                { pub = Nothing
                , item = Surface.ModItemUseF (useClauseLeaf ["std", "io"])
                , ann = ()
                }
            ]
    testParser "use std::io;" pModDefinitionBody $ shouldBeParsed (`shouldBe` expected)

  it "parses use leaf-as 'use std::io as io;'" do
    let expected =
          moduleBody
            [ Surface.ModuleItemF
                { pub = Nothing
                , item = Surface.ModItemUseF (useClauseAs ["std", "io"] "io")
                , ann = ()
                }
            ]
    testParser "use std::io as io;" pModDefinitionBody $ shouldBeParsed (`shouldBe` expected)

  it "parses nested use 'use std::{io, fs::{read, write}};'" do
    let nestedTree =
          Surface.UseTrNested
            ( Vector.fromList
                [ Surface.UseTrLeafIdent
                    Surface.UseTreeLeaf
                      { use = Surface.Identifier{name = "io", ann = ()}
                      , as = Nothing
                      , ann = ()
                      }
                , Surface.UseTrBranch
                    (modIdent "fs")
                    ( Surface.UseTrNested $
                        Surface.UseTrLeafIdent
                          <$> Vector.fromList
                            [ Surface.UseTreeLeaf
                                { use = Surface.Identifier{name = "read", ann = ()}
                                , as = Nothing
                                , ann = ()
                                }
                            , Surface.UseTreeLeaf
                                { use = Surface.Identifier{name = "write", ann = ()}
                                , as = Nothing
                                , ann = ()
                                }
                            ]
                    )
                ]
            )
        useClause =
          Surface.UseClause
            { root = Surface.UsClPackage (modIdent "std")
            , tree = nestedTree
            , ann = ()
            }
    testParser "use std::{io, fs::{read, write}};" pModDefinitionBody $
      shouldBeParsed
        ( `shouldBe`
            moduleBody
              [ Surface.ModuleItemF
                  { pub = Nothing
                  , item = Surface.ModItemUseF useClause
                  , ann = ()
                  }
              ]
        )

  it "parses minimal items: struct, enum, type alias, fn, let" do
    let src =
          Text.unlines
            [ "struct S {}"
            , "enum E { A, B }"
            , "type Pair<X, Y> = (X, Y);"
            , "fn add(a: i32, b: i32) -> i32 { }"
            , "let x: i32 = 42;"
            ]
        items =
          [ structItem nonPub "S"
          , enumItem nonPub "E" ["A", "B"]
          , typeAliasItem nonPub "Pair" (tupleType [simpleType "X", simpleType "Y"])
          , fnItem
              nonPub
              "add"
              [
                ( False
                , "a"
                , Surface.Type
                    { ty =
                        Surface.TyIdentifierF
                          ( Surface.QualifiedIdentifierF
                              { qualifierPrefix = Nothing
                              , qualifier = Nothing
                              , typeQualifier = Nothing
                              , identifier = Surface.Identifier{name = "i32", ann = ()}
                              , ann = ()
                              }
                          )
                    , ann = ()
                    }
                )
              ,
                ( False
                , "b"
                , Surface.Type
                    { ty =
                        Surface.TyIdentifierF
                          ( Surface.QualifiedIdentifierF
                              { qualifierPrefix = Nothing
                              , qualifier = Nothing
                              , typeQualifier = Nothing
                              , identifier = Surface.Identifier{name = "i32", ann = ()}
                              , ann = ()
                              }
                          )
                    , ann = ()
                    }
                )
              ]
              Nothing
              ( Just
                  Surface.Type
                    { ty =
                        Surface.TyIdentifierF
                          ( Surface.QualifiedIdentifierF
                              { qualifierPrefix = Nothing
                              , qualifier = Nothing
                              , typeQualifier = Nothing
                              , identifier = Surface.Identifier{name = "i32", ann = ()}
                              , ann = ()
                              }
                          )
                    , ann = ()
                    }
              )
          , letItem
              nonPub
              "x"
              ( Surface.Type
                  { ty = Surface.TyIdentifierF (Surface.QualifiedIdentifierF{qualifierPrefix = Nothing, qualifier = Nothing, typeQualifier = Nothing, identifier = Surface.Identifier{name = "i32", ann = ()}, ann = ()})
                  , ann = ()
                  }
              )
              (literalInt 42)
          ]
    testParser src pModDefinitionBody $ shouldBeParsed (`shouldBe` moduleBody items)
