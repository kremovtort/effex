{-# LANGUAGE DuplicateRecordFields #-}

module Effex.Core.Package where

import "base" GHC.Generics (Generic)
import "effectful" Effectful.State.Static.Local (execState, gets, modify)
import "generic-lens" Data.Generics.Labels ()
import "hashable" Data.Hashable (Hashable)
import "lens" Control.Lens (to, (%~), (^.))
import "text" Data.Text (Text)

import Control.Monad (when)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.TreeDiff (ToExpr)
import Data.Vector qualified as Vector
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effex.AST.Surface (Expression, Mod, PatternSimple, Statement, Type)
import Effex.AST.Surface.Callable (
  FnDeclarationF,
  FnDefinitionF,
  FnInfixDeclarationF,
  FnInfixDefinitionF,
  OpDeclarationF,
  OpDefinitionF,
  OpInfixDeclarationF,
  OpInfixDefinitionF,
 )
import Effex.AST.Surface.Common (Pub (..), SimpleTypeIdentifier (..), SimpleVarIdentifier (..))
import Effex.AST.Surface.Decl (
  EffectF,
  EnumF,
  EnumVariantF,
  EnumVariantGeneralizedF,
  FieldDeclF (..),
  FieldsDeclF (..),
  StructF (..),
  TraitF,
  TypeDeclarationF,
 )
import Effex.AST.Surface.Module (
  ModDefinitionBodyF (..),
  ModuleItemF (..),
  ModuleItemVariantF (..),
 )
import Effex.AST.Surface.Syntax (LetDefinitionF)
import Effex.Core.Module (BasicAnn, ModuleDefinition, Visibility (..), resolveVisibility)
import Effex.Core.Module qualified as Module
import GHC.Stack (HasCallStack)

data AstPhase

type Key a = (Eq a, Ord a, Hashable a)

class
  ( Key (PackageId a)
  , Key (ModuleId a)
  , Key (TypeId a)
  , Key (ConstructorId a)
  , Key (VarId a)
  , Key (MethodId a)
  ) =>
  Phase a
  where
  data PackageId a
  data ModuleId a
  data TypeId a
  data ConstructorId a
  data VarId a
  data MethodId a

  data Module a
  data Struct a
  data Enum_ a
  data Constructor a
  data Trait a
  data Effect a
  data TypeAlias a
  data TraitTypeFamily a
  data EffectTypeFamily a
  data Var a
  data Accessor a
  data Fn a
  data Method a

data TraitItem fn = TraitFn
  { traitId :: TypeId AstPhase
  , fn :: fn
  }
  deriving (Eq, Ord, Show, Generic, ToExpr)

data EffectItem op = EffectOp
  { effectId :: TypeId AstPhase
  , op :: op
  }
  deriving (Eq, Ord, Show, Generic, ToExpr)

data ConstructorVariant
  = ConsVarStruct (StructF Type BasicAnn)
  | ConsVarEnumSimple (EnumVariantF Type BasicAnn)
  | ConsVarEnumGeneralized (EnumVariantGeneralizedF Type BasicAnn)
  deriving (Eq, Ord, Show, Generic, ToExpr)

data FnVariant
  = FnVarFnDef (FnDefinitionF Statement Type Expression BasicAnn)
  | FnVarTraitFnDecl (TraitItem (FnDeclarationF Type BasicAnn))
  | FnVarTraitFnDef (TraitItem (FnDefinitionF Statement Type Expression BasicAnn))
  | FnVarOpDecl (EffectItem (OpDeclarationF Type BasicAnn))
  | FnVarOpDef (EffectItem (OpDefinitionF Statement Type Expression BasicAnn))
  deriving (Eq, Ord, Show, Generic, ToExpr)

data MethodVariant
  = MetVarMethodDef (FnInfixDefinitionF Statement Type Expression BasicAnn)
  | MetVarTraitMethodDecl (TraitItem (FnInfixDeclarationF Type BasicAnn))
  | MetVarTraitMethodDef (TraitItem (FnInfixDefinitionF Statement Type Expression BasicAnn))
  | MetVarOpMethodDecl (EffectItem (OpInfixDeclarationF Type BasicAnn))
  | MetVarOpMethodDef (EffectItem (OpInfixDefinitionF Statement Type Expression BasicAnn))
  deriving (Eq, Ord, Show, Generic, ToExpr)

instance Phase AstPhase where
  newtype ModuleId AstPhase = ModuleId
    { moduleId :: Module.ModuleId
    }
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (ToExpr, Hashable)

  newtype PackageId AstPhase = PackageId
    { package :: Module.PackageId
    }
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (ToExpr, Hashable)

  data TypeId AstPhase = TypeId
    { moduleId :: ModuleId AstPhase
    , name :: Text
    }
    deriving (Eq, Ord, Show, Generic, Hashable, ToExpr)

  data VarId AstPhase = VarId
    { moduleId :: ModuleId AstPhase
    , name :: Text
    }
    deriving (Eq, Ord, Show, Generic, Hashable, ToExpr)

  data ConstructorId AstPhase = ConstructorId
    { moduleId :: ModuleId AstPhase
    , name :: Text
    }
    deriving (Eq, Ord, Show, Generic, Hashable, ToExpr)

  data MethodId AstPhase = MethodId
    { moduleId :: ModuleId AstPhase
    , name :: Text
    }
    deriving (Eq, Ord, Show, Generic, Hashable, ToExpr)

  newtype Module AstPhase = Module
    { moduleDef :: ModuleDefinition
    }
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (ToExpr)

  data Struct AstPhase = Struct
    { visibility :: Visibility
    , typeId :: TypeId AstPhase
    , ast :: StructF Type BasicAnn
    }
    deriving (Eq, Ord, Show, Generic, ToExpr)

  data Enum_ AstPhase = Enum_
    { visibility :: Visibility
    , typeId :: TypeId AstPhase
    , ast :: EnumF Type BasicAnn
    }
    deriving (Eq, Ord, Show, Generic, ToExpr)

  data Constructor AstPhase = Constructor
    { visibility :: Visibility
    , constructorId :: ConstructorId AstPhase
    , ast :: ConstructorVariant
    }
    deriving (Eq, Ord, Show, Generic, ToExpr)

  data Trait AstPhase = Trait
    { visibility :: Visibility
    , typeId :: TypeId AstPhase
    , ast :: TraitF Statement Type Expression BasicAnn
    }
    deriving (Eq, Ord, Show, Generic, ToExpr)

  data Effect AstPhase = Effect
    { visibility :: Visibility
    , typeId :: TypeId AstPhase
    , ast :: EffectF Statement Type Expression BasicAnn
    }
    deriving (Eq, Ord, Show, Generic, ToExpr)

  data TypeAlias AstPhase = TypeAlias
    { visibility :: Visibility
    , typeId :: TypeId AstPhase
    , ast :: TypeDeclarationF Type BasicAnn
    }
    deriving (Eq, Ord, Show, Generic, ToExpr)

  data TraitTypeFamily AstPhase = TraitTypeFamily
    { visibility :: Visibility
    , traitId :: TypeId AstPhase
    , ast :: TypeDeclarationF Type BasicAnn
    }
    deriving (Eq, Ord, Show, Generic, ToExpr)

  data EffectTypeFamily AstPhase = EffectTypeFamily
    { visibility :: Visibility
    , effectId :: TypeId AstPhase
    , ast :: TypeDeclarationF Type BasicAnn
    }
    deriving (Eq, Ord, Show, Generic, ToExpr)

  data Var AstPhase = Var
    { visibility :: Visibility
    , varId :: VarId AstPhase
    , ast :: LetDefinitionF PatternSimple Type Expression BasicAnn
    }
    deriving (Eq, Ord, Show, Generic, ToExpr)

  data Accessor AstPhase = Accessor
    { visibility :: Visibility
    , constructorId :: ConstructorId AstPhase
    , name :: Text
    }
    deriving (Eq, Ord, Show, Generic, ToExpr)

  data Fn AstPhase = Fn
    { visibility :: Visibility
    , varId :: VarId AstPhase
    , fn :: FnVariant
    }
    deriving (Eq, Ord, Show, Generic, ToExpr)

  data Method AstPhase = Method
    { visibility :: Visibility
    , methodId :: MethodId AstPhase
    , ast :: MethodVariant
    }
    deriving (Eq, Ord, Show, Generic, ToExpr)

data PackageSymbols phase f = PackageSymbols
  { modules :: f (ModuleId phase) (Module phase)
  , structs :: f (TypeId phase) (Struct phase)
  , enums :: f (TypeId phase) (Enum_ phase)
  , traits :: f (TypeId phase) (Trait phase)
  , effects :: f (TypeId phase) (Effect phase)
  , typeAliases :: f (TypeId phase) (TypeAlias phase)
  , traitTypeFamilies :: f (TypeId phase) (TraitTypeFamily phase)
  , effectTypeFamilies :: f (TypeId phase) (EffectTypeFamily phase)
  , constructors :: f (ConstructorId phase) (Constructor phase)
  , vars :: f (VarId phase) (Var phase)
  , fns :: f (VarId phase) (Fn phase)
  , accessors :: f (MethodId phase) (Accessor phase)
  , methods :: f (MethodId phase) (Method phase)
  }
  deriving (Generic)

data SymbolsSet = SymbolsSet
  { types :: HashSet (TypeId AstPhase)
  , constructors :: HashSet (ConstructorId AstPhase)
  , methods :: HashSet (MethodId AstPhase)
  , vars :: HashSet (VarId AstPhase)
  }
  deriving (Eq, Ord, Show, Generic, ToExpr)

collectPackageSymbols ::
  (HasCallStack) =>
  Seq ModuleDefinition ->
  Eff es (PackageSymbols AstPhase Map)
collectPackageSymbols modDefs = do
  _

data DuplicateDeclarationError = DuplicateDeclarationError
  { moduleId :: ModuleId AstPhase
  , name :: Text
  , duplicateAnn :: BasicAnn
  }
  deriving (Eq, Show)

collectModuleSymbols ::
  (HasCallStack, Error DuplicateDeclarationError :> es) =>
  ModuleDefinition ->
  Eff es (PackageSymbols AstPhase Map)
collectModuleSymbols modDef = runStates do
  for_ modDef.body.items collectFromItem
  _
 where
  collectFromItem (item :: ModuleItemF Mod Statement PatternSimple Type Expression BasicAnn) =
    case item.item of
      ModItemStructF struct -> collectFromStruct item.pub struct
      ModItemEnumF enum -> _
      ModItemTraitF trait -> _
      ModItemEffectF effect -> _
      ModItemTypeAliasF typeAlias -> _
      ModItemFnF fn -> _
      ModItemFnInfixF fnInfix -> _
      ModItemLetF letDef -> _
      ModItemImplF impl -> _
      ModItemModF _ -> pure ()
      -- ignore at first pass, collect them when we know all symbols
      ModItemUseF _ -> pure ()

  collectFromStruct pub (struct :: StructF Type BasicAnn) = do
    let typeId =
          TypeId
            { moduleId = ModuleId modDef.identifier
            , name = struct.name.name
            }
        struct' =
          Struct
            { visibility = resolveVisibility modDef.visibility (pubToVisibility pub)
            , typeId = typeId
            , ast = struct
            }
        constructorId =
          ConstructorId
            { moduleId = ModuleId modDef.identifier
            , name = struct.name.name
            }
        constructor' =
          Constructor
            { visibility = struct'.visibility
            , constructorId = constructorId
            , ast = ConsVarStruct struct
            }
    ensureTypeNotDefined typeId struct.name.ann
    modify @SymbolsSet (#types %~ HashSet.insert typeId)
    modify @(PackageSymbols AstPhase Map) (#structs %~ Map.insert typeId struct')
    ensureConstructorNotDefined constructorId struct.name.ann
    modify @SymbolsSet (#constructors %~ HashSet.insert constructorId)
    modify @(PackageSymbols AstPhase Map) (#constructors %~ Map.insert constructorId constructor')
    case struct.fields of
      FieldsDeclTupleF _ -> pure ()
      FieldsDeclNamedF fields ->
        for_ fields \field -> do
          let methodId =
                 MethodId
                    { moduleId =
                        ModuleId $
                          modDef.identifier & #path %~ (`Vector.snoc` typeId.name)
                    , name = field.name.name
                    }
              accessor =
                 Accessor
                    { visibility =
                        resolveVisibility struct'.visibility (pubToVisibility field.pub)
                    , constructorId = constructorId
                    , name = field.name.name
                    }
          ensureMethodNotDefined methodId field.ann
          modify @SymbolsSet (#methods %~ HashSet.insert methodId)
          modify @(PackageSymbols AstPhase Map) (#accessors %~ Map.insert methodId accessor)

  ensureTypeNotDefined typeId ann = do
    alreadyDefined <- gets @SymbolsSet (^. #types . to (HashSet.member typeId))
    when alreadyDefined do
      throwError $
        DuplicateDeclarationError
          { moduleId = typeId.moduleId
          , name = typeId.name
          , duplicateAnn = ann
          }

  ensureConstructorNotDefined constructorId ann = do
    alreadyDefined <- gets @SymbolsSet (^. #constructors . to (HashSet.member constructorId))
    when alreadyDefined do
      throwError $
        DuplicateDeclarationError
          { moduleId = constructorId.moduleId
          , name = constructorId.name
          , duplicateAnn = ann
          }

  ensureMethodNotDefined methodId ann = do
    alreadyDefined <- gets @SymbolsSet (^. #methods . to (HashSet.member methodId))
    when alreadyDefined do
      throwError $
        DuplicateDeclarationError
          { moduleId = methodId.moduleId
          , name = methodId.name
          , duplicateAnn = ann
          }

  runStates = execState emptyPackageSymbols . execState emptySymbolsSet

  pubToVisibility pub = case pub of
    Nothing -> VisibilityModule modDef.identifier
    Just (PubPackage _) -> VisibilityPackage
    Just PubPub -> VisibilityAll

  emptySymbolsSet =
    SymbolsSet
      { types = mempty
      , constructors = mempty
      , methods = mempty
      , vars = mempty
      }

  emptyPackageSymbols =
    PackageSymbols
      { modules = mempty
      , structs = mempty
      , enums = mempty
      , traits = mempty
      , effects = mempty
      , typeAliases = mempty
      , traitTypeFamilies = mempty
      , effectTypeFamilies = mempty
      , constructors = mempty
      , vars = mempty
      , fns = mempty
      , accessors = mempty
      , methods = mempty
      }
