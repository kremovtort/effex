module Flow.AST.Surface.Mod where

import "base" GHC.Generics (Generic)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)

import Flow.AST.Surface.Common (Identifier, Pub)
import Flow.AST.Surface.Core (
  EffectF,
  FnDefinitionF,
  FnInfixDefinitionF,
  ImplF,
  LetDefinitionF,
  TraitF,
 )
import Flow.AST.Surface.Data (EnumF, StructF)
import Flow.AST.Surface.Type (TypeDefinitionF)
import Flow.AST.Surface.Use (UseClause)

--- module

data Mod ann = Mod
  { mod :: ModF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ModF ann
  = ModDeclarationF (Identifier ann)
  | ModDefinitionF (Identifier ann) (ModDefinitionBodyF ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

newtype ModDefinitionBodyF ann = ModDefinitionBodyF
  { items :: Vector (ModuleItemF ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (ToExpr)

data ModuleItemF ann
  = ModuleItemF
  { pub :: Maybe (Pub ann)
  , item :: ModuleItemVariantF ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ModuleItemVariantF ann
  = ModItemModF (Mod ann)
  | ModItemStructF (StructF ann)
  | ModItemEnumF (EnumF ann)
  | ModItemTraitF (TraitF ann)
  | ModItemImplF (ImplF ann)
  | ModItemEffectF (EffectF ann)
  | ModItemTypeAliasF (TypeDefinitionF ann)
  | ModItemFnF (FnDefinitionF ann)
  | ModItemFnInfixF (FnInfixDefinitionF ann)
  | ModItemLetF (LetDefinitionF ann)
  | ModItemUseF (UseClause ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
