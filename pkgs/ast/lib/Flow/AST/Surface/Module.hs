module Flow.AST.Surface.Module where

import "base" GHC.Generics (Generic)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

import Flow.AST.Surface.Callable (FnDefinitionF, FnInfixDefinitionF)
import Flow.AST.Surface.Common (Identifier, Pub)
import Flow.AST.Surface.Constraint (TypeDefinitionF)
import Flow.AST.Surface.Decl qualified as Decl
import Flow.AST.Surface.Syntax (LetDefinitionF)
import Flow.AST.Surface.Use (UseClause)

data ModF mod stmt simPat ty expr ann
  = ModDeclarationF (Identifier ann)
  | ModDefinitionF (Identifier ann) (ModDefinitionBodyF mod stmt simPat ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

newtype ModDefinitionBodyF mod stmt simPat ty expr ann = ModDefinitionBodyF
  { items :: Vector (ModuleItemF mod stmt simPat ty expr ann)
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (ToExpr)


data ModuleItemF mod stmt simPat ty expr ann
  = ModuleItemF
  { pub :: Maybe (Pub ann)
  , item :: ModuleItemVariantF mod stmt simPat ty expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ModuleItemVariantF mod stmt simPat ty expr ann
  = ModItemModF (mod ann)
  | ModItemStructF (Decl.StructF ty ann)
  | ModItemEnumF (Decl.EnumF ty ann)
  | ModItemTraitF (Decl.TraitF stmt ty expr ann)
  | ModItemImplF (Decl.ImplF stmt simPat ty expr ann)
  | ModItemEffectF (Decl.EffectF stmt ty expr ann)
  | ModItemTypeAliasF (TypeDefinitionF ty ann)
  | ModItemFnF (FnDefinitionF stmt ty expr ann)
  | ModItemFnInfixF (FnInfixDefinitionF stmt ty expr ann)
  | ModItemLetF (LetDefinitionF simPat ty expr ann)
  | ModItemUseF (UseClause ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
