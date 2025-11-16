module Flow.AST.Surface.Decl where

import "base" GHC.Generics (Generic)
import "nonempty-vector" Data.Vector.NonEmpty (NonEmptyVector)
import "tree-diff" Data.TreeDiff.Class (ToExpr)
import "vector" Data.Vector (Vector)
import "base" Prelude hiding (Enum)

import Flow.AST.Surface.Callable (
  FnDeclarationF,
  FnDefinitionF,
  FnInfixDeclarationF,
  FnInfixDefinitionF,
  OpDeclarationF,
  OpDefinitionF,
  OpInfixDeclarationF,
  OpInfixDefinitionF,
 )
import Flow.AST.Surface.Common (Pub, Identifier)
import Flow.AST.Surface.Constraint (
  BindersWoConstraintsF,
  TypeDefinitionF,
  WhereBlockF, QualifiedIdentifierF, BindersAppF, KindTreeRootF,
 )
import Flow.AST.Surface.Syntax (LetDefinitionF)

data StructF ty ann = StructF
  { name :: Identifier ann
  , typeParams :: Maybe (BindersWoConstraintsF ty ann)
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , fields :: FieldsDeclF ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FieldsDeclF ty ann
  = FieldsDeclNamedF (Vector (FieldDeclF ty ann))
  | FieldsDeclTupleF (Vector (ty ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data FieldDeclF ty ann = FieldDeclF
  { pub :: Maybe (Pub ann)
  , name :: Identifier ann
  , type_ :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumF ty ann = EnumF
  { name :: Identifier ann
  , typeParams :: Maybe (BindersWoConstraintsF ty ann)
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , variants :: EnumVariantsF ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumVariantsF ty ann
  = EVariantsSimpleF (NonEmptyVector (EnumVariantF ty ann))
  | EVariantsGeneralized (NonEmptyVector (EnumVariantGeneralizedF ty ann))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumVariantF ty ann = EnumVariantF
  { name :: Identifier ann
  , fields :: Maybe (FieldsDeclF ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumVariantGeneralizedF ty ann = EnumVariantGeneralizedF
  { name :: Identifier ann
  , typeParams :: Maybe (BindersWoConstraintsF ty ann)
  , fields :: Maybe (FieldsDeclF ty ann)
  , result :: ty ann
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EnumVariantGeneralizedSimpleF ty ann = EnumVariantGeneralizedSimpleF
  { enumVariant :: EnumVariantF ty ann
  , type_ :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TraitF stmt ty expr ann = TraitF
  { sealed :: Bool
  , name :: Identifier ann
  , typeParams :: BindersWoConstraintsF ty ann
  , superTraits :: Maybe (NonEmptyVector (ty ann))
  , traitBody :: Vector (TraitItemF stmt ty expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TraitItemF stmt ty expr ann = TraitItemF
  { pub :: Maybe (Pub ann)
  , item :: TraitItemVariantF stmt ty expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TraitItemVariantF stmt ty expr ann
  = TItemVarTypeDeclarationF (TypeDeclarationF ty ann)
  | TItemVarLetDeclarationF (LetDeclarationF ty ann)
  | TItemVarFnDeclarationF (FnDeclarationF ty ann)
  | TItemVarFnInfixDeclarationF (FnInfixDeclarationF ty ann)
  | TItemVarFnDefinitionF (FnDefinitionF stmt ty expr ann)
  | TItemVarFnInfixDefinitionF (FnInfixDefinitionF stmt ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ImplF stmt simPat ty expr ann = ImplF
  { implParams :: Maybe (BindersWoConstraintsF ty ann)
  , trait :: QualifiedIdentifierF ty ann
  , traitParams :: BindersAppF ty ann
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , body :: Vector (ImplItemVariantF stmt simPat ty expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data ImplItemVariantF stmt simPat ty expr ann
  = IItemVarTypeF (TypeDefinitionF ty ann)
  | IItemVarLetF (LetDefinitionF simPat ty expr ann)
  | IItemVarFnF (FnDefinitionF stmt ty expr ann)
  | IItemVarFnInfixF (FnInfixDefinitionF stmt ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectF stmt ty expr ann = EffectF
  { sealed :: Bool
  , name :: Identifier ann
  , typeParams :: Maybe (BindersWoConstraintsF ty ann)
  , superEffects :: Maybe (NonEmptyVector (ty ann))
  , whereBlock :: Maybe (WhereBlockF ty ann)
  , effectBody :: Vector (EffectItemF stmt ty expr ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectItemF stmt ty expr ann = EffectItemF
  { pub :: Maybe (Pub ann)
  , item :: EffectItemVariantF stmt ty expr ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data EffectItemVariantF stmt ty expr ann
  = EItemVarTypeDeclarationF (TypeDeclarationF ty ann)
  | EItemVarLetDeclarationF (LetDeclarationF ty ann)
  | EItemVarOpDeclarationF (OpDeclarationF ty ann)
  | EItemVarOpInfixDeclarationF (OpInfixDeclarationF ty ann)
  | EItemVarOpDefinitionF (OpDefinitionF stmt ty expr ann)
  | EItemVarOpInfixDefinitionF (OpInfixDefinitionF stmt ty expr ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data TypeDeclarationF ty ann = TypeDeclarationF
  { name :: Identifier ann
  , kindShort :: Maybe (KindTreeRootF ty ann, ann)
  , type_ :: Maybe (ty ann)
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data LetDeclarationF ty ann = LetDeclarationF
  { name :: Identifier ann
  , type_ :: ty ann
  , ann :: ann
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
