module Effex.AST.Surface.Operators where

import "base" GHC.Generics (Generic)
import "tree-diff" Data.TreeDiff.Class (ToExpr)

import Effex.AST.Surface.Common (RegionIdentifier)

data UnOp ann
  = UnOpNot ann
  | UnOpNeg ann
  | UnOpDeref ann
  | UnOpTakeRef (Maybe (RegionIdentifier ann)) ann
  | UnOpTakeMutRef (Maybe (RegionIdentifier ann)) ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)

data BinOp ann
  = BinOpAdd ann
  | BinOpSub ann
  | BinOpMul ann
  | BinOpDiv ann
  | BinOpMod ann
  | BinOpAnd ann
  | BinOpOr ann
  | BinOpLessThan ann
  | BinOpLessThanOrEqual ann
  | BinOpGreaterThan ann
  | BinOpGreaterThanOrEqual ann
  | BinOpEqual ann
  | BinOpNotEqual ann
  | BinOpConcat ann
  | BinOpBitwiseAnd ann
  | BinOpBitwiseOr ann
  | BinOpBitwiseShiftLeft ann
  | BinOpBitwiseShiftRight ann
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, ToExpr)
