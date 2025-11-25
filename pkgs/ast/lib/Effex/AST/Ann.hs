{-# OPTIONS_GHC -Wno-orphans #-}

module Effex.AST.Ann where

import Data.TreeDiff (ToExpr (..))
import Data.TreeDiff.Expr qualified as Expr
import Data.TreeDiff.OMap qualified as OMap
import GHC.Generics (Generic)
import "megaparsec" Text.Megaparsec (SourcePos (..), unPos)

data SourceSpan = SourceSpan
  { start :: SourcePos
  , end :: SourcePos
  }
  deriving (Eq, Ord, Show, Generic, ToExpr)

instance ToExpr SourcePos where
  toExpr SourcePos{sourceName, sourceLine, sourceColumn} =
    Expr.Rec "SourcePos" $
      OMap.fromList
        [ ("sourceName", toExpr sourceName)
        , ("sourceLine", toExpr (unPos sourceLine))
        , ("sourceColumn", toExpr (unPos sourceColumn))
        ]
