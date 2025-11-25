module Main (main) where

import "hspec" Test.Hspec (hspec)

import Effex.LexerSpec qualified
import Effex.Parser.TypeSpec qualified
import Effex.Parser.PatternSpec qualified
import Effex.Parser.LHSExpressionSpec qualified
import Effex.Parser.ExpressionSpec qualified
import Effex.Parser.ModuleSpec qualified

main :: IO ()
main = hspec do
  Effex.LexerSpec.spec
  Effex.Parser.TypeSpec.spec
  Effex.Parser.PatternSpec.spec
  Effex.Parser.LHSExpressionSpec.spec
  Effex.Parser.ExpressionSpec.spec
  Effex.Parser.ModuleSpec.spec
