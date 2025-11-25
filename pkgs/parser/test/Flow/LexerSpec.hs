module Effex.LexerSpec (spec) where

import "base" Data.Word (Word8)
import "bytestring" Data.ByteString (ByteString)
import "hspec" Test.Hspec (Spec, describe, it)
import "hspec-megaparsec" Test.Hspec.Megaparsec (
  shouldFailOn,
  shouldParse,
 )
import "megaparsec" Text.Megaparsec (parse)
import "scientific" Data.Scientific (fromFloatDigits)
import "text" Data.Text (Text)
import "vector" Data.Vector (Vector)
import "vector" Data.Vector qualified as Vector

import Effex.Lexer

spec :: Spec
spec = do
  keywordSpec
  punctuationSpec
  identifierSpec
  dotIdentifierSpec
  refRegionSpec
  opticSpec
  boolLiteralSpec
  integerLiteralSpec
  floatLiteralSpec
  charLiteralSpec
  stringLiteralSpec
  byteLiteralSpec
  byteStringLiteralSpec
  whitespaceAndCommentsSpec
  combinedTokensSpec
  negativeTestsSpec

-- Helper function to create single-token vector
singleToken :: Token -> Vector Token
singleToken t = Vector.fromList [t]

-- | Test all keywords
keywordSpec :: Spec
keywordSpec = describe "Keywords" do
  it "parses 'do' as identifier (not a keyword)" $
    parse tokens "" "do" `shouldParse` singleToken (Identifier "do")

  it "parses 'effect'" $
    parse tokens "" "effect" `shouldParse` singleToken (Keyword Effect)

  it "parses 'else'" $
    parse tokens "" "else" `shouldParse` singleToken (Keyword Else)

  it "parses 'enum'" $
    parse tokens "" "enum" `shouldParse` singleToken (Keyword Enum)

  it "parses 'fn'" $
    parse tokens "" "fn" `shouldParse` singleToken (Keyword Fn)

  it "parses 'for'" $
    parse tokens "" "for" `shouldParse` singleToken (Keyword For)

  it "parses 'handle'" $
    parse tokens "" "handle" `shouldParse` singleToken (Keyword Handle)

  it "parses 'if'" $
    parse tokens "" "if" `shouldParse` singleToken (Keyword If)

  it "parses 'impl'" $
    parse tokens "" "impl" `shouldParse` singleToken (Keyword Impl)

  it "parses 'in'" $
    parse tokens "" "in" `shouldParse` singleToken (Keyword In)

  it "parses 'let'" $
    parse tokens "" "let" `shouldParse` singleToken (Keyword Let)

  it "parses 'loop'" $
    parse tokens "" "loop" `shouldParse` singleToken (Keyword Loop)

  it "parses 'match'" $
    parse tokens "" "match" `shouldParse` singleToken (Keyword Match)

  it "parses 'mod'" $
    parse tokens "" "mod" `shouldParse` singleToken (Keyword Mod)

  it "parses 'mut'" $
    parse tokens "" "mut" `shouldParse` singleToken (Keyword Mut)

  it "parses 'op'" $
    parse tokens "" "op" `shouldParse` singleToken (Keyword Op)

  it "parses 'pub'" $
    parse tokens "" "pub" `shouldParse` singleToken (Keyword Pub)

  it "parses 'return'" $
    parse tokens "" "return" `shouldParse` singleToken (Keyword Return)

  it "parses 'returning'" $
    parse tokens "" "returning" `shouldParse` singleToken (Keyword Returning)

  it "parses 'struct'" $
    parse tokens "" "struct" `shouldParse` singleToken (Keyword Struct)

  it "parses 'trait'" $
    parse tokens "" "trait" `shouldParse` singleToken (Keyword Trait)

  it "parses 'type'" $
    parse tokens "" "type" `shouldParse` singleToken (Keyword Type)

  it "parses 'use'" $
    parse tokens "" "use" `shouldParse` singleToken (Keyword Use)

  it "parses 'where'" $
    parse tokens "" "where" `shouldParse` singleToken (Keyword Where)

  it "parses 'while'" $
    parse tokens "" "while" `shouldParse` singleToken (Keyword While)

  it "parses 'break'" $
    parse tokens "" "break" `shouldParse` singleToken (Keyword Break)

  it "parses 'continue'" $
    parse tokens "" "continue" `shouldParse` singleToken (Keyword Continue)

  it "parses 'as'" $
    parse tokens "" "as" `shouldParse` singleToken (Keyword As)

  it "does not parse keywords followed by alphanumeric" $
    parse tokens "" "ifx" `shouldParse` singleToken (Identifier "ifx")

  it "does not parse keywords followed by underscore" $
    parse tokens "" "let_" `shouldParse` singleToken (Identifier "let_")

-- | Test all punctuation
punctuationSpec :: Spec
punctuationSpec = describe "Punctuation" do
  it "parses '='" $
    parse tokens "" "=" `shouldParse` singleToken (Punctuation Assign)

  it "parses '<'" $
    parse tokens "" "<" `shouldParse` singleToken (Punctuation LessThan)

  it "parses '<='" $
    parse tokens "" "<=" `shouldParse` singleToken (Punctuation LessThanOrEqual)

  it "parses '=='" $
    parse tokens "" "==" `shouldParse` singleToken (Punctuation Equal)

  it "parses '!='" $
    parse tokens "" "!=" `shouldParse` singleToken (Punctuation NotEqual)

  it "parses '>='" $
    parse tokens "" ">=" `shouldParse` singleToken (Punctuation GreaterThanOrEqual)

  it "parses '>'" $
    parse tokens "" ">" `shouldParse` singleToken (Punctuation GreaterThan)

  it "parses '&&'" $
    parse tokens "" "&&" `shouldParse` singleToken (Punctuation And)

  it "parses '||'" $
    parse tokens "" "||" `shouldParse` singleToken (Punctuation Or)

  it "parses '!'" $
    parse tokens "" "!" `shouldParse` singleToken (Punctuation Not)

  it "parses '~'" $
    parse tokens "" "~" `shouldParse` singleToken (Punctuation Tilde)

  it "parses '+'" $
    parse tokens "" "+" `shouldParse` singleToken (Punctuation Plus)

  it "parses '++'" $
    parse tokens "" "++" `shouldParse` singleToken (Punctuation Concat)

  it "parses '-'" $
    parse tokens "" "-" `shouldParse` singleToken (Punctuation Minus)

  it "parses '*'" $
    parse tokens "" "*" `shouldParse` singleToken (Punctuation Star)

  it "parses '/'" $
    parse tokens "" "/" `shouldParse` singleToken (Punctuation Slash)

  it "parses '%'" $
    parse tokens "" "%" `shouldParse` singleToken (Punctuation Percent)

  it "parses '^'" $
    parse tokens "" "^" `shouldParse` singleToken (Punctuation Caret)

  it "parses '&'" $
    parse tokens "" "&" `shouldParse` singleToken (Punctuation Ampersand)

  it "parses '|'" $
    parse tokens "" "|" `shouldParse` singleToken (Punctuation Pipe)

  it "parses '<<'" $
    parse tokens "" "<<" `shouldParse` singleToken (Punctuation ShiftLeft)

  it "parses '>>'" $
    parse tokens "" ">>" `shouldParse` singleToken (Punctuation ShiftRight)

  it "parses '+='" $
    parse tokens "" "+=" `shouldParse` singleToken (Punctuation PlusAssign)

  it "parses '-='" $
    parse tokens "" "-=" `shouldParse` singleToken (Punctuation MinusAssign)

  it "parses '*='" $
    parse tokens "" "*=" `shouldParse` singleToken (Punctuation StarAssign)

  it "parses '/='" $
    parse tokens "" "/=" `shouldParse` singleToken (Punctuation SlashAssign)

  it "parses '%='" $
    parse tokens "" "%=" `shouldParse` singleToken (Punctuation PercentAssign)

  it "parses '^='" $
    parse tokens "" "^=" `shouldParse` singleToken (Punctuation CaretAssign)

  it "parses '&='" $
    parse tokens "" "&=" `shouldParse` singleToken (Punctuation AmpersandAssign)

  it "parses '|='" $
    parse tokens "" "|=" `shouldParse` singleToken (Punctuation PipeAssign)

  it "parses '<<='" $
    parse tokens "" "<<=" `shouldParse` singleToken (Punctuation ShiftLeftAssign)

  it "parses '>>='" $
    parse tokens "" ">>=" `shouldParse` singleToken (Punctuation ShiftRightAssign)

  it "parses '@'" $
    parse tokens "" "@" `shouldParse` singleToken (Punctuation At)

  it "parses '.'" $
    parse tokens "" ".." `shouldParse` singleToken (Punctuation DotDot)

  it "parses '..'" $
    parse tokens "" ".." `shouldParse` singleToken (Punctuation DotDot)

  it "parses '...'" $
    parse tokens "" "..." `shouldParse` singleToken (Punctuation DotDotDot)

  it "parses '..='" $
    parse tokens "" "..=" `shouldParse` singleToken (Punctuation DotDotEqual)

  it "parses ','" $
    parse tokens "" "," `shouldParse` singleToken (Punctuation Comma)

  it "parses ';'" $
    parse tokens "" ";" `shouldParse` singleToken (Punctuation Semicolon)

  it "parses ':'" $
    parse tokens "" ":" `shouldParse` singleToken (Punctuation Colon)

  it "parses '::'" $
    parse tokens "" "::" `shouldParse` singleToken (Punctuation ColonColon)

  it "parses '->'" $
    parse tokens "" "->" `shouldParse` singleToken (Punctuation Arrow)

  it "parses '<-'" $
    parse tokens "" "<-" `shouldParse` singleToken (Punctuation LeftArrow)

  it "parses '=>'" $
    parse tokens "" "=>" `shouldParse` singleToken (Punctuation FatArrow)

  it "parses '#'" $
    parse tokens "" "#" `shouldParse` singleToken (Punctuation Hash)

  it "parses '$'" $
    parse tokens "" "$" `shouldParse` singleToken (Punctuation Dollar)

  it "parses '?'" $
    parse tokens "" "?" `shouldParse` singleToken (Punctuation Question)

  it "parses '{'" $
    parse tokens "" "{" `shouldParse` singleToken (Punctuation LeftBrace)

  it "parses '}'" $
    parse tokens "" "}" `shouldParse` singleToken (Punctuation RightBrace)

  it "parses '['" $
    parse tokens "" "[" `shouldParse` singleToken (Punctuation LeftBracket)

  it "parses ']'" $
    parse tokens "" "]" `shouldParse` singleToken (Punctuation RightBracket)

  it "parses '('" $
    parse tokens "" "(" `shouldParse` singleToken (Punctuation LeftParen)

  it "parses ')'" $
    parse tokens "" ")" `shouldParse` singleToken (Punctuation RightParen)

-- | Test identifiers
identifierSpec :: Spec
identifierSpec = describe "Identifiers" do
  it "parses simple identifier" $
    parse tokens "" "foo" `shouldParse` singleToken (Identifier "foo")

  it "parses identifier with numbers" $
    parse tokens "" "foo123" `shouldParse` singleToken (Identifier "foo123")

  it "parses identifier with underscores" $
    parse tokens "" "foo_bar" `shouldParse` singleToken (Identifier "foo_bar")

  it "parses identifier starting with underscore" $
    parse tokens "" "_foo" `shouldParse` singleToken (Identifier "_foo")

  it "parses identifier with mixed case" $
    parse tokens "" "fooBar" `shouldParse` singleToken (Identifier "fooBar")

  it "parses identifier with multiple underscores" $
    parse tokens "" "foo__bar__" `shouldParse` singleToken (Identifier "foo__bar__")

  it "does not parse identifier starting with number" $
    parse tokens "" `shouldFailOn` ("123foo" :: Text)

dotIdentifierSpec :: Spec
dotIdentifierSpec = describe "Dot identifiers" do
  it "parses simple dot identifier" $
    parse tokens "" ".bar" `shouldParse` Vector.fromList [Punctuation Dot, Identifier "bar"]

  it "parses dot identifier with numbers" $
    parse tokens "" ".bar123" `shouldParse` Vector.fromList [Punctuation Dot, Identifier "bar123"]

  it "parses dot tuple field identifier" $
    parse tokens "" ".123" `shouldParse` Vector.fromList [Punctuation Dot, IntegerLiteral 123]

-- | Test ref regions
refRegionSpec :: Spec
refRegionSpec = describe "RefRegion" do
  it "parses simple ref region" $
    parse tokens "" "'foo" `shouldParse` singleToken (Region "foo")

  it "parses ref region with numbers" $
    parse tokens "" "'foo123" `shouldParse` singleToken (Region "foo123")

  it "parses ref region with underscores" $
    parse tokens "" "'foo_bar" `shouldParse` singleToken (Region "foo_bar")

  it "parses ref region starting with underscore" $
    parse tokens "" "'_foo" `shouldParse` singleToken (Region "_foo")

  it "does not parse ref region starting with number after apostrophe" $
    parse tokens "" `shouldFailOn` ("'123foo" :: Text)

-- | Test optics
opticSpec :: Spec
opticSpec = describe "Optics" do
  it "parses simple optic" $
    parse tokens "" "#foo" `shouldParse` singleToken (Optic "foo")

  it "parses optic with numbers" $
    parse tokens "" "#foo123" `shouldParse` singleToken (Optic "foo123")

  it "parses optic with underscores" $
    parse tokens "" "#foo_bar" `shouldParse` singleToken (Optic "foo_bar")

  it "parses optic starting with underscore" $
    parse tokens "" "#_foo" `shouldParse` singleToken (Optic "_foo")

  it "parses optic for tuple field" $
    parse tokens "" "#123" `shouldParse` singleToken (Optic "123")

  it "does not parse optic starting with number and followed by letters" $
    parse tokens "" `shouldFailOn` ("#123foo" :: Text)

-- | Test boolean literals
boolLiteralSpec :: Spec
boolLiteralSpec = describe "Boolean literals" do
  it "parses 'true'" $
    parse tokens "" "true" `shouldParse` singleToken (BoolLiteral True)

  it "parses 'false'" $
    parse tokens "" "false" `shouldParse` singleToken (BoolLiteral False)

  it "parses 'true' followed by alphanumeric as identifier" $
    parse tokens "" "truex" `shouldParse` singleToken (Identifier "truex")

  it "parses 'false' followed by underscore as identifier" $
    parse tokens "" "false_" `shouldParse` singleToken (Identifier "false_")

-- | Test integer literals
integerLiteralSpec :: Spec
integerLiteralSpec = describe "Integer literals" do
  describe "Decimal" do
    it "parses simple decimal" $
      parse tokens "" "123" `shouldParse` singleToken (IntegerLiteral 123)

    it "parses decimal with underscores" $
      parse tokens "" "1_000_000" `shouldParse` singleToken (IntegerLiteral 1000000)

    it "parses zero" $
      parse tokens "" "0" `shouldParse` singleToken (IntegerLiteral 0)

  describe "Binary" do
    it "parses binary literal" $
      parse tokens "" "0b1010" `shouldParse` singleToken (IntegerLiteral 10)

    it "parses binary literal with underscores" $
      parse tokens "" "0b1010_1111" `shouldParse` singleToken (IntegerLiteral 175)

    it "does not parse binary with invalid digits" $
      parse tokens "" `shouldFailOn` ("0b102" :: Text)

  describe "Octal" do
    it "parses octal literal" $
      parse tokens "" "0o777" `shouldParse` singleToken (IntegerLiteral 511)

    it "parses octal literal with underscores" $
      parse tokens "" "0o7_7_7" `shouldParse` singleToken (IntegerLiteral 511)

    it "does not parse octal with invalid digits" $
      parse tokens "" `shouldFailOn` ("0o789" :: Text)

  describe "Hexadecimal" do
    it "parses hexadecimal literal" $
      parse tokens "" "0xFF" `shouldParse` singleToken (IntegerLiteral 255)

    it "parses hexadecimal literal with underscores" $
      parse tokens "" "0xDE_AD_BE_EF" `shouldParse` singleToken (IntegerLiteral 3735928559)

    it "parses lowercase hex" $
      parse tokens "" "0xabcdef" `shouldParse` singleToken (IntegerLiteral 11259375)

  it "does not parse integer followed by identifier" $
    parse tokens "" `shouldFailOn` ("123abc" :: Text)

-- | Test float literals
floatLiteralSpec :: Spec
floatLiteralSpec = describe "Float literals" do
  it "parses simple float" $
    parse tokens "" "3.14" `shouldParse` singleToken (FloatLiteral (fromFloatDigits (3.14 :: Double)))

  it "parses float with exponent" $
    parse tokens "" "1.5e10" `shouldParse` singleToken (FloatLiteral (fromFloatDigits (1.5e10 :: Double)))

  it "parses float with negative exponent" $
    parse tokens "" "2.5e-3" `shouldParse` singleToken (FloatLiteral (fromFloatDigits (2.5e-3 :: Double)))

  it "parses integer as scientific notation" $
    parse tokens "" "42e3" `shouldParse` singleToken (FloatLiteral (fromFloatDigits (42e3 :: Double)))

  it "does not parse float followed by identifier" $
    parse tokens "" `shouldFailOn` ("3.14abc" :: Text)

-- | Test character literals
charLiteralSpec :: Spec
charLiteralSpec = describe "Character literals" do
  it "parses simple character" $
    parse tokens "" "'a'" `shouldParse` singleToken (CharLiteral 'a')

  it "parses space character" $
    parse tokens "" "' '" `shouldParse` singleToken (CharLiteral ' ')

  describe "Escape sequences" do
    it "parses newline escape" $
      parse tokens "" "'\\n'" `shouldParse` singleToken (CharLiteral '\n')

    it "parses carriage return escape" $
      parse tokens "" "'\\r'" `shouldParse` singleToken (CharLiteral '\r')

    it "parses tab escape" $
      parse tokens "" "'\\t'" `shouldParse` singleToken (CharLiteral '\t')

    it "parses backslash escape" $
      parse tokens "" "'\\\\'" `shouldParse` singleToken (CharLiteral '\\')

    it "parses null character escape" $
      parse tokens "" "'\\0'" `shouldParse` singleToken (CharLiteral '\0')

    it "parses single quote escape" $
      parse tokens "" "'\\''" `shouldParse` singleToken (CharLiteral '\'')

    it "parses double quote escape" $
      parse tokens "" "'\\\"'" `shouldParse` singleToken (CharLiteral '\"')

  describe "ASCII escape" do
    it "parses ASCII hex escape" $
      parse tokens "" "'\\x41'" `shouldParse` singleToken (CharLiteral 'A')

    it "parses ASCII hex escape lowercase" $
      parse tokens "" "'\\x7f'" `shouldParse` singleToken (CharLiteral '\x7f')

  describe "Unicode escape" do
    it "parses Unicode escape" $
      parse tokens "" "'\\u{1F600}'" `shouldParse` singleToken (CharLiteral '😀')

    it "parses Unicode escape with underscores" $
      parse tokens "" "'\\u{1_F6_00}'" `shouldParse` singleToken (CharLiteral '😀')

    it "parses simple Unicode escape" $
      parse tokens "" "'\\u{41}'" `shouldParse` singleToken (CharLiteral 'A')

  it "does not parse empty character literal" $
    parse tokens "" `shouldFailOn` ("''" :: Text)

-- | Test string literals
stringLiteralSpec :: Spec
stringLiteralSpec = describe "String literals" do
  it "parses simple string" $
    parse tokens "" "\"hello\"" `shouldParse` singleToken (StringLiteral "hello")

  it "parses empty string" $
    parse tokens "" "\"\"" `shouldParse` singleToken (StringLiteral "")

  it "parses string with spaces" $
    parse tokens "" "\"hello world\"" `shouldParse` singleToken (StringLiteral "hello world")

  describe "Escape sequences" do
    it "parses newline escape" $
      parse tokens "" "\"line1\\nline2\"" `shouldParse` singleToken (StringLiteral "line1\nline2")

    it "parses tab escape" $
      parse tokens "" "\"tab\\there\"" `shouldParse` singleToken (StringLiteral "tab\there")

    it "parses backslash escape" $
      parse tokens "" "\"back\\\\slash\"" `shouldParse` singleToken (StringLiteral "back\\slash")

    it "parses quote escape" $
      parse tokens "" "\"say \\\"hi\\\"\"" `shouldParse` singleToken (StringLiteral "say \"hi\"")

  describe "String continuation" do
    it "parses string with line continuation" $
      parse tokens "" "\"hello\\\nworld\"" `shouldParse` singleToken (StringLiteral "hello\nworld")

  describe "Unicode in strings" do
    it "parses Unicode escape in string" $
      parse tokens "" "\"\\u{1F600}\"" `shouldParse` singleToken (StringLiteral "😀")

    it "parses ASCII hex escape in string" $
      parse tokens "" "\"\\x41BC\"" `shouldParse` singleToken (StringLiteral "ABC")

  it "does not parse unclosed string" $
    parse tokens "" `shouldFailOn` ("\"hello" :: Text)

-- | Test byte literals
byteLiteralSpec :: Spec
byteLiteralSpec = describe "Byte literals" do
  it "parses simple byte" $
    parse tokens "" "b'A'" `shouldParse` singleToken (ByteLiteral (65 :: Word8))

  it "parses byte with escape" $
    parse tokens "" "b'\\n'" `shouldParse` singleToken (ByteLiteral (10 :: Word8))

  it "parses byte with hex escape" $
    parse tokens "" "b'\\x41'" `shouldParse` singleToken (ByteLiteral (65 :: Word8))

  it "parses zero byte" $
    parse tokens "" "b'\\0'" `shouldParse` singleToken (ByteLiteral (0 :: Word8))

  it "does not parse non-ASCII character in byte literal" $
    parse tokens "" `shouldFailOn` ("b'ñ'" :: Text)

  it "does not parse unclosed byte literal" $
    parse tokens "" `shouldFailOn` ("b'A" :: Text)

-- | Test byte string literals
byteStringLiteralSpec :: Spec
byteStringLiteralSpec = describe "Byte string literals" do
  it "parses simple byte string" $
    parse tokens "" "b\"hello\"" `shouldParse` singleToken (ByteStringLiteral ("hello" :: ByteString))

  it "parses empty byte string" $
    parse tokens "" "b\"\"" `shouldParse` singleToken (ByteStringLiteral ("" :: ByteString))

  it "parses byte string with escapes" $
    parse tokens "" "b\"\\x41\\x42\"" `shouldParse` singleToken (ByteStringLiteral ("AB" :: ByteString))

  it "parses byte string with continuation" $
    parse tokens "" "b\"hello\\\nworld\"" `shouldParse` singleToken (ByteStringLiteral ("hello\nworld" :: ByteString))

  it "does not parse unclosed byte string" $
    parse tokens "" `shouldFailOn` ("b\"hello" :: Text)

-- | Test whitespace and comments
whitespaceAndCommentsSpec :: Spec
whitespaceAndCommentsSpec = describe "Whitespace and comments" do
  it "skips spaces between tokens" $
    parse tokens "" "   foo   " `shouldParse` singleToken (Identifier "foo")

  it "skips line comment" $
    parse tokens "" "// this is a comment\nfoo" `shouldParse` singleToken (Identifier "foo")

  it "skips block comment" $
    parse tokens "" "/* comment */ foo" `shouldParse` singleToken (Identifier "foo")

  it "skips nested block comment" $
    parse tokens "" "/* outer /* inner */ */ foo" `shouldParse` singleToken (Identifier "foo")

  it "skips multiple line comments" $
    parse tokens "" "// comment 1\n// comment 2\nfoo" `shouldParse` singleToken (Identifier "foo")

  it "skips tabs and newlines" $
    parse tokens "" "\t\n\r\n  foo" `shouldParse` singleToken (Identifier "foo")

  it "handles comment at end of input" $
    parse tokens "" "foo // comment" `shouldParse` singleToken (Identifier "foo")

  it "handles block comment at end of input" $
    parse tokens "" "foo /* comment */" `shouldParse` singleToken (Identifier "foo")

-- | Test combined token sequences
combinedTokensSpec :: Spec
combinedTokensSpec = describe "Combined token sequences" do
  it "parses multiple tokens with spaces" $
    parse tokens "" "let x = 42"
      `shouldParse` Vector.fromList
        [ Keyword Let
        , Identifier "x"
        , Punctuation Assign
        , IntegerLiteral 42
        ]

  it "parses function declaration" $
    parse tokens "" "fn foo(x: i32) -> i32 { }"
      `shouldParse` Vector.fromList
        [ Keyword Fn
        , Identifier "foo"
        , Punctuation LeftParen
        , Identifier "x"
        , Punctuation Colon
        , Identifier "i32"
        , Punctuation RightParen
        , Punctuation Arrow
        , Identifier "i32"
        , Punctuation LeftBrace
        , Punctuation RightBrace
        ]

  it "parses tokens without spaces where unambiguous" $
    parse tokens "" "x+y*z"
      `shouldParse` Vector.fromList
        [ Identifier "x"
        , Punctuation Plus
        , Identifier "y"
        , Punctuation Star
        , Identifier "z"
        ]

  it "parses comparison operators" $
    parse tokens "" "x <= y"
      `shouldParse` Vector.fromList
        [ Identifier "x"
        , Punctuation LessThanOrEqual
        , Identifier "y"
        ]

  it "parses shift operators" $
    parse tokens "" "x << 2"
      `shouldParse` Vector.fromList
        [ Identifier "x"
        , Punctuation ShiftLeft
        , IntegerLiteral 2
        ]

  it "parses with line comments" $
    parse tokens "" "x // comment\n+ y"
      `shouldParse` Vector.fromList
        [ Identifier "x"
        , Punctuation Plus
        , Identifier "y"
        ]

  it "parses with block comments" $
    parse tokens "" "x /* comment */ + y"
      `shouldParse` Vector.fromList
        [ Identifier "x"
        , Punctuation Plus
        , Identifier "y"
        ]

  it "parses string and byte literals together" $
    parse tokens "" "\"hello\" b\"world\""
      `shouldParse` Vector.fromList
        [ StringLiteral "hello"
        , ByteStringLiteral "world"
        ]

  it "parses ref region in context" $
    parse tokens "" "fn<'a> foo(x: &'a i32)"
      `shouldParse` Vector.fromList
        [ Keyword Fn
        , Punctuation LessThan
        , Region "a"
        , Punctuation GreaterThan
        , Identifier "foo"
        , Punctuation LeftParen
        , Identifier "x"
        , Punctuation Colon
        , Punctuation Ampersand
        , Region "a"
        , Identifier "i32"
        , Punctuation RightParen
        ]

  it "parses complex expression with operators" $
    parse tokens "" "a += b * c.d[0]"
      `shouldParse` Vector.fromList
        [ Identifier "a"
        , Punctuation PlusAssign
        , Identifier "b"
        , Punctuation Star
        , Identifier "c"
        , Punctuation Dot
        , Identifier "d"
        , Punctuation LeftBracket
        , IntegerLiteral 0
        , Punctuation RightBracket
        ]

-- | Negative tests for invalid inputs
negativeTestsSpec :: Spec
negativeTestsSpec = describe "Negative tests" do
  describe "Invalid escapes" do
    it "rejects invalid hex escape in char" $
      parse tokens "" `shouldFailOn` ("'\\xGG'" :: Text)

    it "rejects invalid Unicode code point" $
      parse tokens "" `shouldFailOn` ("'\\u{110000}'" :: Text)

    it "rejects invalid byte hex escape" $
      parse tokens "" `shouldFailOn` ("b'\\xZZ'" :: Text)

  describe "Invalid literals" do
    it "rejects invalid binary literal" $
      parse tokens "" `shouldFailOn` ("0b" :: Text)

    it "rejects invalid octal literal" $
      parse tokens "" `shouldFailOn` ("0o" :: Text)

    it "rejects invalid hex literal" $
      parse tokens "" `shouldFailOn` ("0x" :: Text)

  describe "Unclosed literals" do
    it "rejects unclosed string literal" $
      parse tokens "" `shouldFailOn` ("\"hello" :: Text)

    it "rejects unclosed byte literal" $
      parse tokens "" `shouldFailOn` ("b'A" :: Text)

    it "rejects unclosed byte string literal" $
      parse tokens "" `shouldFailOn` ("b\"hello" :: Text)

  describe "Invalid identifiers" do
    it "rejects identifier starting with digit" $
      parse tokens "" `shouldFailOn` ("123abc" :: Text)

  describe "Unclosed comments" do
    it "rejects unclosed block comment" $
      parse tokens "" `shouldFailOn` ("/* unclosed" :: Text)

    it "rejects unclosed nested block comment" $
      parse tokens "" `shouldFailOn` ("/* outer /* inner */" :: Text)
