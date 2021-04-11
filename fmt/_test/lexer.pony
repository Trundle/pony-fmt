use "ponytest"
use ".."

primitive LexerTests is TestList
  fun tag tests(test: PonyTest) =>
    test(BinarySourceTest)
    test(EmptySourceTest)
    test(HexTest)
    test(IntTest)
    test(MultiLineStringTest)
    test(RealTest)
    test(SingleLineCommentTest)
    test(StringTest)
    test(SymbolTest)
    test(WhitespaceTest)


class iso BinarySourceTest is UnitTest
  fun name(): String  => "lexer/binary"

  fun apply(h: TestHelper) =>
    test_literal(h, "0b10100")
    test_literal(h, "0B10111")

  fun test_literal(h: TestHelper, source: String) =>
    let lexer = Lexer(Source.from_string(source))

    let first_token = lexer.next()
    TokenAssert.is_abstract(h, first_token, "int", 1, 1)
    TokenAssert.has_text(h, first_token, source)

    TokenAssert.is_eof(h, lexer.next(), 1, source.size() + 1)


class iso EmptySourceTest is UnitTest
  fun name(): String => "lexer/empty"

  fun apply(h: TestHelper) =>
    let lexer =  Lexer(Source.from_string(""))
    TokenAssert.is_eof(h, lexer.next(), 1, 1)


class iso HexTest is UnitTest
  fun name(): String => "lexer/hex"

  fun apply(h: TestHelper) =>
    test_literal(h, "0x42")
    test_literal(h, "0X42")

  fun test_literal(h: TestHelper, source: String) =>
    let lexer = Lexer(Source.from_string(source))

    let first_token = lexer.next()
    TokenAssert.is_abstract(h, first_token, "int", 1, 1)
    TokenAssert.has_text(h, first_token, source)

    TokenAssert.is_eof(h, lexer.next(), 1, source.size() + 1)


class iso IntTest is UnitTest
  fun name(): String => "lexer/int"

  fun apply(h: TestHelper) =>
    test_literal(h, "0")
    test_literal(h, "1")
    test_literal(h, "123")
    test_literal(h, "1_234")

  fun test_literal(h: TestHelper, source: String) =>
    let lexer = Lexer(Source.from_string(source))

    let first_token = lexer.next()
    TokenAssert.is_abstract(h, first_token, "int", 1, 1)
    TokenAssert.has_text(h, first_token, source)

    TokenAssert.is_eof(h, lexer.next(), 1, source.size() + 1)



 class iso SingleLineCommentTest is UnitTest
   fun name(): String => "lexer/single line comment"

   fun apply(h: TestHelper) =>
     let lexer = Lexer(Source.from_string("// lorem ipsum"))

     let first_token = lexer.next()
     h.assert_eq[String]("comment", first_token.id)
     h.assert_eq[USize](1, first_token.lineno)
     h.assert_eq[USize](1, first_token.pos)
     h.assert_true(first_token.kind is Concrete)

     TokenAssert.is_eof(h, lexer.next(), 1, 15)


class iso MultiLineStringTest is UnitTest
  fun name(): String => "lexer/multi-line string"

  fun apply(h: TestHelper) =>
    let source = "\"\"\"\n  Some multi-line string\n\"\"\""
    let lexer = Lexer(Source.from_string(source))

    let first_token = lexer.next()
    TokenAssert.is_abstract(h, first_token, "string", 1, 1)
    TokenAssert.has_text(h, first_token, source)

    TokenAssert.is_eof(h, lexer.next(), 3, 4)


class iso RealTest is UnitTest
  fun name(): String => "lexer/real"

  fun apply(h: TestHelper)=>
    test_literal(h, "1.0")
    test_literal(h, "12.345")

  fun test_literal(h: TestHelper, source: String) =>
    let lexer = Lexer(Source.from_string(source))

    let first_token = lexer.next()
    TokenAssert.is_abstract(h, first_token, "float", 1, 1)
    TokenAssert.has_text(h, first_token, source)

    TokenAssert.is_eof(h, lexer.next(), 1, source.size() + 1)

class iso StringTest is UnitTest
  fun name(): String => "lexer/string"

  fun apply(h: TestHelper) =>
    test_literal(h, "\"Lorem Ipsum\"")
    test_literal(h, "\"\\\"\\\"\\\"\"")

  fun test_literal(h: TestHelper, source: String) =>
    let lexer = Lexer(Source.from_string(source))

    let first_token = lexer.next()
    TokenAssert.is_abstract(h, first_token, "string", 1, 1)
    TokenAssert.has_text(h, first_token, source)

    TokenAssert.is_eof(h, lexer.next(), 1, source.size() + 1)


class iso SymbolTest is UnitTest
  fun name(): String => "lexer/symbol"

  fun apply(h: TestHelper) =>
    let lexer = Lexer(Source.from_string("=>"))

    let first_token = lexer.next()
    TokenAssert.is_abstract(h, first_token, "dblarrow", 1, 1)

    TokenAssert.is_eof(h, lexer.next(), 1, 3)


class iso WhitespaceTest is UnitTest
  fun name(): String => "lexer/whitespace"

  fun apply(h: TestHelper) =>
    let lexer = Lexer(Source.from_string("// foobar\nprimitive Spam"))

    var token: Token = lexer.next()
    TokenAssert.is_concrete(h, token, "comment", 1, 1)
    TokenAssert.has_text(h, token, "// foobar")

    token = lexer.next()
    TokenAssert.is_concrete(h, token, "whitespace", 2, 1)
    TokenAssert.has_text(h, token, "\n")

    token = lexer.next()
    TokenAssert.is_abstract(h, token, "primitive", 2, 1)
    TokenAssert.has_text(h, token, "primitive")

    token = lexer.next()
    TokenAssert.is_concrete(h, token, "whitespace", 2, 11)
    TokenAssert.has_text(h, token, " ")

    token = lexer.next()
    TokenAssert.is_abstract(h, token, "id", 2, 11)
    TokenAssert.has_text(h, token, "Spam")

    TokenAssert.is_abstract(h, lexer.next(), "eof", 2, 15)


primitive TokenAssert
  fun is_abstract(h: TestHelper, token: Token, id: String, lineno: USize, pos: USize) =>
    h.assert_eq[String](id, token.id)
    h.assert_eq[USize](lineno, token.lineno)
    h.assert_eq[USize](pos, token.pos)
    h.assert_true(token.kind is Abstract)

  fun is_concrete(h: TestHelper, token: Token, id: String, lineno: USize, pos: USize) =>
    h.assert_eq[String](id, token.id)
    h.assert_eq[USize](lineno, token.lineno)
    h.assert_eq[USize](pos, token.pos)
    h.assert_true(token.kind is Concrete)

  fun is_eof(h: TestHelper, token: Token, lineno: USize, pos: USize) =>
    h.assert_eq[String]("eof", token.id)
    h.assert_eq[USize](lineno, token.lineno)
    h.assert_eq[USize](pos, token.pos)
    h.assert_true(token.kind is Abstract)

  fun has_text(h: TestHelper, token: Token, expected: String) =>
    match token.text
    | let text: String => h.assert_eq[String](expected, text)
    else h.fail("Expected token text, but got none")
    end
