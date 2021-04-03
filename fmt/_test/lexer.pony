use "ponytest"
use ".."

primitive LexerTests is TestList
  fun tag tests(test: PonyTest) =>
    test(EmptySourceTest)
    test(MultiLineStringTest)
    test(SingleLineCommentTest)
    test(StringTest)
    test(SymbolTest)


class iso EmptySourceTest is UnitTest
  fun name(): String => "lexer/empty"

  fun apply(h: TestHelper) =>
    let lexer =  Lexer(Source.from_string(""))
    TokenAssert.is_eof(h, lexer.next(), 1, 1)


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


class iso StringTest is UnitTest
  fun name(): String => "lexer/string"

  fun apply(h: TestHelper) =>
    let lexer = Lexer(Source.from_string("\"Lorem Ipsum\""))

    let first_token = lexer.next()
    TokenAssert.is_abstract(h, first_token, "string", 1, 1)
    TokenAssert.has_text(h, first_token, "\"Lorem Ipsum\"")

    TokenAssert.is_eof(h, lexer.next(), 1, 14)


class iso SymbolTest is UnitTest
  fun name(): String => "lexer/symbol"

  fun apply(h: TestHelper) =>
    let lexer = Lexer(Source.from_string("=>"))

    let first_token = lexer.next()
    TokenAssert.is_abstract(h, first_token, "dblarrow", 1, 1)

    TokenAssert.is_eof(h, lexer.next(), 1, 3)


primitive TokenAssert
  fun is_abstract(h: TestHelper, token: Token, id: String, lineno: USize, pos: USize) =>
    h.assert_eq[String](id, token.id)
    h.assert_eq[USize](lineno, token.lineno)
    h.assert_eq[USize](pos, token.pos)
    h.assert_true(token.kind is Abstract)

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
