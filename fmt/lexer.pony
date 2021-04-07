// This file is more or less a literal translations of Pony's lexer. One
//difference is that it doesn't throw away whitespace but rather preserves it.
//
// Pony is
//   Copyright (C) 2016-2020, The Pony Developers
//   Copyright (c) 2014-2015, Causality Ltd.
// and released under a BSD-2-Clause license.


// There are a few differeneces to ponyc's lexer:
//
// * We don't throw away whitespace and comments. Instead, there are two
//   different kind of tokens: abstract and concrete. Abstract tokens are the
//   ones that ponyc would use. Concrete are things such as comments.
// * Strings are "raw": they contain their surrounding quotes, escapes aren't
//   parsed

use "collections"
use "files"


class val Source
  let path: (String | None)
  let content: String

  new val from_path(path': FilePath)? =>
    path = path'.path

    let chunk_size: USize = 1024 * 1024 * 1
    match OpenFile(path')
    | let file: File =>
      var content': String iso = recover String end
      while file.errno() is FileOK do
        content'.append(file.read(chunk_size))
      end

      content = consume content'
    else error
    end

  new val from_string(source: String) =>
    path = None
    content = source


primitive Keywords
  fun apply(value: String box): String? =>
    """
    Looks a keywoard up by its text value. Returns a token ID.
    """
    match value
    | "compile_intrinsic" => "compile_intrinsic"

    | "use" => "use"
    | "type" => "type"
    | "interface" => "interface"
    | "trait" => "trait"
    | "primitive" => "primitive"
    | "struct" => "struct"
    | "class" => "class"
    | "actor" => "actor"

    | "as" => "as"
    | "is" => "is"
    | "isnt" => "isnt"

    | "var" => "var"
    | "let" => "let"
    | "embed" => "embed"
    | "new" => "new"
    | "fun" => "fun"
    | "be" => "be"

    | "iso" => "iso"
    | "trn" => "trn"
    | "ref" => "ref"
    | "val" => "val"
    | "box" => "box"
    | "tag" => "tag"

    | "this" => "this"
    | "return" => "return"
    | "break" => "break"
    | "continue" => "continue"
    | "consume" => "consume"
    | "recover" => "recover"

    | "if" => "if"
    | "ifdef" => "ifdef"
    | "iftype" => "iftype_set"
    | "then" => "then"
    | "else" => "else"
    | "elseif" => "elseif"
    | "end" => "end"
    | "for" => "for"
    | "in" => "in"
    | "while" => "while"
    | "do" => "do"
    | "repeat" => "repeat"
    | "until" => "until"
    | "match" => "match"
    | "where" => "where"
    | "try" => "try"
    | "with" => "with"
    | "error" => "error"
    | "compile_error" => "compile_error"

    | "not" => "not"
    | "and" => "and"
    | "or" => "or"
    | "xor" => "xor"

    | "digestof" => "digestof"
    | "addressof" => "addressof"
    | "__loc" => "location"

    | "true" => "true"
    | "false" => "false"

    | "#read" => "cap_read"
    | "#send" => "cap_send"
    | "#share" => "cap_share"
    | "#alias" => "cap_alias"
    | "#any" => "cap_any"
    else error
    end


primitive Symbols
  fun apply(value: String box): String? =>
    match value
    | "..." => "ellipsis"
    | "->" => "arrow"
    | "=>" => "dblarrow"

    | "<<~" => "lshift_til"
    | ">>~" => "rshift_til"

    | "==~" => "eq_tilde"
    | "!=~" => "ne_tilde"

    | "<=~" => "le_tilde"
    | ">=~" => "ge_tilde"

    | "<~" => "lt_tilde"
    | ">~" => "gt_tilde"

    | "+~" => "plus_tilde"
    | "-~" => "minus_tilde"
    | "*~" => "multiply_ti"
    | "/~" => "divide_tild"
    | "%%~" => "mod_tilde"
    | "%~" => "rem_tilde"

    | "<<" => "lshift"
    | ">>" => "rshift"

    | "==" => "eq"
    | "!=" => "ne"

    | "<=" => "le"
    | ">=" => "ge"

    | ".>" => "chain"

    | "<:" => "subtype"

    | "\\" => "backslash"

    | "@{" => "at_lbrace"

    | "{" => "lbrace"
    | "}" => "rbrace"
    | "(" => "lparen"
    | ")" => "rparen"
    | "[" => "lsquare"
    | "]" => "rsquare"
    | "," => "comma"

    | "." => "dot"
    | "~" => "tilde"
    | ":" => "colon"
    | ";" => "semi"
    | "=" => "assign"

    | "+" => "plus"
    | "-" => "minus"
    | "*" => "multiply"
    | "/" => "divide"
    | "%%" => "mod"
    | "%" => "rem"
    | "@" => "at"

    | "<" => "lt"
    | ">" => "gt"

    | "|" => "pipe"
    | "&" => "isecttype"
    | "^" => "ephemeral"
    | "!" => "aliased"

    | "?" => "question"
    | "-" => "unary_minus"
    | "#" => "constant"

    | "(" => "lparen_new"
    | "[" => "lsquare_new"
    | "-~" => "minus_tilde"
    | "-" => "minus_new"
    else error
    end

  fun max_len(): USize => 3


class ref Lexer
  """
  Splits a Pony source files into tokens.
  """

  let source: Source

  // Information about the next unused character in source
  var _ptr: USize = 0
  var _len: USize
  var _line: USize = 1
  var _pos: USize = 1
  var _newline: Bool = true

  // Current token
  var _token_line: USize = 0
  var _token_pos: USize = 0
  var _buffer: String ref = String
  var _prepending: String ref = String

  var _next_token: (Token | None) = None

  new create(source': Source) =>
    source = source'
    _len = source.content.size()

  fun ref next(): Token =>
    """
    Returns the next token.
    """
    var token: (Token | None) = _next_token = None
    _buffer = String
    _prepending = String
    while token is None do
      _token_line = _line
      _token_pos = _pos

      if is_eof() then
        token = Token.abstract("eof", _token_line, _token_pos)
        break
      end

      let char = look()
      match char
      | '\n' =>
        _newline = true
        prepend_chars(1)
      | '\r' => prepend_chars(1)
      | '\t' => prepend_chars(1)
      | ' '  => prepend_chars(1)
      | '/'  => token = slash()
      | '"'  => token = string()  // " comment for ponylang-mode highlight bug
      else
        if Chars.isdigit(char) then
          token = number()
        elseif Chars.isalpha(char) or (char == '_') then
          token = keyword(true)
        else
          token = symbol()
        end
      end
    end

    // There is a token, so no longer a new line
    _newline = false

    if _prepending.size() > 0 then
      _next_token = token
      // XXX wrong lineno / pos
      return Token.concrete("whitespace", _token_line, _token_pos, _prepending.clone())
    end

    match token
    | let t: Token => t
    | None =>
      // Cannot be reached
      lex_error()
    end

  fun ref consume_chars(count: USize) =>
    """
    Consumes the specified number of characters from our source. Only the first
    character may be a newline.
    """
    if count > 0 then
      if look() == '\n' then
        _line = _line + 1
        _pos = 0
      end

      _ptr = _ptr + count
      _len = _len - count
      _pos = _pos + count
    end

  fun ref append_chars(count: USize) =>
    """
    Appends `count` tokens to the current token buffer and consumes them.
    """
    _buffer.append(source.content, _ptr, count)
    consume_chars(count)

  fun ref append_char(ch: U8) =>
    """
    Appends the given character to the current token buffer.
    """
    _buffer.append([ch])

  fun ref prepend_chars(count: USize) =>
    """
    Adds and consumes `count` chars to the prepend buffer. Only the first
    character may be a newline.
    """
    _prepending.append(source.content, _ptr, count)
    consume_chars(count)

  fun look(): U8 =>
    """
    Look at the next unused character in our source, without consuming it.
    Returns 0 at EOF.
    """
    try source.content(_ptr)?
    else 0
    end

  fun lookn(n: USize): U8 =>
    """
    Look at the `n`th next character without consuming it.
    """
    try source.content(_ptr + (n - 1))?
    else 0
    end

  fun ref line_comment(): Token =>
    append_chars(2)

    while not is_eof() and (look() != '\n') do
      append_chars(1)
    end

    concrete_text_token("comment")

  fun ref nested_comment(): Token =>
    """
    Processes a block commented for which the leading /* has been seen, but not
    consumed yet.
    """
    append_chars(2)
    var depth = USize(1)

    while depth > 0 do
      if _len <= 1 then
        // Syntax error: unterminated
        return lex_error()
      end

      if (look() == '*') and (lookn(2) == '/') then
        append_chars(2)
        depth = depth - 1
      elseif (look() == '/') and (lookn(2) == '*') then
        append_chars(2)
        depth = depth + 1
      else
        append_chars(1)
      end
    end

    concrete_text_token("comment")

  fun ref slash(): Token =>
    """
    Processes a slash which has been seen, but not consumed yet.
    """
    match lookn(2)
    | '*' => nested_comment()
    | '/' => line_comment()
    | '~' =>
      consume_chars(1)
      Token.abstract("divide_tilde", _token_line, _token_pos, _prepending.clone())
    else
      consume_chars(1)
      Token.abstract("divide", _token_line, _token_pos, _prepending.clone())
    end

  fun ref read_id(): USize =>
    """
    Reads an identifier into the current token buffer, but doesn't consume the
    characters yet. Returns the ID's length.
    """
    var len = USize(0)

    while true do
      let ch = lookn(len + 1)

      if (ch != '_') and (ch != '\'') and (not Chars.isalnum(ch)) then
        break
      end

      append_char(ch)
      len = len + 1
    end

    len

  fun ref keyword(allow_identifiers: Bool): Token =>
    """
    Processes a keyword or identifier, possibly with a special prefix (eg '#').
    Any prefix must have been consumed.
    If no keword is found, an identifier token is created if `allow_identifier`
    is true.
    """
    let len = read_id()

    try
      let token_id = Keywords(_buffer)?
      consume_chars(len)
      abstract_text_token(token_id)
    else
      if allow_identifiers and (len > 0) then
        consume_chars(len)
        abstract_text_token("id")
      else
         lex_error()
      end
    end

  fun ref integer(base: U8, end_on_e: Bool): (String | None) =>
    var result: String ref = String
    var previous_underscore = false

    while not is_eof() do
      let ch = look()

      if ch == '_' then
        if previous_underscore then
          return None
        end

        previous_underscore = true
        consume_chars(1)
        result.append([ch])
        continue
      end

      if end_on_e and ((ch == 'e') or (ch == 'E')) then
        break
      end

      var digit: U8 = 0
      if (ch >= '0') and (ch <= '9') then
        digit = ch - '0'
      elseif (ch >= 'a') and (ch <= 'z') then
        digit = ch - ('a' + 10)
      elseif (ch >= 'A') and (ch <= 'Z') then
        digit = ch - ('A' + 10)
      else
        break
      end

      if digit > base then
        return None
      end

      previous_underscore = false
      consume_chars(1)
      result.append([ch])
    end

    if result.size() == 0 then
      // No numeric character at all
      None
    elseif previous_underscore then
      // Numeric literal cannot end with underscore
      None
    else
      result.clone()
    end

  fun ref nondecimal_number(base: U8): Token =>
    """
    Processes a non-decimal number literal. The leading base specifier has
    already been consumed.
    """
    match integer(base, false)
    | let value: String =>
      Token.abstract("int", _token_line, _token_pos, _buffer + value)
    | None => lex_error()
    end

  fun ref number(): Token =>
    """
    Processes a number literal. The first character has been seen, but not
    consumed.
    """
    if look() == '0' then
      let ch = lookn(2)
      if (ch == 'x') or (ch == 'X') then
        append_chars(2)
        return nondecimal_number(16)
      elseif (ch == 'b') or (ch == 'B') then
        append_chars(2)
        return nondecimal_number(2)
      end
    end

    // It's a decimal number
    let value =
      match integer(10, true)
      | None => return lex_error()
      | let value: String => value
      end

    if (look() == '.') or (look() == 'e') or (look() == 'E') then
      return real(value)
    end

    Token.abstract("int", _token_line, _token_pos, value)

  fun ref real(integral: String): Token =>
    """
    Processes a real literal. The leading integral part has already been read.
    The . or e has been seen, but not consumed.
    """
    let result: String ref = integral.clone()

    var ch = look()
    if (ch == '.') then
      if (lookn(2) < '0') or (lookn(2) > '9') then
        // It's an int token, followed by a dot token
        return Token.abstract("int", _token_line, _token_pos, integral)
      end

      consume_chars(1)
      result.append([ch])

      match integer(10, true)
      | let value: String => result.append(value)
      | None => return lex_error()
      end
    end

    ch = look()
    if (ch == 'e') or (ch == 'E') then
      consume_chars(1)
      result.append([ch])

      ch = look()
      if (ch == '+') or (ch == '-') then
        consume_chars(1)
        result.append([ch])
      end

      match integer(10, false)
      | let value: String => result.append(value)
      | None => return lex_error()
      end
    end

    Token.abstract("float", _token_line, _token_pos, result.clone())

  fun ref string(): Token =>
    """
    Processes a string literal. The leading " has been seen, but not consumed.
    """
    if (lookn(2) == '"') and (lookn(3) == '"') then
      return triple_string()
    end

    append_chars(1)

    while true do
      if is_eof() then
        return lex_error()
      end

      match look()
      | '"' => // " comment to work around ponylang-mode highlighting bug
        append_chars(1)
        return abstract_text_token("string")
      else
        append_chars(1)
      end
    end

    // This cannot be reached
    lex_error()

  fun ref symbol(): Token =>
    """
    Processes a symbol. The first character has been seen, but not consumed.
    """
    var s: String =
      recover
        var s = String
        for i in Range[USize](0, Symbols.max_len() + 1) do
          s.append([lookn(i + 1)])
        end
        s
      end

    for i in Range[USize](s.size(), 0, -1) do
      let symbol_text = s.trim(0, i)
      let token_id =
        try
          Symbols(symbol_text)?
        else
          continue
        end
      consume_chars(i)
      return Token.abstract(token_id, _token_line, _token_pos, symbol_text)
    end

    lex_error()

  fun ref triple_string(): Token =>
    """
    Processes a triple-quoted string. The opening quotes have been seen, but
    not consumed.
    """
    append_chars(3)

    let start_line = _line
    var non_space_on_first_line = false

    while true do
      if is_eof() then
        // Unterminated
        return lex_error()
      end

      let ch = look()
      if (ch == '"') and (lookn(2) == '"') and (lookn(3) == '"') then
        append_chars(3)

        // Triple strings can end with 3 or more double quotes. If there are
        // more than three, the extra ones are part of the string contents
        while look() == '"' do
          append_chars(1)
        end

        if (_line > start_line) and non_space_on_first_line then
          // Multi-line string must be started below the opening triple-quote
          return lex_error()
        end

        return abstract_text_token("string")
      elseif (_line == start_line) and (not Chars.isspace(ch)) then
        non_space_on_first_line = true
      end

      append_chars(1)
    end

    lex_error()

  fun box lex_error(): Token =>
    Token.abstract("lex_error", _token_line, _token_pos, "")

  fun is_eof(): Bool => _len == 0

  fun abstract_text_token(id: String): Token =>
    Token.abstract(id, _token_line, _token_pos, _buffer.clone())

  fun concrete_text_token(id: String): Token =>
    Token.concrete(id, _token_line, _token_pos, _buffer.clone())


primitive Chars
  fun isalnum(ch: U8): Bool => isdigit(ch) or isalpha(ch)

  fun isalpha(ch: U8): Bool =>
    ((ch >= 'a') and (ch <= 'z')) or ((ch >= 'A') and (ch <= 'Z'))

  fun isdigit(ch: U8): Bool => ((ch >= '0') and (ch <= '9'))

  fun isspace(ch: U8): Bool =>
    (ch == ' ') or (ch == '\t') or (ch == '\n') or (ch == '\r') or (ch == '\v')
    or (ch == '\f')
