use "assert"
use "debug"
use "strings"


class NoneFormatter
  """
  Formatter that doesn't format at all: returns the input again.
  """

  fun format(tree: SyntaxTree): String =>
    let result: String iso = recover String end

    for token in tree.prepending_concrete_tokens.values() do
      result.append(_format_token(token))
    end

    result.append(_format_token(tree.token))

    for child in tree.children.values() do
      result.append(format(child))
    end

    consume result

  fun _format_token(token: Token): String =>
    let result: String iso = recover String end
    match token.text
    | let text: String => result.append(text)
    end
    consume result

class StdlibFormatter
  """
  A formatter that formats according to the [Pony Standard Library Style](
  https://github.com/ponylang/ponyc/blob/main/STYLE_GUIDE.md).
  """
  let _printer: PrettyPrinter = PrettyPrinter
  // Whether there needs to be a space before the next token
  var _needs_space: Bool = false
  // Whether the next token should be nested
  var _nest_next: Bool = false
  // Whether the current token is inside a method body
  var _in_method: Bool = false

  fun ref format(tree: SyntaxTree): String? =>
    _print_part(TreeNavigator(tree, None, 0))?
    _printer.render()

  fun ref _print_part(tree: TreeNavigator)? =>
    if _nest_next then
      _nest_next = false
      return _printer.nest(2, this~_print_part(tree))?
    end

    _print_prepending(tree.current.prepending_concrete_tokens)?

    match tree.current.token.id
    | "actor" => _print_class_def(tree)?
    | "class" => _print_class_def(tree)?
    | "dblarrow" => _print_symbol_nest_next("=>")?
    | "do" => _print_symbol_nest_next("do")?
    | "else" => _print_else(tree)?
    | "fun" => _print_fun(tree)?
    | "interface" => _print_class_def(tree)?
    | "params" => _print_params(tree)?
    | "primitive" => _print_class_def(tree)?
    | "string" => _print_string(tree.text()?)?
    | "struct" => _print_class_def(tree)?
    | "then" => _print_symbol_nest_next("then")?
    | "trait" => _print_class_def(tree)?
    | "type" => _print_class_def(tree)?
    | "use" => _print_use(tree)?
    else
      if _needs_space then
        _printer.space()?
      end
      _print_generic(tree)?
    end

  fun ref _print_prepending(tokens: Array[Token])? =>
    _needs_space = false

    for token in tokens.values() do
      match token.id
      | "whitespace" =>
        let whitespace = token.text as String
        if whitespace.contains("\n") then
          _needs_space = false
          if _in_method then
            _printer.newline()?
          end
        else
          _needs_space = true
        end
      | "comment" =>
        _print_comment(token.text as String)?
      end
    end

  fun ref _print_generic(tree: TreeNavigator)? =>
    match tree.current.token.text
    | let text: String =>
      _printer.text(text)?
    end

    for child in tree.children() do
      _print_part(child)?
    end

  fun ref _print_comment(comment: String)? =>
    if comment.compare_sub("//", 2) is Equal then
      _print_single_line_comment(comment)?
    else
      _print_multi_line_comment(comment)?
    end

  fun ref _print_multi_line_comment(comment: String)? =>
    let lines: Array[String] = comment.split_by("\n")
    for (i, line) in lines.pairs() do
      _printer.text(line)?
      if (i + 1) < lines.size() then
        _printer.newline()?
      end
    end

  fun ref _print_single_line_comment(comment: String)? =>
    let stripped = comment.substring(3).>strip()
    _printer.text("// " + consume stripped)?

  fun ref _print_string(string: String)? =>
    if string.compare_sub("\"\"\"", 3) is Equal then
      _print_multiline_string(string)?
    else
      _printer.text(string)?
    end

  fun ref _print_else(tree: TreeNavigator)? =>
    Fact(tree.current.token.id == "else", "expected else node")?
    _printer.text("else")?
    match tree.current.children.size()
    | 0 => _nest_next = true
    | 1 => _printer.nest(2, this~_print_part(tree(0)?))?
    | let n: USize =>
      Debug("expected at most one child for else node, got " + n.string())
      error
    end

  fun ref _print_symbol_nest_next(symbol: String)? =>
    _printer.text(symbol)?
    _nest_next = true

  fun ref _print_multiline_string(string: String)? =>
    let lines: Array[String] = string.split_by("\n").slice(1)
    let prefix = CommonPrefix(lines)
    _printer.text("\"\"\"")?
    for line in lines.values() do
      _printer.newline()?
      _printer.text(line.substring(prefix.size().isize()))?
    end

  fun ref _print_fun(method: TreeNavigator)? =>
    _printer.text("fun")?

    for child in method.children() do
      _print_part(child)?
      if child.current.token.id == "dblarrow" then
        _in_method = true
      end
    end
    _in_method = false

    _printer.hard_line()
    _printer.newline()?

  fun ref _print_params(params: TreeNavigator)? =>
    let outer_print_part = this~_print_part()
    _printer.grouped({()? =>
      let print_part = outer_print_part

      for param in params.every_nth_child(2) do
        _printer.grouped({()? =>
          print_part(param)?
          if not (param.next() is None) then
            _printer.text(",")?
          end
          _printer.if_flat(_printer~space(1), _printer~newline())?
        })?
      end
     })?

  fun ref _print_class_def(cls: TreeNavigator)? =>
    _printer.text(cls.text()?)?
    _printer.space()?

    // @
    if cls(0)?.current.token.id != "none" then
      _printer.text("@")?
    end

    let ref' = cls(1)?
    if ref'.current.token.id != "none" then
      _print_part(ref')?
      _printer.space()?
    end

    _print_part(cls(2)?)?

    let type_params = cls(3)?
    if type_params.current.token.id != "none" then
      // XXX format
      _print_part(type_params)?
    end

    let provides = cls(4)?
    if provides.current.token.id != "none" then
      // XXX format
      _printer.space()?
      _print_part(provides)?
    end

    let printer = _printer
    let print_part = this~_print_part()

    printer.nest(2, {ref()? =>
        printer.newline()?

        let docstring = cls(5)?
        if docstring.current.token.id != "none" then
          print_part(docstring)?
          printer.newline()?
        end

        print_part(cls(6)?)?
      })?

  fun ref _print_use(use_node: TreeNavigator )? =>
    Fact(use_node.current.token.id == "use", "expected use node")?
    let source: String iso = recover String end
    source.append("use ")
    let id = use_node.current.children(0)?
    if id.token.id != "none" then
      Fact(id.token.id == "id", "expected id node")?
      source.append(id.token.text as String)
      source.append(" = ")
    end
    source.append(use_node(1)?.text()?)
    // XXX doesn't handle if infix

    _printer.text(consume source)?
    _printer.hard_line()

    match use_node.next()
    | let next: TreeNavigator if next.current.token.id != "use" =>
      _printer.hard_line()
    end
