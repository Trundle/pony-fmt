use "assert"
use "collections"
use "debug"
use "files"


primitive _ParseOk
primitive _ParseError
primitive _RuleNotFound

type _ParseResult is (_ParseOk | _ParseError | _RuleNotFound)


primitive _Actions
  fun parse_rule_set(
    rules: Array[String] val,
    parser: PonyParser,
    state: _RuleState
  ): (_ParseResult, Bool) ? =>
    for rule_name in rules.values() do
      parser._debug("Trying rule " + rule_name)
      let rule = parser._find_rule(rule_name)?
      try
        match rule.parse(parser)?
        | let tree: SyntaxTree =>
          return (parser._handle_found(state, tree), true)
        end
      else return (_ParseError, true)
      end
    end
    parser._debug("parse_rule_set: exiting with handle_not_found")
    (parser._handle_not_found(state), false)

  fun parse_token_set(
    tokens: Array[String] val,
    parser: PonyParser,
    state: _RuleState
  ): (_ParseResult, Bool) ? =>
    parser._debug("Am inside token handler! " + tokens.size().string() + " tokens in set: " + ",".join(tokens.values()))
    let current_token = parser._current_token()?
    for token_id in tokens.values() do
      parser._debug("Trying to match " + token_id + " against " + current_token.id )
      if token_id == "newline" then
        // XXX implement me
        parser._debug("Whoops TK_NEWLINE not implemented yet")
        error
      elseif token_id == current_token.id then
        return (parser._handle_found(state, parser._consume_token()?), true)
      end
    end
    // Nothing matched
    parser._debug("But nothing matched")
    (parser._handle_not_found(state), false)


// Representations for ponyc's parser macros
// ponyc generates C code with them, we interpret them during parsing

trait val _RuleAction
  fun val execute(parser: PonyParser, state: _RuleState): _ParseResult?


class val _Opt is _RuleAction
  let action: _RuleAction
  let default: (_NoDefault | None)

  new val create(action': _RuleAction, default': (_NoDefault | None) = None) =>
    action = action'
    default = default'

  fun val execute(parser: PonyParser, state: _RuleState): _ParseResult? =>
    state.default = None
    parser._debug("Executing optional rule")
    action.execute(parser, state)?


class val _AstNode is _RuleAction
  let id: String

  new val create(id': String) => id = id'

  fun val execute(parser: PonyParser, state: _RuleState): _ParseResult =>
    parser._add_deferrable_node(state, id)
    _ParseOk


class val _If is _RuleAction
  let id: String
  let body: _RuleAction

  new val create(id': String, body': _RuleAction) =>
    id = id'
    body = body'

  fun val execute(parser: PonyParser, state: _RuleState): _ParseResult ? =>
    parser._debug("Inside _If")
    state.default = None
    match _Actions.parse_token_set([id], parser, state)?
    | (_ParseOk, true) => body.execute(parser, state)?
    | (let result: _ParseResult, _) => result
    end


class val _IfElse is _RuleAction
  let id: String
  let then_body: _RuleAction
  let else_body: _RuleAction

  new val create(
    id': String,
    then_body': _RuleAction,
    else_body': _RuleAction
  ) =>
    id = id'
    then_body = then_body'
    else_body = else_body'

  fun val execute(parser: PonyParser, state: _RuleState): _ParseResult ? =>
    parser._debug("Entered _IfElse")
    state.default = None
    match _Actions.parse_token_set([id], parser, state)?
    | (_ParseOk, true) => then_body.execute(parser, state)?
    | (_ParseOk, false) => else_body.execute(parser, state)?
    | (let result: _ParseResult, _) => result
    end


class val _Rule is _RuleAction
  let rules: Array[String] val

  new val create(rules': Array[String] val) =>
    rules = rules'

  fun val execute(parser: PonyParser, state: _RuleState): _ParseResult ? =>
    parser._debug("Entering _Rule (" + ", ".join(rules.values()) + ")")
    _Actions.parse_rule_set(rules, parser, state)?._1


class val _Seq is _RuleAction
  """
  Repeatedly parse a rule set as long as it succeeds.
  """
  let rules: Array[String] val

  new val create(rules': Array[String] val) =>
    rules = rules'

  fun val execute(parser: PonyParser, state: _RuleState): _ParseResult ? =>
    var matched = true
    while matched do
      parser._debug("SEQ in '" + state.name + "': Next iteration!")
      state.default = _NoDefault
      match _Actions.parse_rule_set(rules, parser, state)?
      | (_ParseOk, let found: Bool) =>
        parser._debug("SEQ in '" + state.name + "': parse_rule_set returned parse OK")
        matched = found
      | (_ParseError, _) =>
        parser._debug("SEQ in '" + state.name + "': parse_rule_set returned parse error ")
        return _ParseError
      | (_RuleNotFound, _) =>
        parser._debug("SEQ in '" + state.name + "': parse_rule_set returned rule not found")
        matched = false
      end
      parser.out.flush()
    end
    parser._debug("SEQ in '" + state.name + "': Returning OK")
    _ParseOk


class val _Token is _RuleAction
  let tokens: Array[String] val

  new val create(tokens': Array[String] val) =>
    tokens = tokens'

  fun val execute(parser: PonyParser, state: _RuleState): _ParseResult? =>
    _Actions.parse_token_set(tokens, parser, state)?._1


class val _While is _RuleAction
  let id: String
  let body: (_Rule | _Token)

  new val create(id': String, body': (_Rule | _Token)) =>
    id = id'
    body = body'

  fun val execute(parser: PonyParser, state: _RuleState): _ParseResult? =>
    parser._debug("Unimplemented: _While")
    error


primitive _Required
primitive _NoDefault

class _RuleState
   """
   Processing state of one `_ParserRule`.
   """

   // Rule name (for debugging)
   let name: String
   // Whether the ruled matched
   var matched: Bool = false
   // Deferred AST node's ID
   var deferred_id: (String | None) = None
   // Syntax tree built by this rule
   var tree: (SyntaxTree | None) = None
   // Whether the next action is optional
   var default: (_Required | _NoDefault | None) = _Required

   new create(name': String) =>
     name = name'


class val _ParserRule
  let name: String
  embed actions: Array[_RuleAction] = actions.create()

  new iso create(name': String) =>
    name = name'

  fun ref push(some: _RuleAction) =>
    actions.push(some)

  fun val parse(parser: PonyParser): (SyntaxTree | None)? =>
    parser.indent = parser.indent + 1
    let state = _RuleState(name)
    for action in actions.values() do
      try
        parser._debug("Trying next action in rule " + name)
        match action.execute(parser, state)?
        | _ParseError =>
          parser._debug("Action for rule '" + name + "' returned parse error")
          parser.indent = parser.indent - 1
          error
        | _RuleNotFound =>
          parser._debug("Action for rule '" + name + "' returned rule not found")
          parser.indent = parser.indent - 1
          return
        end
      else
        parser._debug("Got error from action '" + name + "', that's a bug")
        error
      end
    end
    parser._debug("Returning OK for rule '" + name + "'")
    parser.indent = parser.indent - 1
    parser._process_deferred_ast(state)
    Fact(not (state.tree is None), "Expected a syntax tree, but there is none")?
    state.tree


class PonyParser
  let _rules: Map[String, _ParserRule] val
  let _lexer: Lexer

  var indent: USize = 0
  let out: OutStream

  var _token: Token

  new create(rules: Map[String, _ParserRule] val, source: Source, out': OutStream) =>
    _rules = rules
    _lexer = Lexer(source)
    _token = Token.empty()
    // XXX remove me
    out = out'
    _fetch_next_token()

  fun ref parse(): SyntaxTree ? =>
    let module_rule = _rules("module")?
    module_rule.parse(this)? as SyntaxTree

  fun _find_rule(name: String): _ParserRule ? =>
    _rules(name)?

  fun _current_token(): Token? =>
    if _token.id == "lex_error" then
      _debug("Source file contained syntax error")
      error
    end
    _token

  fun ref _fetch_next_token() =>
    while true do
      _token = _lexer.next()
      if _token.kind is Abstract then
        break
      end
    end

    _debug("Next token: " + _token.id)

  fun ref _handle_found(state: _RuleState, node: SyntaxTree): _ParseResult =>
    state.default = _Required
    state.matched = true
    _add_tree_node(state, node)
    _ParseOk

  fun ref _handle_not_found(state: _RuleState): _ParseResult =>
    match state.default = _Required
    | _Required =>
      if state.matched then
        _debug("Not found: already matched for '" + state.name + "', returning parse error")
        return _ParseError
      else
        _debug("Not found: nothing matched, returning rule not found")
        return _RuleNotFound
      end
    | None => _add_deferrable_node(state, "none")
    end
    _debug("Will return OK from not found")
    _ParseOk

  fun ref _add_deferrable_node(state: _RuleState, token_id: String) =>
    """
    Add a syntax tree node for the specified token, which may be deferred.
    """
    if (not state.matched) and (state.tree is None) and
       (state.deferred_id is None)
    then
      // The first tree node, defer its creation
      state.deferred_id = token_id
    else
      _add_tree_node(state, SyntaxTree(Token.empty(token_id)))
    end

  fun ref _add_tree_node(state: _RuleState, node: SyntaxTree) =>
    _process_deferred_ast(state)

    match state.tree
    | None => state.tree = node
    | let parent: SyntaxTree => parent.children.push(node)
    end

  fun ref _process_deferred_ast(state: _RuleState) =>
    match state.deferred_id
    | let id: String =>
      let token = Token.empty(id)
      state.tree = SyntaxTree(token)
      state.deferred_id = None
    end

  fun ref _consume_token(): SyntaxTree iso^ ? =>
    let token = _current_token()?
    _fetch_next_token()
    recover SyntaxTree(token) end

  fun _debug(str: String) =>
    out.print(("  " * indent) + str)
    out.flush()
