use "collections"
use "debug"
use peg = "peg"


type L is peg.L
type R is peg.R

primitive _TID is peg.Label fun text(): String => "ID"
primitive _TMacro is peg.Label fun text(): String => "Macro"
primitive _TRParen is peg.Label fun text(): String => ")"


primitive _ParserRuleParser
  fun apply(): peg.Parser val =>
    recover
      let id = (R('a', 'z') / R('A', 'Z') / L("_")).many1().term(_TID)
      let number = R('0', '9').many1().term(peg.TNumber)
      let string = (
        L("\"") * (not L("\"") * peg.Unicode).many() * L("\"")
      ).term(peg.TString)
      let empty_block = L("{}")

      let literal = id / number / string / empty_block

      let expr = peg.Forward

      let macro = (
        (L("OPT_NO_DFLT") / L("OPT")).opt() *
        id *
        L("(") * (expr).many(L(",")) * L(")").term(_TRParen)
      ).node(_TMacro)

      expr() = (macro * ((L(";") * expr) / L(";").opt())) / literal

      let def_macro = peg.Forward
      def_macro() = L("DONE();") / (macro * L(";") * def_macro)
      let def = L("DEF(") * id * L(");") * def_macro

      let whitespace = L(" ") / L("\n")
      let comment = L("//") * (not L("\n") * peg.Unicode).many()

      def.many1().hide((whitespace / comment).many()).eof()
    end



class val PonyParserFactory
  """
  A factory to create new `PonyParser` instances.
  """

  let _rules: Map[String, _ParserRule] val

  new val create()? =>
     let src = peg.Source.from_string(_ParserRules.rules())
     (let pos, let result) = recover _ParserRuleParser().parse(src) end
     let ast = result as peg.AST
     let rules: Map[String, _ParserRule] iso = recover rules.create() end
     for child in ast.children.values() do
       // XXX out'.print(recover peg.Printer(child) end)
       let rule = _parse_def(child as peg.AST)?
       rules(rule.name) = rule
     end
     _rules = consume rules

  fun box apply(source: Source): PonyParser =>
    """
    Create a new `PonyParser` that will parse the given source.
    """
    PonyParser(_rules, source)

  fun tag _parse_def(ast: peg.AST): _ParserRule? =>
    let name = (ast.children(1)? as peg.Token).string()
    let rule: _ParserRule iso = _ParserRule(consume name)
    var current_macro = ast.children(3)?
    while true do
      match current_macro
      | let _: peg.Token => break
      | let macro_ast: peg.AST =>
          match _parse_macro(macro_ast)?
          | let action: _RuleAction => rule.push(action)
          end

          current_macro = macro_ast.children(macro_ast.children.size() - 1)?
      end
    end
    consume rule

  fun tag _parse_macro(ast: peg.AST): (_RuleAction | None)? =>
     var maybe_action: (_RuleAction | None) =
       match (ast.children(1)? as peg.Token).string()
       | "ANNOTATE" => _annotate(ast)?
       | "AST_NODE" => _ast_node(ast)?
       | "IF" => _if(ast)?
       | "IFELSE" => _ifelse(ast)?
       | "RULE" => _rule(ast)?
       | "SEQ" => _seq(ast)?
       | "SKIP" => _token(ast)?
       | "TERMINATE" => _token(ast)?
       | "TOKEN" => _token(ast)?
       | "WHILE" => _while(ast)?
       end
     match maybe_action
     | let action: _RuleAction =>
       match ast.children(0)?
       | let opt_token: peg.Token =>
         match opt_token.string()
         | "OPT" => maybe_action = _Opt(action)
         | "OPT_NO_DFLT" => maybe_action = _Opt(action, _NoDefault)
         end
       end
     end
     maybe_action

  fun tag _annotate(ast: peg.AST): (_Annotate | None)? =>
    let id = (ast.children(3)? as peg.Token).string()
    _Annotate(consume id)

  fun tag _ast_node(ast: peg.AST): _AstNode? =>
    let id = (ast.children(3)? as peg.Token).string()
    _AstNode(_translate_token_id(consume id))

  fun tag _if(ast: peg.AST): (_If | None)? =>
    let id = _translate_token_id((ast.children(3)? as peg.Token).string())
    let body_ast = ast.children(4)? as peg.AST
    match _parse_macro(body_ast)?
    | None => None
    | let body: _RuleAction => _If(id, body)
    end

  fun tag _ifelse(ast: peg.AST): (_IfElse | None)? =>
    let id = _translate_token_id((ast.children(3)? as peg.Token).string())

    match _parse_macro(ast.children(4)? as peg.AST)?
    | None => None
    | let then_body: _RuleAction =>
      match _parse_macro(ast.children(5)? as peg.AST)?
      | None => None
      | let else_body: _RuleAction => _IfElse(id, then_body, else_body)
      end
    end

  fun tag _rule(ast: peg.AST): _Rule? =>
    let ids: Array[String] iso = recover [] end

    var closing_index = ast.children.size() - 3
    if not (ast.children(closing_index)?.label() is _TRParen) then
      closing_index = closing_index + 1
    end

    for i in Range[USize](4, closing_index) do
      ids.push((ast.children(i)? as peg.Token).string())
    end
    if ids.size() == 0 then error end
    _Rule(consume ids)

  fun tag _seq (ast: peg.AST): _Seq? =>
    let ids: Array[String] iso = recover [] end
    for i in Range[USize](4, ast.children.size() - 3) do
      ids.push((ast.children(i)? as peg.Token).string())
    end
    _Seq(consume ids)

  fun tag _token(ast: peg.AST): _Token? =>
    let ids: Array[String] iso = recover [] end
    for i in Range[USize](4, ast.children.size() - 2) do
      if ast.children(i)? is _TRParen then
        break
      end

      let id = (ast.children(i)? as peg.Token).string()
      ids.push(_translate_token_id(consume id))
    end
    _Token(consume ids)

  fun tag _while(ast: peg.AST): (_While | None)? =>
    let id = _translate_token_id((ast.children(3)? as peg.Token).string())
    let body_ast = ast.children(4)? as peg.AST
    match _parse_macro(body_ast)?
    | let body: _Rule => _While(consume id, body)
    | let body: _Token => _While(consume id, body)
    else None
    end

  fun tag _translate_token_id(id: String): String =>
    """
    Translate a token ID from ponyc to pony-fmt. E.g. "TK_STRING" becomes
    "string".
    """
    let translated = id.lower()
    translated.trim_in_place(3)
    consume translated
