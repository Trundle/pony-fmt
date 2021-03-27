use "files"
use "peg"


primitive TCap     is Label fun text(): String => "cap"
primitive TID      is Label fun text(): String => "ID"
primitive TNewline is Label fun text(): String => "Newline"


primitive PonyParser
  fun apply(): Parser val =>
    recover
      let bool = (L("true") / L("false")).term(TBool)
      let char =
        L("\\\"") / L("\\\\") / L("\\/") / L("\\b") / L("\\f") / L("\\n") /
        L("\\r") / L("\\t") /
        (not L("\"") * not L("\\") * R(' '))
      let string = (L("\"") * char.many() * L("\"")).term(TString)
      let literal = bool / string

      let id: Parser box =
        recover
          let letter = R('a', 'z') / R('A', 'Z')
          let start = L("_") / letter
          let cont = start / R('1', '9') / L("'")
          (start * cont.many()).term(TID)
        end

      let whitespace = L(" ")
      let newline = (L("\r").opt() * L("\n")).term(TNewline)

      let cap = (
        L("iso") / L("trn") / L("ref") / L("val") / L("box") / L("tag")
      ).term(TCap)

      let rawseq = Forward

      // XXX intrinsic
      let jump = (L("return") / L("break") / L("continue") / L("error")) * rawseq

      let semiexpr = semi * (exprseq / jump)

      let exprseq = assignment * (semiexpr / nosemi).opt()

      rawseq() = exprseq / jump

      // XXX
      let atomtype = cap

      // XXX viewpoint
      let type' = atomtype

      let param =
        id * whitespace.many() * L(":") * whitespace.many() * type'
        // XXX
        // (whitespace.many() * L("=") * whitespace.many() * defaultarg).opt()

      let params = param * (L(",") * param).many()

      let method =
        (L("fun") / L("be") / L("new")) *
        // XXX annotations
        // Cap
        (whitespace.many1() * cap).opt() *
        // Name
        whitespace.many1() * id *
        // XXX type params
        // Params
        whitespace.many() * L("(") *
        whitespace.many() * params *
        whitespace.many() * L(")") *
        // Return type
        (whitespace.many() * L(":") * whitespace.many() * type').opt() *
        (whitespace.many() * L("?")).opt() *
        // Docstring
        (newline.many1() * string * newline.many1()).opt() *
        (whitespace.many() * L("=>") * whitespace.many() * rawseq).opt()

      // XXX fields
      let members = method.many()

      let class_def =
        (L("type") / L("interface") / L("trait") / L("primitive") /
         L("struct") / L("class") / L("actor")
        ) * whitespace.many1() * id *
        (whitespace.many1() * cap).opt() * whitespace.many() *
        // Docstring
        (newline.many1() * string).opt() *
        members

      // XXX simplified
      // use string
      let use' = L("use") * whitespace.many1() * string * whitespace.many()

      let module =
        // XXX docstring
        ((use' * (newline.many1() * use').many()).opt() *
        (newline.many1() * class_def.opt()).many()).eof()

      module
    end

  fun parse(path: FilePath, out: OutStream): ASTChild? =>
    let source = Source(path)?
    (let pos, let result) = recover val this().parse(source) end
    match result
    | let p: Parser val =>
      out.print("Oh noes, syntax error at " + pos.string())
      error
    | let a: ASTChild =>
      out.print(recover val Printer(a) end)
      a
    else error
    end
