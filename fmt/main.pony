use "files"
use "peg"


class RoundtripFormatter
  """
  A formatter that doesn't format at all.
  """

  fun format(ast: ASTChild): String val =>
    recover
      let result: String ref = String
      match ast
      | let a: AST =>
        for child in a.children.values() do
          result.append(format(child))
        end
      | let token: Token =>
        result.append(token.source.content, token.offset, token.length)
      end
      result
    end


actor Main
  new create(env: Env) =>
    let auth =
      try
        env.root as AmbientAuth
      else
        env.err.print("[FATAL] Could not obtain root capability :(")
        return
      end

    let filename =
      try
        env.args(1)?
      else
        env.err.print("Usage: pony-fmt <filename>")
        return
      end

    try
      let result = PonyParser.parse(FilePath(auth, filename)?, env.out)?
      env.out.print("Got result!")
      env.out.print(RoundtripFormatter.format(result))
    end
