class SyntaxTree
  """
  One node in Pony file's syntax.
  """

  var parent: (SyntaxTree | None) = None
  embed children: Array[SyntaxTree] = children.create()
  embed prepending_concrete_tokens: Array[Token] =
    prepending_concrete_tokens.create()
  let token: Token

  new create(token': Token, prepending_concrete_tokens': Array[Token] val = []) =>
    token = token'
    prepending_concrete_tokens.concat(prepending_concrete_tokens'.values())

  fun dump(indent: USize = 0): String =>
    let repr = " " * indent
    repr.append("(")
    repr.append(token.id)
    if children.size() != 0 then
      repr.append("\n")
      for child in children.values() do
        repr.append(child.dump(indent + 2))
      end
      repr.append(" " * indent)
    else
      match token.text
      | let text: String =>
        repr.append(" ")
        repr.append(text)
      end
    end
    repr.append(")\n")
    consume repr
