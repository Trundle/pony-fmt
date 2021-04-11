class SyntaxTree
  """
  One node in Pony file's syntax.
  """

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
    match token.text
    | let text: String if text != token.id =>
      repr.append(" ")
      repr.append(text)
    end
    if children.size() != 0 then
      repr.append("\n")
      for child in children.values() do
        repr.append(child.dump(indent + 2))
      end
      repr.append(" " * indent)
    end
    repr.append(")\n")
    consume repr
