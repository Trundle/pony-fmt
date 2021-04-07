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
