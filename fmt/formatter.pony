class NoneFormatter
  """
  Formatter that doesn't format at all: returns the input again.
  """

  fun format(tree: SyntaxTree): String =>
    let result: String ref = String
    result.append(tree.token.prepending)
    match tree.token.text
    | let text: String => result.append(text)
    end
    for child in tree.children.values() do
      result.append(format(child))
    end
    result.clone()
