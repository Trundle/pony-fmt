use "itertools"
use "debug"

class TreeNavigator
  """
  Helper to walk around in a `SyntaxTree`.
  """

  let current: SyntaxTree
  let parent: (SyntaxTree | None)
  let child_index: USize

  new create(
    current': SyntaxTree,
    parent': (SyntaxTree | None),
    child_index': USize)
  =>
    current = current'
    parent = parent'
    child_index = child_index'

  fun ref apply(i: USize): TreeNavigator ? =>
    """
    Returns a new navigator that points to the current node's `i`th child.
    """
    TreeNavigator(current.children(i)?, current, i)

  fun ref children(): ChildrenIter =>
    ChildrenIter(current, current.children.pairs())

  fun ref every_nth_child(n: USize): Iterator[TreeNavigator] =>
    Iter[TreeNavigator](children())
      .filter_stateful(
        object
          var _i: USize = 0

          fun ref apply(child: TreeNavigator): Bool =>
            ((_i = (_i + 1)) % n) == 0
        end)

  fun ref previous(): (TreeNavigator | None) =>
    match parent
    | None => None
    | let some_parent: SyntaxTree =>
      match child_index
      | 0 => None
      else
        let child_index' = child_index - 1
        try
          let current' = some_parent.children(child_index')?
          TreeNavigator(current', some_parent, child_index')
        else
          Debug(
            "Expected a child at position " + child_index.string() +
            " but got none")
          None
        end
      end
    end

  fun ref next(): (TreeNavigator | None) =>
    """
    Returns a new navigator that points to the current node's sibling.
    """
    match parent
    | None => None
    | let some_parent: SyntaxTree =>
      let child_index' = child_index + 1
      try
        let current' = some_parent.children(child_index')?
        TreeNavigator(current', some_parent, child_index')
      else None
      end
    end

  fun ref text(): String ? =>
    current.token.text as String

class ChildrenIter is Iterator[TreeNavigator]
  let _parent: SyntaxTree
  let _tree_children_iter: ArrayPairs[SyntaxTree, Array[SyntaxTree] ref]

  new create(
    parent: SyntaxTree,
    tree_children_iter: ArrayPairs[SyntaxTree, Array[SyntaxTree] ref])
  =>
    _parent = parent
    _tree_children_iter = tree_children_iter

  fun has_next(): Bool =>
    _tree_children_iter.has_next()

  fun ref next(): TreeNavigator ? =>
    (let index, let child) = _tree_children_iter.next()?
    TreeNavigator(child, _parent, index)
