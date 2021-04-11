// The formatting mechanisms are inspired by
// http://davidchristiansen.dk/drafts/final-pretty-printer-draft.pdf

use "debug"

trait val Part

primitive Newline is Part

class val Text is Part
  """
  A text fragment, without newlines.
  """

  let text: String

  new val create(text': String) =>
    text = text'


class val Space is Part
  """
  An amount of horizontal space. Will be skipped in Break layouts.
  """

  let amount: USize

  new val create(amount': USize) =>
    amount = amount'

type Atom is (Chunk | Newline)
type Chunk is (Text | Space)


primitive Break
primitive Flat

type Layout is (Break | Flat)


type Line is Array[Chunk]


primitive CanFail
  """
  The current pretty printing is allowed to fail.
  """

primitive CannotFail
  """
  The current pretty printing is not allowed to fail (e.g. rather overflow).
  """

type Failure is (CanFail | CannotFail)


class val OutSeq
  let left: (None | Atom | OutSeq)
  let right: (None | Atom | OutSeq)

  new val create(
    left': (None | Atom | OutSeq),
    right': (None | Atom | OutSeq)
  ) =>
    left = left'
    right = right'

type PrinterOutput is (None | Atom | OutSeq)


class _PrinterState
  let max_width: USize
  var nesting: USize
  var failure: Failure
  var layout: Layout
  var output: PrinterOutput
  var current_line: Line

  new create(max_width': USize) =>
    max_width = max_width'
    nesting = 0
    failure = CannotFail
    layout = Break
    output = None
    current_line = []

  new _from_values(
    max_width': USize,
    nesting': USize,
    failure': Failure,
    layout': Layout,
    output': PrinterOutput,
    current_line': Line
  ) =>
    max_width = max_width'
    nesting = nesting'
    failure = failure'
    layout = layout'
    output = output'
    current_line = current_line'

  fun box clone(): _PrinterState =>
    _PrinterState._from_values(
      max_width, nesting, failure, layout, output, current_line.clone())


type PrintAction is {ref()? }


class PrettyPrinter
  var _state: _PrinterState = _PrinterState(80)

  fun ref current_line(): Line =>
    _state.current_line

  fun ref render(): String =>
    _render(_state.output)

  fun ref hard_line() =>
    """
    A newline that ignores nesting.
    """
    _state.output = OutSeq(_state.output, Newline)
    _state.current_line = []

  fun ref newline()? =>
    """
    A newline that respects nesting.
    """
    let nesting = _state.nesting
    hard_line()
    space(nesting)?

  fun ref nest(n: USize, action: {ref()? })? =>
    let saved_nesting = _state.nesting = _state.nesting + n
    action()?
    _state.nesting = saved_nesting

  fun ref space(width: USize = 1)? =>
    _print_chunk(Space(width))?

  fun ref text(t: String)? =>
    _print_chunk(Text(t))?

  fun ref grouped(value: PrintAction)? =>
    match _state.layout
    | Flat => value()?
    | Break =>
      let saved_state = _state.clone()
      try
        _compose(this~_with_flat(), this~_with_can_fail())(value)?
      else
        _state = saved_state
        value()?
      end
    end

  fun ref if_flat(flat_action: PrintAction, break_action: PrintAction)? =>
    match _state.layout
    | Flat => flat_action()?
    | Break => break_action()?
    end

  fun tag _compose(
    a: {ref(PrintAction)? },
    b: {ref(PrintAction)? }): {ref(PrintAction)? }
  =>
    """
    a ∘ b: takes two lambdas that take an action and returns a
    lambda `(action) => a(b(action))`.
    """
    {ref(action: PrintAction)? =>
      // XXX Is this necessary or rather some compiler bug?
      let captured_b = b
      a({ref()? => b(action)? })?
      }

  fun ref _with_flat(action: PrintAction)? =>
    let saved_layout = _state.layout = Flat
    action()?
    _state.layout = saved_layout

  fun ref _with_can_fail(action: PrintAction)? =>
    let saved_failure = _state.failure = CanFail
    action()?
    _state.failure = saved_failure

  fun tag _measure(line: Line box): USize =>
    var width: USize = 0
    for chunk in line.values() do
      let chunk_width =
        match chunk
        | let t: Text => t.text.codepoints()
        | let sp: Space => sp.amount
        end
      width = width + chunk_width
    end
    width

  fun ref _print_chunk(chunk: Chunk)? =>
    """
    Adds `chunk` to output.
    """
    _state.output = OutSeq(_state.output, chunk)
    _state.current_line.push(chunk)
    if _state.failure is CanFail then
      let line_width = _measure(_state.current_line)
      if line_width > _state.max_width then
        error
      end
    end

  fun tag _render(output: PrinterOutput): String =>
    match output
    | None => ""
    | let atom: Atom => _render_atom(atom)
    | let seq: OutSeq => _render(seq.left) + _render(seq.right)
    end

  fun tag _render_atom(atom: Atom): String =>
    """
    Converts `atom` to string.
    """
    match atom
    | let chunk: Chunk => _render_chunk(chunk)
    | Newline => "\n"
    end

  fun tag _render_chunk(chunk: Chunk): String =>
    """
    Converts `chunk` to string.
    """
    match chunk
    | let t: Text => t.text
    | let sp: Space => " " * sp.amount
    end
