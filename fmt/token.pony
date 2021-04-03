primitive Abstract
primitive Concrete


class val Token
  let id: String
  let lineno: USize
  let pos: USize
  let prepending: String
  let text: (String | None)
  let kind: (Abstract | Concrete)

  new val empty(id': String = "none") =>
    id = id'
    lineno = 0
    pos = 0
    prepending = ""
    text = None
    kind = Abstract

  new val abstract(
    id': String,
    lineno': USize,
    pos': USize,
    prepending': String,
    text': (String | None) = None
  ) =>
    id = id'
    lineno = lineno'
    pos = pos'
    prepending = prepending'
    text = text'
    kind = Abstract

 new val concrete(
   id': String,
   lineno': USize,
   pos': USize,
   prepending': String,
   text': (String | None) = None
) =>
   id = id'
   lineno = lineno'
   pos = pos'
   prepending = prepending'
   text = text'
   kind = Concrete
