use "files"
use "pony_test"

use "../../fmt"


class _DirWalker
  """
  Recursively ascends a file tree and yields all pony files.
  """

  embed _to_do: Array[Directory] = []
  embed _files: Array[FilePath] ref = []

  new create(root: Directory)? =>
    _to_do.push(root)
    _fill()?

  fun has_next(): Bool => _files.size() > 0

  fun ref next(): FilePath? =>
    let value = _files.pop()?
    if _files.size() == 0 then
        _fill()?
    end
    value

  fun ref _fill()? =>
    while true do
      if _files.size() > 0 then
        return
      end

      let dir =
        try _to_do.pop()?
        else return
        end
      for entry in dir.entries()?.values() do
        let entry_path = dir.path.join(entry)?
        let info = FileInfo(entry_path)?
        if info.directory then
          _to_do.push(dir.open(entry)?)
        elseif entry.substring(entry.size().isize() - 5) == ".pony" then 
          _files.push(entry_path)
        end
      end
    end


class RoundtripTest is UnitTest  
  fun name(): String => "formatter/roundtrip"

  fun ref apply(helper: TestHelper)? =>
    let parser_factory = PonyParserFactory.create()?
    let dir = Directory(FilePath(FileAuth(helper.env.root), "."))?
    for path in _DirWalker(dir)? do
      helper.log("Trying to parse " + path.path, true)
      test_file(helper, parser_factory, path)?
    end

  fun test_file(helper: TestHelper, parser_factory: PonyParserFactory, path: FilePath)? =>
    let source = Source.from_path(path)?
    let tree = parser_factory(source).parse()?
    let formatted = NoneFormatter.format(tree)
    helper.assert_eq[String](source.content, formatted)
