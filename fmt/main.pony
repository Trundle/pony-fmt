use "files"


actor Main
  new create(env: Env) =>
    let filename =
      try
        env.args(1)?
      else
        env.err.print("Usage: pony-fmt <filename>")
        return
      end

   let parser_factory =
     try
       PonyParserFactory.create()?
     else
       env.err.print("[FATAL] Could not create parser factory. That's an internal bug.")
       return
     end

   let source =
     try
       Source.from_path(FilePath(FileAuth(env.root), filename))?
     else
       env.err.print("[FATAL] Could not read source file")
       return
     end

   try
     let tree = parser_factory(source).parse()?
     env.out.print(tree.dump())
     env.out.print("\n\n")
     env.out.print(StdlibFormatter.format(tree)?)
   end

