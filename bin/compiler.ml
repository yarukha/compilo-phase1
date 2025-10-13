let compile ?(type_check_only = false) ?(dump_dir = "./") ?(dump_all = false)
    ?(dump_parsed = false) ?(dump_typed = false) ?(raise_errors = true) filename
    _out_c =
  Runner.run ~raise_errors filename (fun untyped_prog ->
      let _ = type_check_only in
      let basename =
        Filename.(
          concat (Sys.getcwd ())
            (concat dump_dir (chop_suffix (basename filename) ".c")))
      in
      if dump_all || dump_parsed then
        C_AST.dump_parsed_dot ~basename untyped_prog;
      let typed_prog = Typechecker.typecheck untyped_prog in
      if dump_all || dump_typed then C_AST.dump_typed_dot ~basename typed_prog)
