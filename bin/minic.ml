let file = ref None
let dump_parsed = ref false
let dump_typed = ref false
let dump_all = ref false
let dump_dir = ref "./"

let arg_spec =
  let open Arg in
  [
    ("-dump-parsed", Set dump_parsed, "dump the parsed AST");
    ("-dump-typed", Set dump_typed, "dump the typed AST");
    ("-dump-all", Set dump_all, "dump all intermediate passes");
    ("-dump-dir", Set_string dump_dir, "set the dump directory");
  ]

let () =
  Arg.parse arg_spec (fun f -> file := Some f) "minic [options] <file.c>";
  match !file with
  | None -> Printf.eprintf "Error: At least a file is expected\n"
  | Some file ->
    let out_c = open_out "/dev/null" in
    Compiler.compile ~dump_dir:!dump_dir ~dump_all:!dump_all
      ~dump_parsed:!dump_parsed ~dump_typed:!dump_typed file out_c
