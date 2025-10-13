open Test_lib
open C_AST

exception NoError

let expectation_tab =
  [
    ("tests/comp/", function NoError -> () | e -> raise e);
    ( "tests/tchk/type_check/",
      function
      | AST_Error (_, TypeCheck _) -> ()
      | _ -> failwith "expecting typecheck error" );
    ( "tests/tchk/no_main/",
      function
      | AST_Error (_, NoMain) -> ()
      | _ -> failwith "expecting no_main error" );
    ( "tests/tchk/undef_var/",
      function
      | AST_Error (_, UndefinedVar _) -> ()
      | _ -> failwith "expected undefined_var error" );
    ( "tests/tchk/undef_func/",
      function
      | AST_Error (_, UndefinedFunc _) -> ()
      | _ -> failwith "expected undefined type error" );
    ( "tests/tchk/undef_typ/",
      function
      | AST_Error (_, UndefinedTyp _) -> ()
      | _ -> failwith "expected undef_type error" );
    ( "tests/tchk/no_return/",
      function
      | AST_Error (_, NoReturn _) -> ()
      | _ -> failwith "expected no_return error" );
    ( "tests/tchk/function_arg_num/",
      function
      | AST_Error (_, FunctionWrongArgNum _) -> ()
      | _ -> failwith "expected arg_number error" );
    ( "tests/tchk/argument_too_big/",
      function
      | AST_Error (_, ArgumentTooBig _) -> ()
      | _ -> failwith "expected argument_too_big error" );
    ( "tests/tchk/no_struct_field/",
      function
      | AST_Error (_, NoStructField _) -> ()
      | _ -> failwith "expected no_struct_field error" );
  ]

let test_ko_tchk_file dir f =
  Format.printf "typechecking %s...@." f;
  let f_c = Filename.concat dir f in
  let c_asm = open_out @@ "/dev/null" in
  try
    Compiler.compile ~raise_errors:false ~type_check_only:true f_c c_asm;
    raise NoError
  with e -> (
    try (List.assoc dir expectation_tab) e
    with Not_found -> failwith "uncompatible test case")

let () = run_tests test_ko_tchk_file "tests/" "typechecker"
