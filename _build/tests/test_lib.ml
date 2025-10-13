let read_file file =
  let ic = open_in file in
  let s = In_channel.input_all ic in
  close_in ic;
  s

let (cmdf :
      (string -> int, Format.formatter, unit, int) format4 -> string -> int) =
  Format.kasprintf (fun s -> Sys.command s)

type test_case = { name : string; classname : string; failure : exn option }

type test_suite = {
  classname : string;
  cases : test_case list;
  sub_suites : test_suite list;
}

let pp_xml_failure fmt failure =
  match failure with
  | None -> ()
  | Some err ->
      Format.fprintf fmt "@[<v 2><failure>@;%s@]@;</failure>"
        (Printexc.to_string err)

let pp_xml_case fmt { name; classname; failure } =
  Format.fprintf fmt
    "@[<v 2><testcase name=\"%s\" classname=\"%s\">@;%a@]@;</testcase>" name
    classname pp_xml_failure failure

let rec pp_xml_suite fmt { classname; cases; sub_suites } =
  let open Format in
  fprintf fmt "@[<v 2><testsuite name=\"%s\">@;%a@;@;%a</testsuite>@]" classname
    (pp_print_list pp_xml_case)
    cases
    (pp_print_list pp_xml_suite)
    sub_suites

let dump_xml filename suite =
  let oc = open_out filename in

  Format.fprintf
    (Format.formatter_of_out_channel oc)
    "@[<v 0><?xml version=\"1.0\" encoding=\"UTF-8\"?>@;\
     @;\
     @[<v 2><testsuites>%a@]</testsuites>@]"
    pp_xml_suite suite

let pp_case_errors fmt { name; failure; _ } =
  match failure with
  | None -> ()
  | Some (C_AST.AST_Error (_, kind)) ->
      Format.fprintf fmt "@;\027[31m%s error: %a\027[0m" name
        C_AST.pp_error_kind kind
  | Some err ->
      Format.fprintf fmt "@;\027[31m%s error: %s\027[0m" name
        (Printexc.to_string err)

let rec pp_suite_errors fmt { classname; cases; sub_suites } =
  let open Format in
  Format.fprintf fmt "@[<v 2>%s:%a@;%a@]@;" classname
    (pp_print_list ~pp_sep:pp_print_nothing pp_case_errors)
    cases
    (pp_print_list ~pp_sep:pp_print_nothing pp_suite_errors)
    sub_suites

let rec has_error { cases; sub_suites; _ } =
  List.exists (fun { failure; _ } -> Option.is_some failure) cases
  || List.exists has_error sub_suites

let display_results suite =
  if not (has_error suite) then (
    Format.printf "all tests passed!@;";
    exit 0)
  else
    Format.eprintf "@[<v 0>@;@;Tests failed! Here is a summary.@;@;%a@]"
      pp_suite_errors suite;
  exit (-1)

let test_file test_function dir classname name =
  Format.printf "@[<v 2>testing %s/%s...@;" classname name;
  let result =
    try
      test_function dir name;
      { name; classname; failure = None }
    with e -> { name; classname; failure = Some e }
  in
  Format.printf "@]@;";
  result

let test_file_list test_function dir classname l =
  l
  |> (match Sys.getenv_opt "TEST_FILTER" with
     | None -> Fun.id
     | Some file -> List.filter (String.equal file))
  |> List.filter (fun f -> Filename.check_suffix f ".c")
  |> List.map (test_file test_function dir classname)

let rec test_directory test_function test_dir classname =
  let curr_dir = Filename.concat (Sys.getcwd ()) test_dir in
  let sub_dirs, files =
    Sys.readdir curr_dir |> Array.to_list |> List.sort compare
    |> List.partition (fun f -> Sys.is_directory (curr_dir ^ f))
  in
  let cases = test_file_list test_function test_dir classname files in
  let sub_suites =
    List.filter_map
      (fun sub_dir ->
        if String.starts_with ~prefix:"." sub_dir then None
        else
          Option.some
          @@ test_directory test_function
               (test_dir ^ sub_dir ^ "/")
               (classname ^ "/" ^ sub_dir ^ ""))
      sub_dirs
  in
  { classname; cases; sub_suites }

let run_tests test_function start_dir classname =
  let suite = test_directory test_function start_dir classname in
  (match Sys.getenv_opt "DUMP_RESULT" with
  | None -> ()
  | Some file -> dump_xml file suite);

  display_results suite
