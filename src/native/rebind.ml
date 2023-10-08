module T = New_shared

let cat filename =
  let ic = open_in_bin filename in
  let len = in_channel_length ic in
  let buf = Buffer.create len in
  Buffer.add_channel buf ic len;
  let content = Buffer.contents buf in
  close_in ic;
  content

let read_stdin () =
  let out = ref (input_line stdin) in
  if String.length !out > 0 then
    try
      while true do
        let line = input_line stdin in
        out := String.concat "" [ !out; line ]
      done;
      ""
    with End_of_file -> !out
  else !out

let processContent () =
  let file, content =
    if Array.length Sys.argv <> 2 && not (Unix.isatty Unix.stdin) then
      ("stdin", read_stdin ())
    else
      let file = Sys.argv.(1) in
      let content = cat file in
      (file, content)
  in
  let result =
    Pprintast.string_of_structure
      (New_shared.get_bindings (Some (File_key.SourceFile file)) content)
  in
  let formatted =
    let open Ocamlformat_lib in
    match
      Translation_unit.parse_and_format Syntax.Use_file Conf.default
        ~input_name:"_none_" ~source:result
    with
    | Ok formatted -> formatted
    | Error _e -> failwith "ocamlformat error"
  in
  print_endline formatted

let () =
  if Array.length Sys.argv <> 2 && Unix.isatty Unix.stdin then
    print_endline "Missing input: Add a file path or use stdin"
  else processContent ()
