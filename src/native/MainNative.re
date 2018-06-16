let cat = filename => {
  let ic = open_in_bin(filename);
  let len = in_channel_length(ic);
  let buf = Buffer.create(len);
  Buffer.add_channel(buf, ic, len);
  let content = Buffer.contents(buf);
  close_in(ic);
  content;
};

let readStdin = () => {
  let out = ref(input_line(stdin));
  if (String.length(out^) > 0) {
    try (
      {
        while (true) {
          let line = input_line(stdin);
          out := String.concat("", [out^, line]);
        };
        "";
      }
    ) {
    | End_of_file => out^
    };
  } else {
    out^;
  };
};

let processContent = () => {
  let (file, content) =
    if (Array.length(Sys.argv) != 2 && ! Unix.isatty(Unix.stdin)) {
      ("stdin", readStdin());
    } else {
      let file = Sys.argv[1];
      let content = cat(file);
      (file, content);
    };
  output_string(stdout, Config.ast_impl_magic_number);
  output_value(stdout, file);
  let result = Rebind.getBindings(file, content);
  output_value(stdout, result);
};

/* Ocaml  */
let () =
  if (Array.length(Sys.argv) != 2 && Unix.isatty(Unix.stdin)) {
    print_endline("Missing input: Add a file path or use stdin");
  } else {
    processContent();
  };