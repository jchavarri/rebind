let cat = filename => {
  let ic = open_in_bin(filename);
  let len = in_channel_length(ic);
  let buf = Buffer.create(len);
  Buffer.add_channel(buf, ic, len);
  let content = Buffer.contents(buf);
  close_in(ic);
  content;
};

/* Js */
let () =
  if (Array.length(Sys.argv) != 3) {
    raise(
      Invalid_argument(
        "Please provide as argument the JS file to convert over.",
      ),
    );
  } else {
    let file = Sys.argv[2];
    let content = Node.Fs.readFileAsUtf8Sync(file);
    
    /* Js.log(Js.Json.stringifyAny(file)); */
    let result = Rebind.getBindings(file, content);
    Js.log(Js.Json.stringifyAny(result));
  };