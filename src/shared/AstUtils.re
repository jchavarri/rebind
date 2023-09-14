open Ppxlib;

let stripChars = (charsToStrip, s) => {
  let len = String.length(s);
  let res = Bytes.create(len);
  let rec aux = (i, j) =>
    if (i >= len) {
      Bytes.sub(res, 0, j);
    } else if (Bytes.contains(charsToStrip, s.[i])) {
      aux(succ(i), j);
    } else {
      Bytes.set(res, j, s.[i]);
      aux(succ(i), succ(j));
    };
   Bytes.to_string(aux(0, 0));
};

/* helpers */
/* TODO: turn foo_bar into foo_bar_ */
let correctIdentifier = ident => {
  let rec stripLeadingUnderscores = s =>
    if (String.length(s) == 0) {
      s;
    } else if (s.[0] == '_') {
      stripLeadingUnderscores(String.sub(s, 1, String.length(s) - 1));
    } else {
      s;
    };
  /* ocaml/reason identifiers need to be lower-cased (uppercase reserved for variants constructors, modules, etc.) */
  if (ident == "") {
    ident;
  } else {
    /* foo => foo
       Foo => foo
       _foo => foo
       _foo_bar => foo_bar_ */
    let correctedName = stripLeadingUnderscores(ident) |> stripChars(Bytes.of_string(".-"));
    let correctedName =
      String.contains(correctedName, '_') ?
        correctedName ++ "_" : correctedName;
    let correctedName = String.uncapitalize_ascii(correctedName);
    /* correct other cases where the js name is a reserved ocaml/reason keyword */
    switch (correctedName) {
    | "object" => "object_"
    | "type" => "type_"
    | "done" => "done_"
    | "then" => "then_"
    | n => n
    };
  };
};

let astHelperStrLidIdent = (a) =>
  switch (a) {
  | [] => raise(Invalid_argument("identifier is empty."))
  | _ =>
    let inner =
      Lident(List.hd(a));
    let res =
      List.tl(a)
      |> List.fold_left(
           (acc, curr) =>
             Ldot(acc, curr),
           inner,
         );
    {loc: Ast_helper. default_loc.contents, txt: res};
  };
