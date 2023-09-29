open Asttypes
open Longident

let strip_chars charsToStrip s =
  let len = String.length s in
  let res = Bytes.create len in
  let rec aux i j =
    if i >= len then Bytes.sub res 0 j
    else if Bytes.contains charsToStrip s.[i] then aux (succ i) j
    else (
      Bytes.set res j s.[i];
      aux (succ i) (succ j))
  in
  Bytes.to_string (aux 0 0)

let process_reserved_words name =
  match name with
  | "object" -> "object_"
  | "type" -> "type_"
  | "done" -> "done_"
  | "then" -> "then_"
  | n -> n

let correct_labelled_arg name =
  let corrected_name =
    match name with
    | "" -> "_"
    | _ ->
        let first_char = name.[0] in
        if Char.uppercase_ascii first_char = first_char then "_" ^ name
        else name
  in
  process_reserved_words corrected_name

(* helpers *)
(* TODO: turn foo_bar into foo_bar_ *)
let correct_identifier ident =
  let rec strip_leading_underscores s =
    if String.length s = 0 then s
    else if s.[0] = '_' then
      strip_leading_underscores (String.sub s 1 (String.length s - 1))
    else s
  in
  (* ocaml/reason identifiers need to be lower-cased (uppercase reserved for variants constructors, modules, etc.)*)
  if ident = "" then ident
  else
    (* foo => foo
       Foo => foo
       _foo => foo
       _foo_bar => foo_bar_ *)
    let correctedName =
      strip_leading_underscores ident |> strip_chars (Bytes.of_string ".-")
    in
    (* correct other cases where the js name is a reserved ocaml/reason keyword *)
    let correctedName =
      match String.contains correctedName '_' with
      | true -> correctedName ^ "_"
      | false -> correctedName
    in
    let correctedName = String.uncapitalize_ascii correctedName in
    process_reserved_words correctedName

let ast_helper_str_lident a =
  match a with
  | [] -> raise (Invalid_argument "identifier is empty.")
  | _ ->
      let inner = Lident (List.hd a) in
      let res =
        List.tl a |> List.fold_left (fun acc curr -> Ldot (acc, curr)) inner
      in
      { loc = Ast_helper.default_loc.contents; txt = res }
