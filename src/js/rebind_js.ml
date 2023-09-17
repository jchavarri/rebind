module Js = Jsoo_runtime.Js

let export (field : Js.t) v = Js.set (Js.pure_js_expr "globalThis") field v

module Reason = struct
  module Js = Jsoo_runtime.Js
  module RE = Reason_toolchain.RE

  let printWith ~f structureAndComments =
    Js.string (Format.asprintf "%a" f structureAndComments)

  let printRE = printWith ~f:RE.print_implementation_with_comments
end

let () =
  export (Js.string "rebind")
    (Js.obj
       [|
         ( "generateML",
           Js.wrap_meth_callback (fun _ js_code ->
               let result =
                 Pprintast.string_of_structure
                   (Shared.get_bindings None js_code)
               in
               Js.(obj [| ("code", Js.string result) |])) );
         ( "generateRE",
           Js.wrap_meth_callback (fun _ js_code ->
               let result = Shared.get_bindings None js_code in
               let reason =
                 Reason.printRE
                   (Ppxlib.Selected_ast.Of_ocaml.copy_structure result, [])
               in
               Js.(obj [| ("code", (reason : Js.t)) |])) );
       |])
