(* open Parser_flow.Ast.Literal

let h (state : Shared_types.state) { value; raw = _ } =
  match value with
  | String v -> (state, Shared_types.String v)
  | Number _ -> (state, Float)
  | Boolean _ | Null | RegExp _ -> (state, Unit) *)
