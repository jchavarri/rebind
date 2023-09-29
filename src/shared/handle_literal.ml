type literal = String of string | Number of float

let h (state : Shared_types.state) literal =
  match literal with
  | String v -> (state, Shared_types.String v)
  | Number _ -> (state, Float)
