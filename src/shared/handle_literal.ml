type literal = String of string | Number of float

let is_int n =
  let intN = int_of_float n in
  float_of_int intN = n

let h (state : Shared_types.state) literal =
  match literal with
  | String v -> (state, Shared_types.String v)
  | Number n ->
      let typ =
        match is_int n with true -> Shared_types.Int | false -> Float
      in
      (state, typ)
