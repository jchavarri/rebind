let keepSome = lst =>
  lst
  |> List.filter(a =>
       switch (a) {
       | None => false
       | _ => true
       }
     );

let optMap = (opt, f, def) =>
  switch (opt) {
  | Some(s) => f(s)
  | None => def
  };

let tryFindId = (x, xs) =>
  try (Some(SharedTypes.Identifiers.find(x, xs))) {
  | Not_found => None
  };

let tryFindInList = (f, xs) =>
  try (Some(List.find(f, xs))) {
  | Not_found => None
  };