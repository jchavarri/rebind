let tryFindId = (x, xs) =>
  try (Some(SharedTypes.Identifiers.find(x, xs))) {
  | Not_found => None
  };

let tryFindInList = (f, xs) =>
  try (Some(List.find(f, xs))) {
  | Not_found => None
  };