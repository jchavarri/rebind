let get_with_default opt def = match opt with Some c -> c | None -> def

let try_find_id x xs =
  try Some (Shared_types.Identifiers.find x xs) with Not_found -> None

let try_find_in_list f xs = try Some (List.find f xs) with Not_found -> None
