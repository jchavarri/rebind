open Ast_helper
open Parsetree
open Shared_types
module OrigToSafeMap = Map.Make (String)

let find_with_default x xs =
  try Shared_types.Identifiers.find x xs with Not_found -> x

module SafeToOrigMap = Map.Make (String)

let external_type_to_string t =
  match t with
  | String _ -> "string"
  | Float -> "float"
  | Int -> "int"
  | Abstract a -> a
  | Unit -> "unit"
  | Fun (_, _) ->
      failwith
        "Fun type should be resolved before calling external_type_to_string"
  | Module n -> n
  | ModuleProperty (_name, local, _remote) -> local
  | Named (_, _) ->
      failwith
        "Named type should be resolved before calling external_type_to_string"

let type_constr externalType safeIds =
  {
    ptyp_desc =
      Ptyp_constr
        ( Ast_utils.ast_helper_str_lident
            [ find_with_default (external_type_to_string externalType) safeIds ],
          [] );
    ptyp_loc = default_loc.contents;
    ptyp_attributes = [];
    ptyp_loc_stack = [];
  }

let type_arrow a b label =
  {
    ptyp_desc = Ptyp_arrow (label, a, b);
    ptyp_loc = default_loc.contents;
    ptyp_attributes = [];
    ptyp_loc_stack = [];
  }

let type_from_external_types typesList safeIds =
  let rec root_type list =
    let rec process l acc =
      match (l, acc) with
      | [], Some t -> t
      | Fun (name, l) :: xl, Some a ->
          let funTypes = l @ [ Abstract (find_with_default name safeIds) ] in
          process xl (Some (type_arrow (root_type funTypes) a Nolabel))
      | Named (name, type_) :: xl, Some a ->
          process xl
            (Some
               (type_arrow (root_type [ type_ ]) a
                  (Labelled (find_with_default name safeIds))))
      | ( a :: xl,
          Some
            {
              ptyp_loc;
              ptyp_attributes;
              ptyp_desc = Ptyp_constr (bLoc, bList);
              ptyp_loc_stack = _;
            } ) ->
          process xl
            (Some
               (type_arrow (type_constr a safeIds)
                  {
                    ptyp_loc;
                    ptyp_attributes;
                    ptyp_desc = Ptyp_constr (bLoc, bList);
                    ptyp_loc_stack = [];
                  }
                  Nolabel))
      | a :: xl, None -> process xl (Some (type_constr a safeIds))
      | ( a :: xl,
          Some
            {
              ptyp_loc;
              ptyp_attributes;
              ptyp_desc = Ptyp_arrow (bLabel, bType1, bType2);
              ptyp_loc_stack = _;
            } ) ->
          process xl
            (Some
               (type_arrow (type_constr a safeIds)
                  {
                    ptyp_loc;
                    ptyp_attributes;
                    ptyp_desc = Ptyp_arrow (bLabel, bType1, bType2);
                    ptyp_loc_stack = [];
                  }
                  Nolabel))
      | [], None -> failwith "empty list passed to descFromTypes"
      | _, _ -> failwith "impossible"
    in
    process (List.rev list) None
  in
  root_type typesList

let external_with_attribute originalName types safeIds attr =
  let newName = OrigToSafeMap.find originalName safeIds in
  let outputAttrs, valueDescription =
    match attr with
    | Module -> ([ ("mel.module", None) ], originalName)
    | Send -> ([ ("mel.send", None) ], originalName)
    | Get -> ([ ("mel.get", None) ], originalName)
    | ObjectCreation -> ([ ("mel.obj", None) ], "")
    | Val -> ([], originalName)
    | NewAttr -> ([ ("mel.new", None) ], originalName)
    | ModuleAndNew -> ([ ("mel.new", None); ("mel.module", None) ], originalName)
    | ScopedModuleAndNew (name, scopeProperty) ->
        ([ ("mel.new", None); ("mel.module", Some name) ], scopeProperty)
    | ScopedModule (name, scopeProperty) ->
        ([ ("mel.module", Some name) ], scopeProperty)
  in
  let attrs =
    (outputAttrs
     |> List.map (fun (outputAttr, constant) ->
            let constExpr =
              (match constant with
               | Some c ->
                   [
                     {
                       pstr_desc =
                         Pstr_eval
                           ( {
                               pexp_desc =
                                 Pexp_constant
                                   (Pconst_string (c, Location.none, None));
                               pexp_loc = default_loc.contents;
                               pexp_attributes = [];
                               pexp_loc_stack = [];
                             },
                             [] );
                       pstr_loc = default_loc.contents;
                     };
                   ]
               | None -> []
                : Parsetree.structure_item list)
            in
            {
              attr_loc = default_loc.contents;
              attr_name = { txt = outputAttr; loc = Location.none };
              attr_payload = PStr constExpr;
            })
      : Parsetree.attributes)
  in
  let t =
    ({
       pval_name = { loc = default_loc.contents; txt = newName };
       pval_prim = [ valueDescription ];
       pval_loc = default_loc.contents;
       pval_type = type_from_external_types types safeIds;
       pval_attributes = attrs;
     }
      : Parsetree.value_description)
  in
  Str.primitive ~loc:Location.none t

let rec generate_safe_id ?(postFix = None) name safeToOrig =
  let strPostFix =
    match postFix with Some n -> string_of_int n | None -> ""
  in
  let safeName = Ast_utils.correct_identifier name ^ strPostFix in
  match (postFix, Utils.try_find_id safeName safeToOrig) with
  | Some n, Some _safeName ->
      generate_safe_id ~postFix:(Some (n + 1)) name safeToOrig
  | None, Some _existingId -> generate_safe_id ~postFix:(Some 2) name safeToOrig
  | _, None -> safeName

let safe_identifiers identifiers =
  let origToSafe, _safeToOrig =
    Identifiers.fold
      (fun name _externalType (origToSafe, safeToOrig) ->
        match Utils.try_find_id name origToSafe with
        | Some _existingId -> (origToSafe, safeToOrig)
        | None ->
            let safeId = generate_safe_id name safeToOrig in
            ( OrigToSafeMap.add name safeId origToSafe,
              SafeToOrigMap.add safeId name safeToOrig ))
      identifiers
      (OrigToSafeMap.empty, SafeToOrigMap.empty)
  in
  origToSafe

let transform state =
  let {
    identifiers;
    output_types;
    output_externals;
    output_statements;
    right_side_types = _;
    parent_context_name = _;
  } =
    state
  in
  let safeIds = safe_identifiers identifiers in
  (List.rev output_types
  |> List.map (fun outputType ->
         Str.type_ ~loc:Location.none Recursive
           [
             Type.mk ~loc:Location.none ~kind:Ptype_abstract ~priv:Public
               {
                 loc = default_loc.contents;
                 txt = OrigToSafeMap.find outputType safeIds;
               };
           ]))
  @ (List.rev output_externals
    |> List.map (fun structureItem ->
           external_with_attribute structureItem.name structureItem.types
             safeIds structureItem.attr))
  @ output_statements
