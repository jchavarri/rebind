let init_state =
  let open Shared_types in
  {
    right_side_types = [];
    outputTypes = [];
    outputExternals = [];
    identifiers = Identifiers.empty;
    parentContextName = "";
  }

let get_output statements =
  statements
  |> List.fold_left
       (fun acc_state statement -> Handle_plain_statement.h acc_state statement)
       init_state
  |> Ir_transformer.transform

let get_bindings file content =
  let parse_options =
    Some
      (let open Parser_env in
       {
         (* Always parse ES proposal syntax. The user-facing config option to
            ignore/warn/enable them is handled during inference so that a clean error
            can be surfaced (rather than a more cryptic parse error). *)
         esproposal_class_instance_fields = true;
         esproposal_class_static_fields = true;
         esproposal_decorators = true;
         esproposal_export_star_as = true;
         types = true;
         use_strict = false;
       })
  in
  let ast, _ =
    Parser_flow.program_file ~fail:true ~parse_options content
      (Option.map (fun f -> Loc.SourceFile f) file)
  in
  let _, statements, _ = ast in
  get_output statements
