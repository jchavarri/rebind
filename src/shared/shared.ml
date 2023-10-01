let init_state =
  let open Shared_types in
  {
    right_side_types = [];
    output_types = [];
    output_externals = [];
    output_statements = [];
    identifiers = Identifiers.empty;
    parent_context_name = "";
  }

let get_output (statements : (Loc.t, Loc.t) Flow_ast.Statement.t list) =
  let state =
    statements
    |> List.fold_left
         (fun acc_state statement ->
           Handle_plain_statement.h acc_state statement)
         init_state
  in
  let state =
    {
      state with
      output_statements =
        (let result : Parsetree.structure =
           List.map Mapper.topStatementsMapper statements |> List.concat
         in
         result);
    }
  in
  Ir_transformer.transform state

let get_bindings file content =
  let parse_options =
    let open Parser_env in
    Some
      {
        components = true;
        enums = true;
        (*
         * Always parse ES proposal syntax. The user-facing config option to
         * ignore/warn/enable them is handled during inference so that a clean error
         * can be surfaced (rather than a more cryptic parse error).
         *)
        esproposal_decorators = true;
        types = false;
        use_strict = false;
        module_ref_prefix = None;
        module_ref_prefix_LEGACY_INTEROP = None;
      }
  in

  let (_, ast), _parse_errors =
    Parser_flow.program_file ~fail:false ~parse_options content file
  in
  (* if parse_errors <> [] then
       let converted =
         List.fold_left
           (fun acc parse_error ->
             Flow_errors_utils.ConcreteLocPrintableErrorSet.add
               (error_of_parse_error file parse_error)
               acc)
           Flow_errors_utils.ConcreteLocPrintableErrorSet.empty parse_errors
       in
       Error converted
     else Ok *)
  get_output ast.statements
