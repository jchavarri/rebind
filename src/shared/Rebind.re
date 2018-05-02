let initState =
  SharedTypes.{
    rightSideTypes: [],
    outputTypes: [],
    outputExternals: [],
    identifiers: Identifiers.empty,
    parentContextName: "",
  };

let getOutput = statements =>
  statements
  |> List.fold_left(
       (accState, statement) => HandlePlainStatement.h(accState, statement),
       initState,
     )
  |> IrTransformer.transform;

let getBindings = (file, content) => {
  let parse_options =
    Some(
      Parser_env.{
        /***
         * Always parse ES proposal syntax. The user-facing config option to
         * ignore/warn/enable them is handled during inference so that a clean error
         * can be surfaced (rather than a more cryptic parse error).
         */
        esproposal_class_instance_fields: true,
        esproposal_class_static_fields: true,
        esproposal_decorators: true,
        esproposal_export_star_as: true,
        types: true,
        use_strict: false,
      },
    );
  let (ast, _) =
    Parser_flow.program_file(
      ~fail=false,
      ~parse_options,
      content,
      Some(Loc.SourceFile(file)),
    );
  let (_, statements, _) = ast;
  getOutput(statements);
};