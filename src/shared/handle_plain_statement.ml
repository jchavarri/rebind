open Parser_flow.Ast.Statement

let h (state : Shared_types.state) (_, statement) =
  (* Reset the expression related state *)
  let state = { state with right_side_types = [] } in
  match statement with
  | VariableDeclaration
      { VariableDeclaration.declarations; kind = _; comments = _ } -> (
      let _, { VariableDeclaration.Declarator.id = _loc, id; init } =
        List.hd declarations
      in
      match (init, id) with
      | ( Some e,
          Identifier
            { name = _, { name; comments = _ }; annot = _; optional = _ } ) ->
          let state, lastType =
            Handle_expression.h { state with parent_context_name = name } e
          in
          {
            state with
            identifiers =
              Shared_types.Identifiers.add name lastType state.identifiers;
          }
      | _, _ -> state)
  | Expression { expression; directive = _; comments = _ } ->
      let state, _lastType = Handle_expression.h state expression in
      state
  | ImportDeclaration
      {
        import_kind;
        source = _loc, { raw = _; value = source; comments = _ };
        specifiers;
        default;
        comments = _;
      } -> (
      let state =
        match default with
        | Some
            { identifier = _loc, identifier; remote_default_name_def_loc = _ }
          ->
            let state, _lastType =
              Handle_expression.maybe_add_identifier
                ~customType:
                  (ModuleProperty (source, identifier.name, "default"))
                state identifier.name
            in
            state
        | None -> state
      in
      match specifiers with
      | None -> state
      | Some specifier -> (
          match (import_kind, source) with
          | ImportValue, name -> (
              match specifier with
              | ImportNamespaceSpecifier (_loc, (_anotherLoc, specifierName)) ->
                  let state, _lastType =
                    Handle_expression.maybe_add_identifier
                      ~customType:(Module name) state specifierName.name
                  in
                  state
              | ImportNamedSpecifiers specifiers ->
                  specifiers
                  |> List.fold_left
                       (fun state
                            Parser_flow.Ast.Statement.ImportDeclaration.
                              {
                                local;
                                remote;
                                kind = _;
                                remote_name_def_loc = _;
                              } ->
                         let _loc, localName =
                           Utils.get_with_default local remote
                         in
                         let _loc, remoteName = remote in
                         let state, _lastType =
                           Handle_expression.maybe_add_identifier
                             ~customType:
                               (ModuleProperty
                                  (name, localName.name, remoteName.name))
                             state localName.name
                         in
                         state)
                       state)
          | ( ImportType,
              _ (* ImportType and ImportTypeof are flow imports (I think) *) )
          | ImportTypeof, _ ->
              state))
  | Return _ | If _ | Block _ | FunctionDeclaration _ | ClassDeclaration _
  | ExportDefaultDeclaration _ | ExportNamedDeclaration _ | Labeled _ | Break _
  | Continue _ | With _ | TypeAlias _ | Switch _ | Throw _ | Try _ | While _
  | DoWhile _ | For _ | ForIn _ | ForOf _ | InterfaceDeclaration _
  | DeclareVariable _ | DeclareFunction _ | DeclareClass _ | DeclareModule _
  | DeclareModuleExports _ | DeclareExportDeclaration _ | ComponentDeclaration _
  | Debugger _ | DeclareComponent _ | DeclareEnum _ | DeclareInterface _
  | DeclareTypeAlias _ | DeclareOpaqueType _ | Empty _ | EnumDeclaration _
  | OpaqueType _ ->
      state
