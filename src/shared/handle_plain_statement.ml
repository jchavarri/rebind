open Parser_flow.Ast.Statement

let h (state : Shared_types.state) (_, statement) =
  (* Reset the expression related state *)
  let state = { state with right_side_types = [] } in
  match statement with
  | VariableDeclaration { VariableDeclaration.declarations } -> (
      let _, { VariableDeclaration.Declarator.id = _loc, id; init } =
        List.hd declarations
      in
      match (init, id) with
      | Some e, Identifier { name = _, name } ->
          let state, lastType =
            Handle_expression.h { state with parentContextName = name } e
          in
          {
            state with
            identifiers =
              Shared_types.Identifiers.add name lastType state.identifiers;
          }
      | _, _ -> state)
  | Expression { expression } ->
      let state, _lastType = Handle_expression.h state expression in
      state
  | ImportDeclaration
      { importKind; source = _loc, { raw = _; value = source }; specifiers }
    -> (
      match (importKind, source) with
      | ImportValue, String name ->
          specifiers
          |> List.fold_left
               (fun state specifier ->
                 match specifier with
                 | ImportDeclaration.ImportDefaultSpecifier (_loc, specifierName)
                   ->
                     let state, _lastType =
                       Handle_expression.maybe_add_identifier
                         ~customType:
                           (ModuleProperty (name, specifierName, "default"))
                         state specifierName
                     in
                     state
                 | ImportNamedSpecifier { local; remote } ->
                     let _loc, localName =
                       Utils.get_with_default local remote
                     in
                     let _loc, remoteName = remote in
                     let state, _lastType =
                       Handle_expression.maybe_add_identifier
                         ~customType:
                           (ModuleProperty (name, localName, remoteName))
                         state localName
                     in
                     state
                 | ImportNamespaceSpecifier (_loc, (_anotherLoc, specifierName))
                   ->
                     let state, _lastType =
                       Handle_expression.maybe_add_identifier
                         ~customType:(Module name) state specifierName
                     in
                     state)
               state
      | ImportValue, _
      | ( ImportType,
          _ (* ImportType and ImportTypeof are flow imports (I think) *) )
      | ImportTypeof, _ ->
          state)
  | Return _ | If _ | Block _ | FunctionDeclaration _ | Empty
  | ClassDeclaration _ | ExportDefaultDeclaration _ | ExportNamedDeclaration _
  | Labeled _ | Break _ | Continue _ | With _ | TypeAlias _ | Switch _ | Throw _
  | Try _ | While _ | DoWhile _ | For _ | ForIn _ | ForOf _ | Debugger
  | InterfaceDeclaration _ | DeclareVariable _ | DeclareFunction _
  | DeclareClass _ | DeclareModule _ | DeclareModuleExports _
  | DeclareExportDeclaration _ ->
      state
