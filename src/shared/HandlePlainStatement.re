open Parser_flow.Ast.Statement;

let h = (state: SharedTypes.state, (_, statement)) => {
  /* Reset the expression related state */
  let state = {...state, rightSideTypes: []};
  switch (statement) {
  | VariableDeclaration({VariableDeclaration.declarations}) =>
    let (_, {VariableDeclaration.Declarator.id: (_loc, id), init}) =
      List.hd(declarations);
    switch (init, id) {
    | (Some(e), Identifier({name: (_, name)})) =>
      let (state, lastType) = HandleExpression.h(state, e);
      {
        ...state,
        identifiers:
          SharedTypes.Identifiers.add(name, lastType, state.identifiers),
      };
    | (_, _) => state
    };
  | Expression({expression}) =>
    let (state, _lastType) = HandleExpression.h(state, expression);
    state;
  | ImportDeclaration({
      importKind,
      source: (_loc, {raw: _, value: source}),
      specifiers,
    }) =>
    switch (importKind, source) {
    | (ImportValue, String(name)) =>
      specifiers
      |> List.fold_left(
           (state, specifier) =>
             switch (specifier) {
             | ImportDeclaration.ImportDefaultSpecifier((_loc, specifierName)) =>
               let (state, _lastType) =
                 HandleExpression.maybeAddIdentifier(
                   ~customType=ModuleProperty(name, specifierName, "default"),
                   state,
                   specifierName,
                 );
               state;
             | ImportNamedSpecifier({local, remote}) =>
               let (_loc, localName) = Utils.getWithDefault(local, remote);
               let (_loc, remoteName) = remote;
               let (state, _lastType) =
                 HandleExpression.maybeAddIdentifier(
                   ~customType=ModuleProperty(name, localName, remoteName),
                   state,
                   localName,
                 );
               state;
             | ImportNamespaceSpecifier((_loc, (_anotherLoc, specifierName))) =>
               let (state, _lastType) =
                 HandleExpression.maybeAddIdentifier(
                   ~customType=Module(name),
                   state,
                   specifierName,
                 );
               state;
             },
           state,
         )
    | (ImportValue, _)
    | (ImportType, _)
    | (ImportTypeof, _) => state /* ImportType and ImportTypeof are flow imports (I think) */
    }
  | Return(_)
  | If(_)
  | Block(_)
  | FunctionDeclaration(_)
  | Empty
  | ClassDeclaration(_)
  | ExportDefaultDeclaration(_)
  | ExportNamedDeclaration(_)
  | Labeled(_)
  | Break(_)
  | Continue(_)
  | With(_)
  | TypeAlias(_)
  | Switch(_)
  | Throw(_)
  | Try(_)
  | While(_)
  | DoWhile(_)
  | For(_)
  | ForIn(_)
  | ForOf(_)
  | Debugger
  | InterfaceDeclaration(_)
  | DeclareVariable(_)
  | DeclareFunction(_)
  | DeclareClass(_)
  | DeclareModule(_)
  | DeclareModuleExports(_)
  | DeclareExportDeclaration(_) => state
  };
};