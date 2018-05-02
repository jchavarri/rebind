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
  | DeclareExportDeclaration(_)
  | ImportDeclaration(_) => state
  };
};