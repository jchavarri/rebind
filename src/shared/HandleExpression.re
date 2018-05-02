open Parser_flow.Ast.Expression;

open SharedTypes;

let isStringType = t =>
  switch (t) {
  | String(_) => true
  | _ => false
  };

let unsafeGetStringValue = t =>
  switch (t) {
  | String(a) => a
  | _ => failwith("")
  };

let getLastType = ((_state, lastType)) => lastType;

let getState = ((state, _lastType)) => state;

let maybeAddIdentifier = (state, name) => {
  let (lastType, outputTypes) =
    switch (Utils.tryFindId(name, state.identifiers)) {
    | Some(existingType) => (existingType, state.outputTypes)
    | None => (Abstract(name), [name, ...state.outputTypes])
    };
  ({...state, outputTypes}, lastType);
};

/*
   Modules are generally created lazily when a function is found to reduce output code.
   But because of this laziness, for modules that export an object,
   we might reach the identifier of a member expression and not have the
   module export available, so we create it at that point. That's what `maybeAddModule` is for.
 */
let maybeAddModule = (state, name) => {
  let (state, lastType) = maybeAddIdentifier(state, name);
  let existingModule =
    Utils.tryFindInList(
      e => e.name == name && e.attr == Module,
      state.outputExternals,
    );
  switch (existingModule) {
  | Some(_) => (state, lastType)
  | None => (
      {
        ...state,
        outputExternals: [
          {attr: Module, name, types: [lastType]},
          ...state.outputExternals,
        ],
      },
      lastType,
    )
  };
};

let addExternal = (state, externalAttr, name, inputTypes) => {
  let (state, lastType) = maybeAddIdentifier(state, name);
  let name =
    switch (externalAttr) {
    | ObjectCreation => String.concat("", ["make", String.capitalize(name)])
    | _ => name
    };
  /* TODO: Try to match params with existing externals */
  let existingExternal =
    Utils.tryFindInList(e => e.name == name, state.outputExternals);
  switch (existingExternal) {
  | Some(_) => (state, lastType)
  | None =>
    let externalStatement = {
      attr: externalAttr,
      name,
      types: inputTypes @ [lastType],
    };
    (
      {
        ...state,
        outputExternals: [externalStatement, ...state.outputExternals],
      },
      lastType,
    );
  };
};

let rec h = (state, (_, expression)) =>
  switch (expression) {
  | Function(f)
  | ArrowFunction(f) =>
    let (params, _spread) = f.params;
    let funName =
      switch (f.id) {
      | Some((_loc, name)) => name
      | None => "Callback"
      };
    let (state, types) =
      params
      |> List.fold_left(
           ((accState, accTypes), (_loc, p)) =>
             switch (p) {
             | Parser_flow.Ast.Pattern.Identifier({
                 name: (_loc, idName),
                 typeAnnotation: _,
                 optional: _,
               }) =>
               let (accState, lastType) =
                 maybeAddIdentifier(accState, idName);
               (accState, accTypes @ [lastType]);
             | Object(_)
             | Array(_)
             | Assignment(_)
             | Expression(_) =>
               failwith("Unsupported pattern in Function or ArrowFunction")
             },
           (state, []),
         );
    (maybeAddIdentifier(state, funName) |> getState, Fun(funName, types));
  | Identifier((_, name)) => maybeAddModule(state, name)
  | Literal(lit) => HandleLiteral.h(state, lit)
  | Call({callee: (calleeLoc, calleeExp), arguments}) =>
    let calleeName =
      switch (calleeExp) {
      | Identifier((_, name)) => name
      | Member({
          _object,
          property: Member.PropertyIdentifier((_, name)),
          computed: _,
        }) => name
      | _ => ""
      };
    let (state, rightSideTypes) =
      arguments
      |> List.fold_left(
           ((accState, accTypes), argument) =>
             switch (argument) {
             | Expression(e) =>
               let (accState, lastType) = h(accState, e);
               (accState, accTypes @ [lastType]);
             | Spread((_, _)) => failwith("Call.argument.Spread")
             },
           (
             {
               ...state,
               parentContextName: String.concat("", [calleeName, "Param"]),
             },
             [],
           ),
         );
    switch (calleeExp) {
    /* Maybe this shouldn't be nested here, and the recursion should continue... */
    | Identifier((_, name)) =>
      switch (name, rightSideTypes) {
      | ("require", [String(requireType)]) =>
        /* Discard the input types of this expression -they'll be just the string inside `require`-,
           and pick the ones passed from above in the state */
        List.length(state.rightSideTypes) > 0 ?
          addExternal(state, Module, requireType, state.rightSideTypes) :
          maybeAddIdentifier(state, requireType)
      | _ => addExternal(state, Module, name, rightSideTypes)
      }
    | Member({_object, property, computed: _}) =>
      let (objectState, objectLastType) = h(state, _object);
      switch (property) {
      | Member.PropertyIdentifier((_, name)) =>
        addExternal(
          objectState,
          Send,
          name,
          [objectLastType] @ rightSideTypes,
        )
      | Member.PropertyExpression(_) => failwith("Member.PropertyExpression")
      };
    | _ => h({...state, rightSideTypes}, (calleeLoc, calleeExp))
    };
  /* I know this duplication is ugly but yolo */
  | Member({_object, property, computed: _}) =>
    let (objectState, objectLastType) =
      switch (_object) {
      | (_, Identifier((_, name))) => addExternal(state, Module, name, [])
      | _ => h(state, _object)
      };
    switch (property) {
    | Member.PropertyIdentifier((_, name)) =>
      addExternal(objectState, Get, name, [objectLastType])
    | PropertyExpression(_) => failwith("Member.PropertyExpression")
    };
  | Object(obj) =>
    let (state, objTypes) =
      obj.properties
      |> List.fold_left(
           ((accState, accTypes), property) => {
             let property =
               switch (property) {
               | Object.Property((_loc, property)) => property
               | SpreadProperty(_) =>
                 failwith("Spread properties are unsupported")
               };
             let (propState, propType) = h(accState, property.value);
             let propertyName =
               switch (property.key) {
               | Identifier((_loc, name)) => name
               | Literal(_)
               | Computed(_) =>
                 failwith("Computed properties in objects are unsupported")
               };
             let namedType = Named(propertyName, propType);
             (propState, [namedType, ...accTypes]);
           },
           (state, []),
         );
    addExternal(state, ObjectCreation, state.parentContextName, objTypes @ [Unit]);
  | This
  | Super
  | Array(_)
  | Sequence(_)
  | Unary(_)
  | Binary(_)
  | Assignment(_)
  | Update(_)
  | Logical(_)
  | Conditional(_)
  | New(_)
  | Yield(_)
  | Comprehension(_)
  | Generator(_)
  | TemplateLiteral(_)
  | TaggedTemplate(_)
  | JSXElement(_)
  | Class(_)
  | TypeCast(_)
  | MetaProperty(_) => (state, Unit)
  };