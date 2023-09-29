open Parser_flow.Ast.Expression
open Shared_types

type callable_expr = CallExpr | NewExpr

let get_state (state, _lastType) = state

let maybe_add_identifier ?customType state name =
  let lastType, outputTypes =
    match Utils.try_find_id name state.identifiers with
    | Some existingType -> (existingType, state.outputTypes)
    | None ->
        ( Utils.get_with_default customType (Abstract name),
          name :: state.outputTypes )
  in
  ( {
      state with
      outputTypes;
      identifiers = Identifiers.add name lastType state.identifiers;
    },
    lastType )

let maybe_add_external state externalAttr name inputTypes =
  let state, lastType = maybe_add_identifier state name in
  let name =
    match externalAttr with
    | ObjectCreation ->
        String.concat "" [ "make"; String.capitalize_ascii name ]
    | _ -> name
  in
  (* Add the potential `make*` identifier *)
  let state =
    { state with identifiers = Identifiers.add name lastType state.identifiers }
  in
  (* TODO: Try to match params with existing externals *)
  let existingExternal =
    Utils.try_find_in_list (fun e -> e.name = name) state.outputExternals
  in
  match existingExternal with
  | Some _ -> (state, lastType)
  | None ->
      let external_statement =
        { attr = externalAttr; name; types = inputTypes @ [ lastType ] }
      in
      ( {
          state with
          outputExternals = external_statement :: state.outputExternals;
        },
        lastType )

let rec handle_callable_expr exprType callee (_, arguments) state =
  let callee_loc, callee_exp = callee in
  let callee_name =
    match callee_exp with
    | Identifier (_, { name; comments = _ }) -> name
    | Member
        {
          _object;
          property = Member.PropertyIdentifier (_, { name; comments = _ });
          comments = _;
        } ->
        name
    | _ -> ""
  in
  let state, right_side_types =
    arguments.ArgList.arguments
    |> List.fold_left
         (fun (accState, accTypes) argument ->
           match argument with
           | Expression e ->
               let accState, lastType = h accState e in
               (accState, accTypes @ [ lastType ])
           | Spread _ -> failwith "Call.argument.Spread")
         ({ state with parentContextName = callee_name ^ "Param" }, [])
  in
  match callee_exp with
  (* Maybe this shouldn't be nested here, and the recursion should continue... *)
  | Identifier (_, { name; comments = _ }) -> (
      match (name, right_side_types) with
      | "require", String requireType :: [] -> (
          (* Discard the input types of this expression -they'll be just the string inside `require`-,
             and pick the ones passed from above in the state *)
          match List.length state.right_side_types > 0 with
          | true ->
              maybe_add_external state Module requireType state.right_side_types
          | false ->
              maybe_add_identifier ~customType:(Module requireType) state
                requireType)
      | _ -> (
          match (exprType, Utils.try_find_id name state.identifiers) with
          | NewExpr, Some (Module n) ->
              maybe_add_external state ModuleAndNew n right_side_types
          | NewExpr, _ -> maybe_add_external state NewAttr name right_side_types
          | _, Some (Module n) ->
              maybe_add_external state Module n right_side_types
          | _, Some (ModuleProperty (moduleName, local, remote)) ->
              maybe_add_external state
                (ScopedModule (moduleName, remote))
                local right_side_types
          | _, _ -> maybe_add_external state Val name right_side_types))
  | Member { _object; property; comments = _ } -> (
      let objectState, objectLastType = h state _object in
      match property with
      | Member.PropertyIdentifier (_, { name; comments = _ }) ->
          maybe_add_external objectState Send name
            ([ objectLastType ] @ right_side_types)
      | PropertyExpression _ -> failwith "Member.PropertyExpression"
      | PropertyPrivateName _ -> failwith "Member.PropertyPrivateName")
  | _ -> h { state with right_side_types } (callee_loc, callee_exp)

and h state (_, expression) =
  match expression with
  | Function f | ArrowFunction f ->
      let _loc, params = f.params in
      let funName =
        match f.id with
        | Some (_loc, { name; comments = _ }) -> name
        | None -> "Callback"
      in
      let state, types =
        params.params
        |> List.fold_left
             (fun (accState, accTypes)
                  ((_loc, p) : ('a, 'b) Flow_ast.Function.Param.t) ->
               match snd p.argument with
               | Parser_flow.Ast.Pattern.Identifier
                   { name = _loc, idName; annot = _; optional = _ } ->
                   let accState, lastType =
                     maybe_add_identifier accState idName.name
                   in
                   (accState, accTypes @ [ lastType ])
               | Object _ | Array _ | Expression _ ->
                   failwith "Unsupported pattern in Function or ArrowFunction")
             (state, [])
      in
      (maybe_add_identifier state funName |> get_state, Fun (funName, types))
  | Identifier (_, { name; comments = _ }) -> (
      match Utils.try_find_id name state.identifiers with
      | Some (Module n) -> maybe_add_external state Module n []
      | Some (ModuleProperty (moduleName, local, remote)) ->
          maybe_add_external state (ScopedModule (moduleName, remote)) local []
      | _ -> maybe_add_identifier state name)
  | Call { callee; arguments; targs = _; comments = _ } ->
      handle_callable_expr CallExpr callee arguments state
  | New { callee; arguments; targs = _; comments = _ } -> (
      match arguments with
      | Some arguments -> handle_callable_expr NewExpr callee arguments state
      | None -> failwith "New with arguments None")
  (* I know this duplication is ugly but yolo *)
  | Member { _object; property; comments = _ } -> (
      let objectState, objectLastType =
        match _object with
        | _, Identifier (_, { name; comments = _ }) -> (
            match Utils.try_find_id name state.identifiers with
            | Some (Module n) -> maybe_add_external state Module n []
            | Some (ModuleProperty (moduleName, local, remote)) ->
                maybe_add_external state
                  (ScopedModule (moduleName, remote))
                  local []
            | _ -> maybe_add_external state Val name [])
        | _ -> h state _object
      in
      match property with
      | Member.PropertyIdentifier (_, { name; comments = _ }) ->
          maybe_add_external objectState Get name [ objectLastType ]
      | PropertyExpression _ -> failwith "Member.PropertyExpression"
      | PropertyPrivateName _ -> failwith "Member.PropertyPrivateName")
  | Object obj ->
      let originalContextName = state.parentContextName in
      let state, objTypes =
        obj.properties
        |> List.fold_left
             (fun (accState, accTypes) property ->
               let propertyKey, propertyValue =
                 match property with
                 | Object.Property (_loc, property) -> (
                     match property with
                     | Init { key; value; shorthand = _ } -> (key, value)
                     | Method _ ->
                         failwith "Object property using unsupported Method"
                     | Get _ -> failwith "Object property using unsupported Get"
                     | Set _ -> failwith "Object property using unsupported Set"
                     )
                 | SpreadProperty _ ->
                     failwith "Spread properties are unsupported"
               in
               let propertyName =
                 match propertyKey with
                 | Identifier (_loc, { name; comments = _ }) ->
                     Ast_utils.correct_labelled_arg name
                 | Computed _ | StringLiteral _ | NumberLiteral _
                 | BigIntLiteral _ | PrivateName _ ->
                     failwith "Computed properties in objects are unsupported"
               in
               let propState, propType =
                 h
                   { accState with parentContextName = propertyName }
                   propertyValue
               in
               let namedType = Named (propertyName, propType) in
               (propState, namedType :: accTypes))
             (state, [])
      in
      maybe_add_external state ObjectCreation originalContextName
        (objTypes @ [ Unit ])
  | MetaProperty { meta = _, meta; property = _, property; comments = _ } ->
      let state, lastType = maybe_add_external state Val meta.name [] in
      maybe_add_external state Get property.name [ lastType ]
  | StringLiteral { value; raw = _; comments = _ } ->
      Handle_literal.h state (String value)
  | NumberLiteral { value; raw = _; comments = _ } ->
      Handle_literal.h state (Number value)
  | Array _ | Sequence _ | Unary _ | Binary _ | Assignment _ | Update _
  | Logical _ | Conditional _ | Yield _ | TemplateLiteral _ | TaggedTemplate _
  | JSXElement _ | Class _ | TypeCast _ | Import _ | JSXFragment _
  | BooleanLiteral _ | NullLiteral _ | BigIntLiteral _ | RegExpLiteral _
  | ModuleRefLiteral _ | OptionalCall _ | OptionalMember _ | Super _ | This _
  | TSTypeCast _ ->
      (state, Unit)
