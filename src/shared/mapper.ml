open Ast_helper
open Asttypes
open Parsetree
open Longident

(* todo: consolidate with handle_literal *)
type literal =
  | String of string
  | Boolean of bool
  | Number of float
  | Null
  | RegExp

(* helpers *)
(* TODO: turn foo_bar into foo_bar_ *)
let correctIdentifier ident =
  let rec stripLeadingUnderscores s =
    if String.length s = 0 then s
    else if s.[0] = '_' then
      stripLeadingUnderscores (String.sub s 1 (String.length s - 1))
    else s
  in
  (* OCaml identifiers need to be lower-cased (uppercase reserved for variants constructors, modules, etc.) *)
  if ident = "" then ident
  else
    (* foo => foo
       Foo => foo
       _foo => foo
       _foo_bar => foo_bar_ *)
    let correctedName = stripLeadingUnderscores ident in
    let correctedName =
      match String.contains correctedName '_' with
      | true -> correctedName ^ "_"
      | false -> correctedName
    in
    let correctedName = String.uncapitalize_ascii correctedName in
    (* correct other cases where the js name is a reserved OCaml / Reason keyword *)
    match correctedName with
    | "object" -> "object_"
    | "type" -> "type_"
    | n -> n

let astHelperStrLidStr ?(correct = true) name =
  {
    loc = Location.none;
    txt = (match correct with true -> correctIdentifier name | false -> name);
  }

let astHelperStrAttr ?correct name payload : Parsetree.attribute =
  {
    attr_name = astHelperStrLidStr ?correct name;
    attr_payload = payload;
    attr_loc = Location.none;
  }

let astHelperStrLidIdent ?(correct = true) a =
  match a with
  | [] -> raise (Invalid_argument "identifier is empty.")
  | _ ->
      let inner =
        Lident
          (match correct with
          | true -> correctIdentifier (List.hd a)
          | false -> List.hd a)
      in
      let res =
        List.tl a
        |> List.fold_left
             (fun acc curr ->
               Ldot
                 ( acc,
                   match correct with
                   | true -> correctIdentifier curr
                   | false -> curr ))
             inner
      in
      { loc = default_loc.contents; txt = res }

let expUnit = Exp.construct (astHelperStrLidIdent ~correct:false [ "()" ]) None

(* using this as a convenient placeholder output, for checking whether I've matched the js ast correctly *)
let placeholder s = Exp.ident (astHelperStrLidIdent [ s ])
let expMarker = placeholder "marker"

let parseTreeValueBinding ~pat ~expr =
  {
    pvb_pat = pat;
    pvb_expr = expr;
    pvb_attributes = [];
    pvb_loc = default_loc.contents;
    pvb_constraint = None;
  }

let listToListAst lst =
  let nullList =
    Exp.construct (astHelperStrLidIdent ~correct:false [ "[]" ]) None
  in
  (* we transform js array to OCaml array, except in the case of jsx which takes an OCaml list instead *)
  match lst with
  | [] -> nullList
  | oneItemOrMore ->
      List.rev oneItemOrMore
      |> List.fold_left
           (fun accumExp expr ->
             Exp.construct
               (astHelperStrLidIdent ~correct:false [ "::" ])
               (Some (Exp.tuple [ expr; accumExp ])))
           nullList

type insideReactClass = Nope | InsidePropTypes | Yes of string

type context = {
  terminalExpr : Parsetree.expression option;
  insideReactClass : insideReactClass;
  mutable reactClassSpecRandomProps : string list list;
}

let attemptToGenerateStateRecord initialStateDeclaration =
  let rec innerMostExpr pexp_desc =
    match pexp_desc with
    | Pexp_extension
        ( { txt = "mel.obj"; _ },
          PStr
            ({
               pstr_desc = Pstr_eval ({ pexp_desc = Pexp_record (lst, _); _ }, _);
               _;
             }
            :: []) ) ->
        Some lst
    | Pexp_let (_, _, { pexp_desc; _ })
    | Pexp_sequence (_, { pexp_desc; _ })
    | Pexp_constraint ({ pexp_desc; _ }, _) ->
        innerMostExpr pexp_desc
    | _ -> None
  in
  let bailType =
    Str.type_ Nonrecursive
      [
        Type.mk ~kind:Ptype_abstract ~priv:Public
          ~manifest:
            {
              ptyp_loc = default_loc.contents;
              ptyp_attributes = [];
              ptyp_loc_stack = [];
              ptyp_desc =
                Ptyp_constr
                  ( { loc = default_loc.contents; txt = Ldot (Lident "Js", "t") },
                    [
                      {
                        ptyp_loc = default_loc.contents;
                        ptyp_attributes = [];
                        ptyp_loc_stack = [];
                        ptyp_desc =
                          Ptyp_object
                            ( [
                                {
                                  pof_desc =
                                    Otag
                                      ( {
                                          loc = Location.none;
                                          txt = "cantAnalyzeComplexStateType";
                                        },
                                        {
                                          ptyp_loc = default_loc.contents;
                                          ptyp_attributes = [];
                                          ptyp_loc_stack = [];
                                          ptyp_desc =
                                            Ptyp_constr
                                              ( astHelperStrLidIdent
                                                  [
                                                    "pleaseProvideTheShapeOfStateManually";
                                                  ],
                                                [] );
                                        } );
                                  pof_loc = Location.none;
                                  pof_attributes = [];
                                };
                              ],
                              Closed );
                      };
                    ] );
            }
          (astHelperStrLidStr "state");
      ]
  in
  (* drill into the function AST to get the shape of the return value *)
  match initialStateDeclaration with
  | {
   pcf_desc =
     Pcf_method
       ( _,
         _,
         Cfk_concrete
           ( _,
             {
               pexp_desc =
                 Pexp_poly
                   ( {
                       pexp_desc =
                         Pexp_fun
                           ( _,
                             _,
                             _,
                             {
                               pexp_desc =
                                 Pexp_constraint ({ pexp_desc = a; _ }, _);
                               _;
                             } );
                       _;
                     },
                     None );
               _;
             } ) );
   _;
  } -> (
      match innerMostExpr a with
      | Some returnExpression ->
          let fields =
            List.map
              (fun ({ txt; _ }, _) ->
                let label =
                  match txt with
                  | Lident txt -> txt
                  | _ -> "cannotConvertStateKeyOver"
                in
                Type.field (astHelperStrLidStr label)
                  {
                    ptyp_loc = default_loc.contents;
                    ptyp_attributes = [];
                    ptyp_loc_stack = [];
                    ptyp_desc =
                      Ptyp_constr
                        ( astHelperStrLidIdent
                            [ "pleaseFillTheTypeForThisKeyManually" ],
                          [] );
                  })
              returnExpression
          in
          Str.type_ Nonrecursive
            [ Type.mk ~kind:(Ptype_record fields) (astHelperStrLidStr "state") ]
      | None -> bailType)
  | _ -> bailType

let rec statementBlockMapper ~context
    ({ Parser_flow.Ast.Statement.Block.body; _ } :
      (_, _) Parser_flow.Ast.Statement.Block.t) : Parsetree.expression =
  match body with
  | [] -> expUnit
  | bodyNotEmpty ->
      let bodyNotEmptyFlipped = List.rev bodyNotEmpty in
      let lastItemReason =
        List.hd bodyNotEmptyFlipped
        |> statementMapper ~context:{ context with terminalExpr = None }
      in
      List.tl bodyNotEmptyFlipped
      |> List.fold_left
           (fun accumExp statement ->
             statementMapper
               ~context:{ context with terminalExpr = Some accumExp }
               statement)
           lastItemReason

and functionMapper ~context ~returnType
    ({ Parser_flow.Ast.Function.params = _, params; body; _ } :
      (Loc.t, Loc.t) Parser_flow.Ast.Function.t) =
  let open Parser_flow.Ast in
  (* functionMapper is currently the only one using context.addPropsAndStateDeclImmediately. It wraps around the
     function in props and state declaration. don't forget to unset `context.addPropsAndStateDeclImmediately`; *)
  let bodyReason =
    match body with
    | Function.BodyExpression expression -> expressionMapper ~context expression
    | Function.BodyBlock (_, body) -> statementBlockMapper ~context body
  in
  let wrapBodyInReturnType body =
    match returnType with
    | None -> body
    | Some typeName ->
        Exp.constraint_ body (Typ.constr (astHelperStrLidIdent [ typeName ]) [])
  in
  match params.params with
  | [] ->
      Exp.fun_ Nolabel None
        (Pat.construct (astHelperStrLidIdent ~correct:false [ "()" ]) None)
        (wrapBodyInReturnType bodyReason)
  | (_, { argument = _, first; _ }) :: rest -> (
      let partialResult =
        List.rev rest
        |> List.fold_left
             (fun expr'
                  ((_, { argument = _, argument; _ }) :
                    (Loc.t, Loc.t) Flow_ast.Function.Param.t) ->
               match argument with
               | Pattern.Identifier
                   { Pattern.Identifier.name = _, { name; _ }; _ } ->
                   Exp.fun_ Nolabel None
                     (Pat.construct (astHelperStrLidIdent [ name ]) None)
                     expr'
               | Pattern.Object _ | Pattern.Array _ | Pattern.Expression _ ->
                   Exp.fun_ Nolabel None
                     (Pat.var (astHelperStrLidStr "fixme"))
                     expr')
             bodyReason
      in
      match first with
      | Pattern.Identifier { Pattern.Identifier.name = _, { name; _ }; _ } ->
          Exp.fun_ Nolabel None
            (Pat.construct (astHelperStrLidIdent [ name ]) None)
            (wrapBodyInReturnType partialResult)
      | _ ->
          Exp.fun_ Nolabel None
            (Pat.var (astHelperStrLidStr "fixme"))
            (wrapBodyInReturnType partialResult))

and literalMapper ?(isNegativeNumber = false) literal =
  match literal with
  | String s -> Exp.constant (Pconst_string (s, Location.none, None))
  | Boolean boolean ->
      Exp.ident
        (astHelperStrLidIdent ~correct:false
           [ "Js"; (match boolean with true -> "true_" | false -> "false_") ])
  | Null -> Exp.ident (astHelperStrLidIdent ~correct:false [ "Js"; "null" ])
  | Number n ->
      let intN = int_of_float n in
      if float_of_int intN = n then
        Exp.constant
          (Pconst_integer
             ( string_of_int
                 (match isNegativeNumber with true -> -intN | false -> intN),
               None ))
      else
        Exp.constant
          (Pconst_float
             ( string_of_float
                 (match isNegativeNumber with true -> -.n | false -> n),
               None ))
  | RegExp -> placeholder "regexPlaceholder"

and jsxElementMapper ~context
    ({
       Parser_flow.Ast.JSX.opening_element =
         _, { Parser_flow.Ast.JSX.Opening.name; attributes; _ };
       children = _, children;
       _;
     } :
      (Loc.t, Loc.t) Parser_flow.Ast.JSX.element) =
  let open Parser_flow.Ast.JSX in
  match name with
  | Identifier (_, { Identifier.name; _ }) ->
      let jsxPropHasHyphen =
        List.exists
          (fun attr ->
            match attr with
            | Opening.Attribute
                ( _,
                  {
                    Attribute.name =
                      Attribute.Identifier (_, { Identifier.name; _ });
                    _;
                  } ) ->
                String.contains name '-'
            | _ -> false)
          attributes
      in
      let childrenReact =
        List.filter_map (fun child -> jsxChildMapper ~context child) children
      in
      let constructRecordOrLabels f =
        List.map
          (fun attr ->
            match attr with
            | Opening.Attribute (_, { Attribute.name; value }) -> (
                (* JSX's <Foo checked /> is sugar for <Foo checked={true} />. What a waste *)
                let valueReason =
                  match value with
                  | None ->
                      Exp.ident
                        (astHelperStrLidIdent ~correct:false [ "Js"; "true_" ])
                  | Some (Attribute.StringLiteral (_, lit)) ->
                      literalMapper (String lit.value)
                  | Some
                      (Attribute.ExpressionContainer
                        (_, { ExpressionContainer.expression; _ })) -> (
                      match expression with
                      | ExpressionContainer.Expression expr ->
                          expressionMapper ~context expr
                      | ExpressionContainer.EmptyExpression -> expUnit)
                in
                match name with
                | Attribute.Identifier (_, { Identifier.name; _ }) ->
                    (f name, valueReason)
                | Attribute.NamespacedName _ ->
                    (f "NamespacedName", placeholder "notImplementedYet"))
            | Opening.SpreadAttribute _ ->
                (f "spreadAttrbute", placeholder "notImplementedYet"))
          attributes
      in
      if jsxPropHasHyphen then
        (* if there's a hyphen (e.g. aria-label) then we can't transform it into a jsx function label (invalid syntax). We'll
           have to take a shortcut and use a bs.obj instead *)
        let jsObj =
          Exp.extension
            ( astHelperStrLidStr "mel.obj",
              PStr
                [
                  Str.eval
                    (Exp.record
                       (constructRecordOrLabels (fun name ->
                            astHelperStrLidIdent [ name ]))
                       None);
                ] )
        in
        Exp.apply
          (Exp.ident
             (astHelperStrLidIdent ~correct:false
                [ "ReactRe"; "createElement" ]))
          [
            (Nolabel, Exp.ident (astHelperStrLidIdent ~correct:false [ name ]));
            (Nolabel, jsObj);
            (Nolabel, Exp.array childrenReact);
          ]
      else
        let partialArguments =
          constructRecordOrLabels (fun name ->
              Labelled (correctIdentifier name))
        in
        (* add children *)
        let arguments =
          partialArguments @ [ (Nolabel, listToListAst childrenReact) ]
        in
        Exp.apply
          ~attrs:[ astHelperStrAttr ~correct:false "JSX" (PStr []) ]
          (Exp.ident (astHelperStrLidIdent ~correct:false [ name ]))
          arguments
  | MemberExpression (_, { MemberExpression._object; _ }) ->
      placeholder "complexJSXYet"
  | NamespacedName _ -> placeholder "noNameSpaceJSXYet"

and jsxChildMapper ~context
    ((_, child) : (Loc.t, Loc.t) Parser_flow.Ast.JSX.child) =
  let open Parser_flow.Ast.JSX in
  match child with
  | Parser_flow.Ast.JSX.Element element ->
      Some (jsxElementMapper ~context element)
  | ExpressionContainer { ExpressionContainer.expression; _ } -> (
      match expression with
      | ExpressionContainer.Expression expr ->
          Some (expressionMapper ~context expr)
      | ExpressionContainer.EmptyExpression -> Some expUnit)
  | Text { Text.value; _ } ->
      (* JS jsx is whitespace sensitive and scatters around "\n          " in the AST *)
      let trimmedValue = String.trim value in
      if trimmedValue = "" then None
      else
        Some (Exp.constant (Pconst_string (trimmedValue, Location.none, None)))
  | Fragment _ -> failwith "todo: jsxChildMapper Fragment"
  | SpreadChild _ -> failwith "todo: jsxChildMapper SpreadChild"

and objectMapper ~context
    ({ Parser_flow.Ast.Expression.Object.properties; _ } :
      (Loc.t, Loc.t) Flow_ast.Expression.Object.t) =
  let open Parser_flow.Ast.Expression.Object in
  match properties with
  | [] ->
      Exp.extension
        ( astHelperStrLidStr "mel.raw",
          PStr
            [
              Str.eval
                (Exp.constant (Pconst_string ("{}", Location.none, None)));
            ] )
  | properties ->
      Exp.extension
        ( astHelperStrLidStr "mel.obj",
          PStr
            [
              Str.eval
                (Exp.record
                   (List.map
                      (fun property ->
                        match property with
                        | Property (_, Property.Init { key; value; _ }) ->
                            let keyReason =
                              match key with
                              | Property.StringLiteral (_, { value = name; _ })
                                ->
                                  [ name ]
                              | NumberLiteral (_, { value; _ }) ->
                                  [ string_of_float value ]
                              | BigIntLiteral (_, { value; _ }) -> (
                                  match value with
                                  | Some v -> [ Int64.to_string v ]
                                  | None -> [ "int64IsNone" ])
                              | PrivateName (_, { name; _ })
                              | Identifier (_, { name; _ }) ->
                                  [ name ]
                              | Computed _ -> [ "notThereYet" ]
                            in
                            ( astHelperStrLidIdent keyReason,
                              expressionMapper ~context value )
                        | Property (_, (Method _ | Get _ | Set _)) ->
                            ( astHelperStrLidIdent
                                [
                                  "objectPropertyMethodGetSetNotImplementedYet";
                                ],
                              placeholder
                                "objectPropertyMethodGetSetNotImplementedYet" )
                        | SpreadProperty _ ->
                            ( astHelperStrLidIdent
                                [ "objectSpreadNotImplementedYet" ],
                              placeholder "objectSpreadNotImplementedYet" ))
                      properties)
                   None);
            ] )

and memberMapper ~context
    {
      Parser_flow.Ast.Expression.Member._object = (_, _object) as objectWrap;
      property;
      _;
    } =
  let open Parser_flow.Ast.Expression in
  (* heuristics: if it's Foo.bar, transform into Foo.bar in ocaml (module property). If it's foo.bar,
     transform into foo##bar, which Melange will pick up and compile (back) into dot. *)
  (* TODO: actually implement this *)
  let defaultCase () =
    let left = expressionMapper ~context objectWrap in
    match property with
    | Member.PropertyExpression ((_, NumberLiteral { value = n; _ }) as expr) ->
        (* foo[1] => foo.(1); *)
        let intN = int_of_float n in
        if float_of_int intN = n then
          Exp.apply
            (Exp.ident (astHelperStrLidIdent ~correct:false [ "Array"; "get" ]))
            [
              (Nolabel, left);
              (Nolabel, Exp.constant (Pconst_integer (string_of_int intN, None)));
            ]
        else expressionMapper ~context expr
    (* astexplorer flow says foo[bar], bar is an identifier. In reality this flow parses it as an expression *)
    | PropertyIdentifier (_, { name; _ }) -> (
        match _object with
        | Identifier (_, { name = "reactDOM" | "ReactDOM"; _ }) ->
            (* ReactDOM.foo -> ReactDOMRe.foo *)
            Exp.ident
              (astHelperStrLidIdent ~correct:false [ "ReactDOMRe"; name ])
        | _ ->
            (* foo.bar => foo##bar; *)
            Exp.apply
              (Exp.ident (astHelperStrLidIdent ~correct:false [ "##" ]))
              [
                (Nolabel, left);
                (Nolabel, Exp.ident (astHelperStrLidIdent [ name ]));
              ])
    | PropertyExpression ((_, _expr) as exprWrap) -> (
        match _object with
        | Identifier (_, { name = "reactDOM" | "ReactDOM"; _ }) ->
            let name =
              match property with
              | Member.PropertyIdentifier (_, { name; _ }) -> name
              | _ -> "notSureWhatThisIs"
            in
            Exp.apply
              (Exp.ident
                 (astHelperStrLidIdent ~correct:false [ "ReactDOMRe"; name ]))
              [ (Nolabel, expressionMapper ~context exprWrap) ]
        | _ ->
            (* if computed then
                      (* foo.[bar] => foo.(bar); treat as array *)
               Exp.apply
                        (Exp.ident
                           (astHelperStrLidIdent ~correct:false [ "Array"; "get" ]))
                        [
                          (Nolabel, left); (Nolabel, expressionMapper ~context exprWrap);
                        ]
                    else *)
            (* foo.bar => foo##bar; *)
            Exp.apply
              (Exp.ident (astHelperStrLidIdent ~correct:false [ "##" ]))
              [ (Nolabel, left); (Nolabel, expressionMapper ~context exprWrap) ]
        )
    | PropertyPrivateName _ ->
        placeholder "memberMapperPropertyPrivateNameNotImplementedYet"
  in
  match context.insideReactClass with
  | InsidePropTypes -> (
      match property with
      | Member.PropertyIdentifier (_, { name; _ }) -> (
          match name with
          | "isRequired" ->
              Exp.apply
                (Exp.ident
                   (astHelperStrLidIdent ~correct:false
                      [ "ReactRe"; "PropTypes"; name ]))
                [ (Nolabel, expressionMapper ~context objectWrap) ]
          | "oneOfType" | "oneOf" | "objectOf" | "instanceOf" | "arrayOf"
          | "string" | "bool" | "number" | "node" | "symbol" | "any" | "element"
          | "func" | "shape" ->
              Exp.ident
                (astHelperStrLidIdent ~correct:false
                   [ "ReactRe"; "PropTypes"; name ])
          | "object" ->
              Exp.ident
                (astHelperStrLidIdent ~correct:false
                   [ "ReactRe"; "PropTypes"; "object_" ])
          | _unrecognizedPropName -> defaultCase ())
      | PropertyExpression expr -> expressionMapper ~context expr
      | PropertyPrivateName _ ->
          placeholder "memberMapperPropertyPrivateNameNotImplementedYet")
  | Yes _ -> (
      match (_object, property) with
      | ( This _,
          Member.PropertyIdentifier
            (_, { name = ("props" | "state") as name; _ }) ) ->
          (* we turn `this.props` (assumed to be a react class) into just `props`. Same for `state`. *)
          Exp.ident (astHelperStrLidIdent [ name ])
      | _ -> defaultCase ())
  | Nope -> defaultCase ()

and statementMapper ~context statement =
  let (_, statement) : (Loc.t, Loc.t) Flow_ast.Statement.t = statement in
  let open Parser_flow.Ast.Statement in
  let open Parser_flow.Ast in
  match statement with
  | VariableDeclaration { VariableDeclaration.declarations; _ } -> (
      (* this is the part that transforms non-top-level var declarations list (in a single var
         declaration) from js to let binding with a tuple in OCaml *)
      (* TODO: actually do this  *)
      let _, { Statement.VariableDeclaration.Declarator.id = _, id; init } =
        List.hd declarations
      in
      let context, isReactClassDecl =
        match (init, id) with
        | ( Some
              ( _,
                Expression.Call
                  {
                    Expression.Call.callee =
                      ( _,
                        Expression.Member
                          {
                            Expression.Member._object =
                              _, Expression.Identifier (_, { name = "React"; _ });
                            property =
                              Expression.Member.PropertyIdentifier
                                (_, { name = "createClass"; _ });
                            _;
                          } );
                    arguments;
                    _;
                  } ),
            Pattern.Identifier { Pattern.Identifier.name = _, { name; _ }; _ } )
          ->
            ({ context with insideReactClass = Yes name }, Some arguments)
        | _ -> (context, None)
      in
      let expr =
        match init with
        | None ->
            Exp.construct (astHelperStrLidIdent ~correct:false [ "None" ]) None
        | Some e -> expressionMapper ~context e
      in
      let innerMostExpr =
        match context.terminalExpr with None -> expUnit | Some expr -> expr
      in
      match id with
      | Pattern.Identifier { Pattern.Identifier.name = _, { name; _ }; _ } ->
          Exp.let_ Nonrecursive
            (* every react component must be called comp so that we could do Foo.comp on it for JSX *)
            [
              parseTreeValueBinding
                ~pat:
                  (Pat.var
                     (astHelperStrLidStr
                        (match isReactClassDecl with
                        | None -> name
                        | Some _ -> "comp")))
                ~expr;
            ]
            innerMostExpr
      | Pattern.Object { Pattern.Object.properties; _ } -> (
          match (properties, init) with
          | _ ->
              Exp.let_ Nonrecursive
                [
                  parseTreeValueBinding
                    ~pat:
                      (Pat.constant
                         (Pconst_string
                            ("destructuringNotImplemented", Location.none, None)))
                    ~expr;
                ]
                innerMostExpr)
      | Pattern.Array _ | Pattern.Expression _ ->
          Exp.let_ Nonrecursive
            [
              parseTreeValueBinding
                ~pat:
                  (Pat.constant
                     (Pconst_string
                        ("destructuringNotImplemented", Location.none, None)))
                ~expr;
            ]
            innerMostExpr)
  | Return { Return.argument; _ } -> (
      let result =
        match argument with
        | None -> expUnit
        | Some expr -> expressionMapper ~context expr
      in
      match context.terminalExpr with
      | None -> result
      | Some expr -> Exp.sequence result expr)
  | Expression { Statement.Expression.expression; _ } -> (
      match context.terminalExpr with
      | None -> expressionMapper ~context expression
      | Some expr -> Exp.sequence (expressionMapper ~context expression) expr)
  | If { test; consequent; alternate; _ } -> (
      let result =
        Exp.ifthenelse
          (expressionMapper ~context test)
          (statementMapper
             ~context:{ context with terminalExpr = None }
             consequent)
          (match alternate with
          | None -> None
          | Some (_, statement) ->
              Some
                (statementMapper
                   ~context:{ context with terminalExpr = None }
                   statement.body))
      in
      match context.terminalExpr with
      | None -> result
      | Some expr -> Exp.sequence result expr)
  | Block body -> statementBlockMapper ~context body
  | FunctionDeclaration functionWrap ->
      let funcName =
        match functionWrap.Function.id with
        | None -> "thisNameShouldntAppearPleaseReport"
        | Some (_, { name; _ }) -> name
      in
      let innerMostExpr =
        match context.terminalExpr with None -> expUnit | Some expr -> expr
      in
      Exp.let_ Nonrecursive
        [
          parseTreeValueBinding
            ~pat:(Pat.var (astHelperStrLidStr funcName))
            ~expr:(functionMapper ~context ~returnType:None functionWrap);
        ]
        innerMostExpr
  | Empty _ -> (
      match context.terminalExpr with None -> expUnit | Some expr -> expr)
  | ClassDeclaration _ -> placeholder "ClassDeclarationNotImplementedYet"
  | ExportDefaultDeclaration { ExportDefaultDeclaration.declaration; _ } -> (
      match declaration with
      | ExportDefaultDeclaration.Declaration decl ->
          statementMapper ~context decl
      | ExportDefaultDeclaration.Expression expr ->
          expressionMapper ~context expr)
  | ExportNamedDeclaration { ExportNamedDeclaration.declaration; _ } -> (
      match declaration with
      | None -> expUnit
      | Some statement -> statementMapper ~context statement)
  | Labeled _ | Break _ | Continue _ | With _ | TypeAlias _ | Switch _ | Throw _
  | Try _ | While _ | DoWhile _ | For _ | ForIn _ | ForOf _
  | ComponentDeclaration _ | Debugger _ | DeclareComponent _ | DeclareEnum _
  | DeclareInterface _ | DeclareTypeAlias _ | DeclareOpaqueType _
  | EnumDeclaration _ | OpaqueType _ | InterfaceDeclaration _
  | DeclareVariable _ | DeclareFunction _ | DeclareClass _ | DeclareModule _
  | DeclareModuleExports _ | DeclareExportDeclaration _ | ImportDeclaration _
    -> (
      match context.terminalExpr with
      | None -> placeholder "statementBail"
      | Some expr -> Exp.sequence (placeholder "statementBail") expr)

and expressionMapper ~context
    ((_, expression) : (Loc.t, Loc.t) Parser_flow.Ast.Expression.t) :
    Parsetree.expression =
  let open Parser_flow.Ast in
  let open Parser_flow.Ast.Expression in
  match expression with
  | Object obj -> objectMapper ~context obj
  | ArrowFunction functionWrap | Function functionWrap ->
      functionMapper ~context ~returnType:None functionWrap
  | Call
      {
        Call.callee = (_, callee) as calleeWrap;
        arguments = _, { arguments; _ };
        _;
      } -> (
      let argumentsIntoReasonArguments arguments =
        List.map
          (fun argument ->
            match argument with
            | Expression e -> expressionMapper ~context e
            | Spread (_, _) -> placeholder "argumentSpreadNotImplementedYet")
          arguments
      in
      let processArguments arguments =
        (* see Expression.Function above: *)
        (* Js: () => 1 has 0 param. In OCaml, it has one param: unit. *)
        match argumentsIntoReasonArguments arguments with
        | [] -> [ (Nolabel, expUnit) ]
        | oneArgOrMore -> List.map (fun arg -> (Nolabel, arg)) oneArgOrMore
      in
      match (callee, context.insideReactClass, arguments) with
      | ( Member
            {
              Member._object = _, This _;
              property = Member.PropertyIdentifier (_, { name = "setState"; _ });
              _;
            },
          Yes _,
          _ ) ->
          Exp.apply
            (Exp.ident
               (astHelperStrLidIdent ~correct:false [ "ReactRe"; "setState" ]))
            ((Nolabel, Exp.ident (astHelperStrLidIdent [ "this" ]))
            :: processArguments arguments)
      | ( Expression.Member
            {
              Expression.Member._object =
                _, Expression.Identifier (_, { name = "React"; _ });
              property =
                Expression.Member.PropertyIdentifier
                  (_, { name = "createClass"; _ });
              _;
            },
          Yes className,
          Expression (_, Object { Object.properties; _ }) :: [] ) ->
          (* the context insideReactClass is already set by the variable declarator (search declarator)
             a level above *)
          let hasDisplayNameAlready = ref false in
          let createClassSpecForCreateClassDecl =
            List.map
              (fun property ->
                let open Object in
                match property with
                | Property
                    ( _,
                      Property.Init
                        {
                          key = Property.Identifier (_, { name; _ });
                          value = (_, value) as valueWrap;
                          _;
                        } ) -> (
                    match value with
                    | Function functionWrap | ArrowFunction functionWrap ->
                        Cf.method_ (astHelperStrLidStr name) Public
                          (Cfk_concrete
                             ( Fresh,
                               Exp.poly
                                 (functionMapper ~context
                                    ~returnType:
                                      (match name = "getInitialState" with
                                      | true -> Some "state"
                                      | false -> None)
                                    functionWrap)
                                 None ))
                    | _ -> (
                        match name with
                        | "propTypes" ->
                            Cf.val_ (astHelperStrLidStr name) Immutable
                              (Cfk_concrete
                                 ( Fresh,
                                   expressionMapper
                                     ~context:
                                       {
                                         context with
                                         insideReactClass = InsidePropTypes;
                                       }
                                     valueWrap ))
                        | "mixins" ->
                            Cf.val_ (astHelperStrLidStr name) Immutable
                              (Cfk_concrete
                                 (Fresh, expressionMapper ~context valueWrap))
                        | "displayName" ->
                            hasDisplayNameAlready := true;
                            Cf.val_ (astHelperStrLidStr name) Immutable
                              (Cfk_concrete
                                 (Fresh, expressionMapper ~context valueWrap))
                        | name ->
                            Cf.val_ (astHelperStrLidStr name) Mutable
                              (Cfk_concrete
                                 (Fresh, expressionMapper ~context valueWrap))))
                | Property _ | SpreadProperty _ ->
                    Cf.val_
                      (astHelperStrLidStr "notSureWhat")
                      Immutable
                      (Cfk_concrete
                         (Fresh, Exp.ident (astHelperStrLidIdent [ "thisIs" ]))))
              properties
          in
          let createClassSpecForCreateClassDecl =
            match !hasDisplayNameAlready with
            | true -> createClassSpecForCreateClassDecl
            | false ->
                Cf.val_
                  (astHelperStrLidStr "displayName")
                  Immutable
                  (Cfk_concrete
                     ( Fresh,
                       Exp.constant
                         (Pconst_string (className, Location.none, None)) ))
                :: createClassSpecForCreateClassDecl
          in
          let createClassObj =
            Exp.object_
              ~attrs:[ astHelperStrAttr "mel" (PStr []) ]
              (Cstr.mk
                 (Pat.mk (Ppat_var (astHelperStrLidStr "this")))
                 createClassSpecForCreateClassDecl)
          in
          Exp.apply
            (Exp.ident
               (astHelperStrLidIdent ~correct:false
                  [ "ReactRe"; "createClass" ]))
            [ (Nolabel, createClassObj) ]
      | _, _, _ ->
          Exp.apply
            (expressionMapper ~context calleeWrap)
            (processArguments arguments))
  | Identifier (_, { name; _ }) -> Exp.ident (astHelperStrLidIdent [ name ])
  | This _ -> Exp.ident (astHelperStrLidIdent [ "this" ])
  | StringLiteral { value; _ } -> literalMapper (String value)
  | BooleanLiteral { value; _ } -> literalMapper (Boolean value)
  | NullLiteral _comments -> literalMapper Null
  | NumberLiteral { value; _ } -> literalMapper (Number value)
  | RegExpLiteral _ -> literalMapper RegExp
  | Member member -> memberMapper ~context member
  | Logical
      { Logical.operator; left = leftWrap; right = (_, right) as rightWrap; _ }
    -> (
      match operator with
      | Logical.Or ->
          Exp.apply
            (Exp.ident (astHelperStrLidIdent ~correct:false [ "||" ]))
            [
              (Nolabel, expressionMapper ~context leftWrap);
              (Nolabel, expressionMapper ~context rightWrap);
            ]
      | And -> (
          (* common pattern: `show && <Foo />`. Transpile to `show ? <Foo /> : Js.null` *)
          match right with
          | JSXElement _ ->
              Exp.match_
                (expressionMapper ~context leftWrap)
                [
                  {
                    pc_lhs =
                      Pat.construct (astHelperStrLidIdent [ "true" ]) None;
                    pc_guard = None;
                    pc_rhs = expressionMapper ~context rightWrap;
                  };
                  {
                    pc_lhs =
                      Pat.construct (astHelperStrLidIdent [ "false" ]) None;
                    pc_guard = None;
                    pc_rhs =
                      Exp.ident
                        (astHelperStrLidIdent ~correct:false [ "Js"; "null" ]);
                  };
                ]
          | _ ->
              Exp.apply
                (Exp.ident (astHelperStrLidIdent ~correct:false [ "&&" ]))
                [
                  (Nolabel, expressionMapper ~context leftWrap);
                  (Nolabel, expressionMapper ~context rightWrap);
                ])
      | NullishCoalesce -> placeholder "NullishCoallesce")
  | JSXElement element -> jsxElementMapper ~context element
  | Array { Array.elements; comments = _ } ->
      List.map
        (fun element ->
          match element with
          | Array.Hole _ ->
              Exp.construct
                (astHelperStrLidIdent ~correct:false [ "None" ])
                None
          | Expression e -> expressionMapper ~context e
          | Spread (_, _) -> placeholder "argumentSpreadNotImplementedYet")
        elements
      |> Exp.array
  | Binary { Binary.operator; left; right; _ } -> (
      let operatorReason =
        match operator with
        | Binary.Equal -> "="
        | NotEqual -> "!="
        | StrictEqual -> "=="
        | StrictNotEqual -> "!=="
        | LessThan -> "<"
        | LessThanEqual -> "<="
        | GreaterThan -> ">"
        | GreaterThanEqual -> ">="
        | LShift -> "lsl"
        | RShift -> "lsr"
        | RShift3 -> "RShift3NotImplemented"
        (* TODO: integer/float *)
        | Plus -> "+"
        | Minus -> "-"
        | Mult -> "*"
        | Exp -> "**"
        | Div -> "/"
        | Mod -> "mod"
        | BitOr -> "lor"
        | Xor -> "lxor"
        | BitAnd -> "land"
        | In -> "inNotImplemented"
        | Instanceof -> "instanceOfNotImplemented"
      in
      match (operator, left, right) with
      | Binary.Equal, (_, NullLiteral _), _ ->
          Exp.apply
            (Exp.ident
               (astHelperStrLidIdent ~correct:false
                  [ "Js"; "Null_undefined"; "test" ]))
            [ (Nolabel, expressionMapper ~context right) ]
      | Binary.Equal, _, (_, NullLiteral _) ->
          Exp.apply
            (Exp.ident
               (astHelperStrLidIdent ~correct:false
                  [ "Js"; "Null_undefined"; "test" ]))
            [ (Nolabel, expressionMapper ~context left) ]
      | _ ->
          Exp.apply
            (Exp.ident (astHelperStrLidIdent ~correct:false [ operatorReason ]))
            [
              (Nolabel, expressionMapper ~context left);
              (Nolabel, expressionMapper ~context right);
            ])
  | Assignment { Assignment.left = _, left; right; _ } -> (
      match left with
      | Pattern.Expression
          ( _,
            Member
              {
                Member._object = _, Identifier (_, { name = "module"; _ });
                property = Member.PropertyIdentifier (_, { name = "exports"; _ });
                _;
              } ) ->
          (* module.exports is added naturally by BuckleScript. We don't need any translation from JS. *)
          expUnit
      | _ ->
          (* TODO: this is redundant with what's above *)
          let leftReason =
            match left with
            | Pattern.Expression expr -> expressionMapper ~context expr
            | Pattern.Identifier { Pattern.Identifier.name = _, { name; _ }; _ }
              ->
                Exp.ident (astHelperStrLidIdent [ name ])
            | Pattern.Object _ | Pattern.Array _ -> expMarker
          in
          Exp.apply
            (Exp.ident (astHelperStrLidIdent ~correct:false [ "#=" ]))
            [
              (Nolabel, leftReason); (Nolabel, expressionMapper ~context right);
            ])
  | Unary { Unary.operator; argument = (_, argument) as argumentWrap; _ } -> (
      match operator with
      | Unary.Not -> (
          match argument with
          | Unary { Unary.operator = Unary.Not; argument = innerArgument; _ } ->
              (* !! is a js idiom for casting to boolean *)
              Exp.apply
                (Exp.ident
                   (astHelperStrLidIdent [ "pleaseWriteAIsTruthyFunction" ]))
                [ (Nolabel, expressionMapper ~context innerArgument) ]
          | _ ->
              Exp.apply
                (Exp.ident (astHelperStrLidIdent [ "not" ]))
                [ (Nolabel, expressionMapper ~context argumentWrap) ])
      | Unary.Minus -> (
          match argument with
          | NumberLiteral { value; _ } ->
              literalMapper ~isNegativeNumber:true (Number value)
          | _ ->
              Exp.apply
                (Exp.ident (astHelperStrLidIdent [ "~-" ]))
                [ (Nolabel, expressionMapper ~context argumentWrap) ])
      | Unary.Plus | Unary.BitNot | Unary.Typeof | Unary.Void | Unary.Delete
      | Unary.Await ->
          Exp.apply
            (Exp.ident (astHelperStrLidIdent [ "unaryPlaceholder" ]))
            [ (Nolabel, expressionMapper ~context argumentWrap) ])
  | Conditional { Conditional.test; consequent; alternate; _ } ->
      Exp.match_
        (expressionMapper ~context test)
        [
          {
            pc_lhs = Pat.construct (astHelperStrLidIdent [ "true" ]) None;
            pc_guard = None;
            pc_rhs = expressionMapper ~context consequent;
          };
          {
            pc_lhs = Pat.construct (astHelperStrLidIdent [ "false" ]) None;
            pc_guard = None;
            pc_rhs = expressionMapper ~context alternate;
          };
        ]
  | BigIntLiteral _ | TemplateLiteral _ | Sequence _ | Update _ | New _
  | Yield _ | TaggedTemplate _ | Class _ | TypeCast _ | MetaProperty _
  | Import _ | JSXFragment _ | ModuleRefLiteral _ | OptionalCall _
  | OptionalMember _ | Super _ | TSTypeCast _ ->
      Exp.ident (astHelperStrLidIdent [ "expressionPlaceholder" ])

(* top level output format are slightly different than expressions (they have to be `structure_item`s). We'd
   like to reuse the statementMapper's logic as much as possible though; so we use it, destructure to
   get what we want, then re-wrap for top level output. *)
let topStatementsMapper (statementWrap : (Loc.t, Loc.t) Flow_ast.Statement.t) =
  let { pexp_desc; _ } =
    statementMapper
      ~context:
        {
          terminalExpr = None;
          insideReactClass = Nope;
          reactClassSpecRandomProps = [];
        }
      statementWrap
  in
  match pexp_desc with
  | Pexp_let (_, valueBindings, { pexp_desc = _; _ }) ->
      (* get some propTypes, generate externals and type decls, e.g. type props = ... *)
      let extraDeclarations =
        match valueBindings with [] -> [ Str.eval expMarker ] | _ -> []
      in
      let baseDeclarations =
        match valueBindings with
        (* strip `require` calls *)
        | {
            pvb_expr =
              {
                pexp_desc =
                  Pexp_apply
                    ( { pexp_desc = Pexp_ident { txt = Lident "require"; _ }; _ },
                      _ );
                _;
              };
            _;
          }
          :: [] ->
            []
        | _ -> [ Str.value Nonrecursive valueBindings ]
      in
      extraDeclarations @ baseDeclarations
  | Pexp_constant a -> (
      match a with
      | Pconst_string ("use strict", _, _) -> []
      | _ -> [ Str.eval (Exp.constant a) ])
  | Pexp_ifthenelse (cond, consequent, alternate) ->
      [ Str.eval (Exp.ifthenelse cond consequent alternate) ]
  | Pexp_fun (_expr, _, argument, body) ->
      [
        Str.value Nonrecursive
          [ parseTreeValueBinding ~pat:argument ~expr:body ];
      ]
  | Pexp_function _ ->
      [
        Str.value Nonrecursive
          [
            {
              pvb_pat = Pat.var (astHelperStrLidStr "topPlaceholderMe");
              pvb_expr = expUnit;
              pvb_attributes = [];
              pvb_loc = default_loc.contents;
              pvb_constraint = None;
            };
          ];
      ]
  | Pexp_ident ident -> [ Str.eval (Exp.ident ident) ]
  | Pexp_apply (expr, lst) -> [ Str.eval (Exp.apply expr lst) ]
  | Pexp_construct (_, _) -> []
  | Pexp_letmodule (ident, modExpr, _) ->
      [
        Str.module_
          {
            pmb_name = ident;
            pmb_expr = modExpr;
            pmb_attributes = [];
            pmb_loc = default_loc.contents;
          };
      ]
  | Pexp_record (_, _)
  | Pexp_match (_, _)
  | Pexp_try (_, _)
  | Pexp_tuple _
  | Pexp_variant (_, _)
  | Pexp_field (_, _)
  | Pexp_setfield (_, _, _)
  | Pexp_array _
  | Pexp_sequence (_, _)
  | Pexp_while (_, _)
  | Pexp_for (_, _, _, _, _)
  | Pexp_constraint (_, _)
  | Pexp_coerce (_, _, _)
  | Pexp_send (_, _)
  | Pexp_new _
  | Pexp_setinstvar (_, _)
  | Pexp_override _ | Pexp_assert _ | Pexp_lazy _
  | Pexp_poly (_, _)
  | Pexp_object _
  | Pexp_newtype (_, _)
  | Pexp_pack _
  | Pexp_open (_, _)
  | Pexp_unreachable
  | Pexp_letexception (_, _)
  | Pexp_letop _ | Pexp_extension _ ->
      [
        Str.mk
          (Pstr_value
             ( Nonrecursive,
               [
                 {
                   pvb_pat = Pat.var (astHelperStrLidStr "topPlaceholder");
                   pvb_expr = expUnit;
                   pvb_attributes = [];
                   pvb_loc = default_loc.contents;
                   pvb_constraint = None;
                 };
               ] ));
      ]
