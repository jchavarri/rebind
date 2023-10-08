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

module Js_identifiers = Map.Make (String)

type typ = Abstract of { js_name : string; ml_name : string }
type known_ids = typ Js_identifiers.t

type state = {
  (* the ML expression equivalent to the current JS expression *)
  expr : Parsetree.expression;
  (* a map from JS ids to inferred types *)
  known_ids : known_ids;
}

(* helpers *)
(* TODO: turn foo_bar into foo_bar_ *)
let correct_identifier ident =
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
    txt = (match correct with true -> correct_identifier name | false -> name);
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
          | true -> correct_identifier (List.hd a)
          | false -> List.hd a)
      in
      let res =
        List.tl a
        |> List.fold_left
             (fun acc curr ->
               Ldot
                 ( acc,
                   match correct with
                   | true -> correct_identifier curr
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

type context = { terminalExpr : Parsetree.expression option }

let rec statementBlockMapper ~context:_ ~known_ids
    ({ Parser_flow.Ast.Statement.Block.body; _ } :
      (_, _) Parser_flow.Ast.Statement.Block.t) : state =
  match body with
  | [] -> { known_ids; expr = expUnit }
  | bodyNotEmpty ->
      let bodyNotEmptyFlipped = List.rev bodyNotEmpty in
      let lastItemReason =
        List.hd bodyNotEmptyFlipped
        |> statementMapper ~context:{ terminalExpr = None } ~known_ids
      in
      List.tl bodyNotEmptyFlipped
      |> List.fold_left
           (fun accumExp statement ->
             statementMapper
               ~context:{ terminalExpr = Some accumExp.expr }
               ~known_ids statement)
           lastItemReason

and functionMapper ~context ~known_ids ~returnType
    ({ Parser_flow.Ast.Function.params = _, params; body; _ } :
      (Loc.t, Loc.t) Parser_flow.Ast.Function.t) =
  let open Parser_flow.Ast in
  (* functionMapper is currently the only one using context.addPropsAndStateDeclImmediately. It wraps around the
     function in props and state declaration. don't forget to unset `context.addPropsAndStateDeclImmediately`; *)
  let bodyReason =
    match body with
    | Function.BodyExpression expression ->
        expression_mapper ~context ~known_ids expression
    | Function.BodyBlock (_, body) ->
        statementBlockMapper ~context ~known_ids body
  in
  let wrapBodyInReturnType body =
    match returnType with
    | None -> body
    | Some typeName ->
        Exp.constraint_ body (Typ.constr (astHelperStrLidIdent [ typeName ]) [])
  in
  let expr =
    match params.params with
    | [] ->
        Exp.fun_ Nolabel None
          (Pat.construct (astHelperStrLidIdent ~correct:false [ "()" ]) None)
          (wrapBodyInReturnType bodyReason.expr)
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
               bodyReason.expr
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
  in
  { known_ids; expr }

and literalMapper ?(isNegativeNumber = false) literal =
  match literal with
  | String s -> Exp.constant (Pconst_string (s, Location.none, None))
  | Boolean boolean ->
      Exp.ident
        (astHelperStrLidIdent ~correct:false
           [ "Js"; (match boolean with true -> "true_" | false -> "false_") ])
  | Null -> Exp.ident (astHelperStrLidIdent ~correct:false [ "Js"; "null" ])
  | Number n -> (
      let intN = int_of_float n in
      let is_int = float_of_int intN = n in
      match is_int with
      | true ->
          let intN = int_of_float n in
          Exp.constant
            (Pconst_integer
               ( string_of_int
                   (match isNegativeNumber with true -> -intN | false -> intN),
                 None ))
      | false ->
          Exp.constant
            (Pconst_float
               ( string_of_float
                   (match isNegativeNumber with true -> -.n | false -> n),
                 None )))
  | RegExp -> placeholder "regexPlaceholder"

and jsxElementMapper ~context ~known_ids
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
      let known_ids, childrenReact =
        let known_ids, children =
          List.fold_left
            (fun ((known_ids, children) as acc) child ->
              match jsxChildMapper ~context ~known_ids child with
              | None -> acc
              | Some state -> (state.known_ids, state.expr :: children))
            (known_ids, []) children
        in
        (known_ids, List.rev children)
      in
      let constructRecordOrLabels ~known_ids f =
        List.fold_left
          (fun (known_ids, args) attr ->
            match attr with
            | Opening.Attribute (_, { Attribute.name; value }) -> (
                (* JSX's <Foo checked /> is sugar for <Foo checked={true} />. What a waste *)
                let known_ids, ml_value =
                  match value with
                  | None ->
                      ( known_ids,
                        Exp.ident
                          (astHelperStrLidIdent ~correct:false [ "Js"; "true_" ])
                      )
                  | Some (Attribute.StringLiteral (_, lit)) ->
                      (known_ids, literalMapper (String lit.value))
                  | Some
                      (Attribute.ExpressionContainer
                        (_, { ExpressionContainer.expression; _ })) -> (
                      match expression with
                      | ExpressionContainer.Expression expr ->
                          let state =
                            expression_mapper ~context ~known_ids expr
                          in
                          (state.known_ids, state.expr)
                      | ExpressionContainer.EmptyExpression ->
                          (known_ids, expUnit))
                in
                match name with
                | Attribute.Identifier (_, { Identifier.name; _ }) ->
                    (known_ids, (f name, ml_value) :: args)
                | Attribute.NamespacedName _ ->
                    ( known_ids,
                      (f "NamespacedName", placeholder "notImplementedYet")
                      :: args ))
            | Opening.SpreadAttribute _ ->
                ( known_ids,
                  (f "spreadAttribute", placeholder "notImplementedYet") :: args
                ))
          (known_ids, []) attributes
      in
      if jsxPropHasHyphen then
        (* if there's a hyphen (e.g. aria-label) then we can't transform it into a jsx function label (invalid syntax). We'll
           have to take a shortcut and use a bs.obj instead *)
        let known_ids, jsObj =
          let known_ids, fields =
            constructRecordOrLabels ~known_ids (fun name ->
                astHelperStrLidIdent [ name ])
          in
          ( known_ids,
            Exp.extension
              ( astHelperStrLidStr "mel.obj",
                PStr [ Str.eval (Exp.record fields None) ] ) )
        in
        {
          known_ids;
          expr =
            Exp.apply
              (Exp.ident
                 (astHelperStrLidIdent ~correct:false
                    [ "ReactRe"; "createElement" ]))
              [
                ( Nolabel,
                  Exp.ident (astHelperStrLidIdent ~correct:false [ name ]) );
                (Nolabel, jsObj);
                (Nolabel, Exp.array childrenReact);
              ];
        }
      else
        let known_ids, partial_arguments =
          constructRecordOrLabels ~known_ids (fun name ->
              Labelled (correct_identifier name))
        in
        (* add children *)
        let arguments =
          partial_arguments @ [ (Nolabel, listToListAst childrenReact) ]
        in
        {
          known_ids;
          expr =
            Exp.apply
              ~attrs:[ astHelperStrAttr ~correct:false "JSX" (PStr []) ]
              (Exp.ident (astHelperStrLidIdent ~correct:false [ name ]))
              arguments;
        }
  | MemberExpression (_, { MemberExpression._object; _ }) ->
      { known_ids; expr = placeholder "complexJSXYet" }
  | NamespacedName _ -> { known_ids; expr = placeholder "noNameSpaceJSXYet" }

and jsxChildMapper ~context ~known_ids
    ((_, child) : (Loc.t, Loc.t) Parser_flow.Ast.JSX.child) : state option =
  let open Parser_flow.Ast.JSX in
  match child with
  | Parser_flow.Ast.JSX.Element element ->
      Some (jsxElementMapper ~context ~known_ids element)
  | ExpressionContainer { ExpressionContainer.expression; _ } -> (
      match expression with
      | ExpressionContainer.Expression expr ->
          Some (expression_mapper ~known_ids ~context expr)
      | ExpressionContainer.EmptyExpression ->
          Some { known_ids; expr = expUnit })
  | Text { Text.value; _ } ->
      (* JS jsx is whitespace sensitive and scatters around "\n          " in the AST *)
      let trimmedValue = String.trim value in
      if trimmedValue = "" then None
      else
        Some
          {
            known_ids;
            expr =
              Exp.constant (Pconst_string (trimmedValue, Location.none, None));
          }
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
      let props =
        List.map
          (fun property ->
            match property with
            | Property (_, Property.Init { key; value; _ }) ->
                let keyReason =
                  match key with
                  | Property.StringLiteral (_, { value = name; _ }) -> [ name ]
                  | NumberLiteral (_, { value; _ }) -> [ string_of_float value ]
                  | BigIntLiteral (_, { value; _ }) -> (
                      match value with
                      | Some v -> [ Int64.to_string v ]
                      | None -> [ "int64IsNone" ])
                  | PrivateName (_, { name; _ }) | Identifier (_, { name; _ })
                    ->
                      [ name ]
                  | Computed _ -> [ "notThereYet" ]
                in
                ( astHelperStrLidIdent keyReason,
                  expression_mapper ~context ~known_ids value )
            | Property (_, (Method _ | Get _ | Set _)) ->
                ( astHelperStrLidIdent
                    [ "objectPropertyMethodGetSetNotImplementedYet" ],
                  placeholder "objectPropertyMethodGetSetNotImplementedYet" )
            | SpreadProperty _ ->
                ( astHelperStrLidIdent [ "objectSpreadNotImplementedYet" ],
                  placeholder "objectSpreadNotImplementedYet" ))
          properties
      in
      Exp.extension
        (astHelperStrLidStr "mel.obj", PStr [ Str.eval (Exp.record props None) ])

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
    let left = expression_mapper ~context objectWrap in
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
        else expression_mapper ~context expr
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
              [ (Nolabel, expression_mapper ~context exprWrap) ]
        | _ ->
            (* if computed then
                      (* foo.[bar] => foo.(bar); treat as array *)
               Exp.apply
                        (Exp.ident
                           (astHelperStrLidIdent ~correct:false [ "Array"; "get" ]))
                        [
                          (Nolabel, left); (Nolabel, expression_mapper ~context exprWrap);
                        ]
                    else *)
            (* foo.bar => foo##bar; *)
            Exp.apply
              (Exp.ident (astHelperStrLidIdent ~correct:false [ "##" ]))
              [
                (Nolabel, left); (Nolabel, expression_mapper ~context exprWrap);
              ])
    | PropertyPrivateName _ ->
        placeholder "memberMapperPropertyPrivateNameNotImplementedYet"
  in
  defaultCase ()

and statementMapper ~context ~known_ids statement : state =
  let (_, statement) : (Loc.t, Loc.t) Flow_ast.Statement.t = statement in
  let open Parser_flow.Ast.Statement in
  let open Parser_flow.Ast in
  match statement with
  | VariableDeclaration { VariableDeclaration.declarations; _ } ->
      (* this is the part that transforms non-top-level var declarations list (in a single var
         declaration) from js to let binding with a tuple in OCaml *)
      (* TODO: actually do this  *)
      let _, { Statement.VariableDeclaration.Declarator.id = _, id; init } =
        List.hd declarations
      in
      let expr =
        match init with
        | None ->
            Exp.construct (astHelperStrLidIdent ~correct:false [ "None" ]) None
        | Some e -> expression_mapper ~context e
      in
      let innerMostExpr =
        match context.terminalExpr with None -> expUnit | Some expr -> expr
      in
      let expr =
        match id with
        | Pattern.Identifier { Pattern.Identifier.name = _, { name; _ }; _ } ->
            Exp.let_ Nonrecursive
              (* every react component must be called comp so that we could do Foo.comp on it for JSX *)
              [
                parseTreeValueBinding
                  ~pat:(Pat.var (astHelperStrLidStr name))
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
                              ( "destructuringNotImplemented",
                                Location.none,
                                None )))
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
              innerMostExpr
      in
      { known_ids; expr }
  | Return { Return.argument; _ } ->
      let result =
        match argument with
        | None -> expUnit
        | Some expr -> expression_mapper ~context expr
      in
      let expr =
        match context.terminalExpr with
        | None -> result
        | Some expr -> Exp.sequence result expr
      in
      { known_ids; expr }
  | Expression { Statement.Expression.expression; _ } ->
      let expr =
        match context.terminalExpr with
        | None -> expression_mapper ~context expression
        | Some expr -> Exp.sequence (expression_mapper ~context expression) expr
      in
      { known_ids; expr }
  | If { test; consequent; alternate; _ } ->
      let expr =
        let result =
          let then_ =
            statementMapper ~context:{ terminalExpr = None } ~known_ids
              consequent
          in
          let else_ =
            match alternate with
            | None -> None
            | Some (_, statement) ->
                let t =
                  statementMapper ~context:{ terminalExpr = None } ~known_ids
                    statement.body
                in
                Some t.expr
          in
          Exp.ifthenelse (expression_mapper ~context test) then_.expr else_
        in
        match context.terminalExpr with
        | None -> result
        | Some expr -> Exp.sequence result expr
      in
      { known_ids; expr }
  | Block body -> statementBlockMapper ~context ~known_ids body
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
          expression_mapper ~context expr)
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

and expression_mapper ~context ~known_ids
    ((_, expression) : (Loc.t, Loc.t) Parser_flow.Ast.Expression.t) : state =
  let open Parser_flow.Ast in
  let open Parser_flow.Ast.Expression in
  let expr =
    match expression with
    | Object obj -> objectMapper ~context obj
    | ArrowFunction functionWrap | Function functionWrap ->
        functionMapper ~context ~known_ids ~returnType:None functionWrap
    | Call
        {
          Call.callee = (_, callee) as calleeWrap;
          arguments = _, { arguments; _ };
          _;
        } -> (
        let to_ocaml_args arguments =
          List.map
            (fun argument ->
              match argument with
              | Expression e -> expression_mapper ~context ~known_ids e
              | Spread (_, _) -> placeholder "argumentSpreadNotImplementedYet")
            arguments
        in
        let process_args arguments =
          (* see Expression.Function above: *)
          (* Js: () => 1 has 0 param. In OCaml, it has one param: unit. *)
          match to_ocaml_args arguments with
          | [] -> [ (Nolabel, expUnit) ]
          | oneArgOrMore -> List.map (fun arg -> (Nolabel, arg)) oneArgOrMore
        in
        match (callee, arguments) with
        | ( Identifier (_, { name = "require"; comments = _ }),
            [ Expression (_, StringLiteral { value; _ }) ] ) ->
            Exp.ident (astHelperStrLidIdent [ correct_identifier value ])
        | _, _ ->
            Exp.apply
              (expression_mapper ~context calleeWrap)
              (process_args arguments))
    | Identifier (_, { name; _ }) -> Exp.ident (astHelperStrLidIdent [ name ])
    | This _ -> Exp.ident (astHelperStrLidIdent [ "this" ])
    | StringLiteral { value; _ } -> literalMapper (String value)
    | BooleanLiteral { value; _ } -> literalMapper (Boolean value)
    | NullLiteral _comments -> literalMapper Null
    | NumberLiteral { value; _ } -> literalMapper (Number value)
    | RegExpLiteral _ -> literalMapper RegExp
    | Member member -> memberMapper ~context member
    | Logical
        {
          Logical.operator;
          left = leftWrap;
          right = (_, right) as rightWrap;
          _;
        } -> (
        match operator with
        | Logical.Or ->
            Exp.apply
              (Exp.ident (astHelperStrLidIdent ~correct:false [ "||" ]))
              [
                (Nolabel, expression_mapper ~context leftWrap);
                (Nolabel, expression_mapper ~context rightWrap);
              ]
        | And -> (
            (* common pattern: `show && <Foo />`. Transpile to `show ? <Foo /> : Js.null` *)
            match right with
            | JSXElement _ ->
                Exp.match_
                  (expression_mapper ~context leftWrap)
                  [
                    {
                      pc_lhs =
                        Pat.construct (astHelperStrLidIdent [ "true" ]) None;
                      pc_guard = None;
                      pc_rhs = expression_mapper ~context rightWrap;
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
                    (Nolabel, expression_mapper ~context leftWrap);
                    (Nolabel, expression_mapper ~context rightWrap);
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
            | Expression e -> expression_mapper ~context e
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
              [ (Nolabel, expression_mapper ~context right) ]
        | Binary.Equal, _, (_, NullLiteral _) ->
            Exp.apply
              (Exp.ident
                 (astHelperStrLidIdent ~correct:false
                    [ "Js"; "Null_undefined"; "test" ]))
              [ (Nolabel, expression_mapper ~context left) ]
        | _ ->
            Exp.apply
              (Exp.ident
                 (astHelperStrLidIdent ~correct:false [ operatorReason ]))
              [
                (Nolabel, expression_mapper ~context left);
                (Nolabel, expression_mapper ~context right);
              ])
    | Assignment { Assignment.left = _, left; right; _ } -> (
        match left with
        | Pattern.Expression
            ( _,
              Member
                {
                  Member._object = _, Identifier (_, { name = "module"; _ });
                  property =
                    Member.PropertyIdentifier (_, { name = "exports"; _ });
                  _;
                } ) ->
            (* module.exports is added naturally by BuckleScript. We don't need any translation from JS. *)
            expUnit
        | _ ->
            (* TODO: this is redundant with what's above *)
            let leftReason =
              match left with
              | Pattern.Expression expr -> expression_mapper ~context expr
              | Pattern.Identifier
                  { Pattern.Identifier.name = _, { name; _ }; _ } ->
                  Exp.ident (astHelperStrLidIdent [ name ])
              | Pattern.Object _ | Pattern.Array _ -> expMarker
            in
            Exp.apply
              (Exp.ident (astHelperStrLidIdent ~correct:false [ "#=" ]))
              [
                (Nolabel, leftReason);
                (Nolabel, expression_mapper ~context right);
              ])
    | Unary { Unary.operator; argument = (_, argument) as argumentWrap; _ } -> (
        match operator with
        | Unary.Not -> (
            match argument with
            | Unary { Unary.operator = Unary.Not; argument = innerArgument; _ }
              ->
                (* !! is a js idiom for casting to boolean *)
                Exp.apply
                  (Exp.ident
                     (astHelperStrLidIdent [ "pleaseWriteAIsTruthyFunction" ]))
                  [ (Nolabel, expression_mapper ~context innerArgument) ]
            | _ ->
                Exp.apply
                  (Exp.ident (astHelperStrLidIdent [ "not" ]))
                  [ (Nolabel, expression_mapper ~context argumentWrap) ])
        | Unary.Minus -> (
            match argument with
            | NumberLiteral { value; _ } ->
                literalMapper ~isNegativeNumber:true (Number value)
            | _ ->
                Exp.apply
                  (Exp.ident (astHelperStrLidIdent [ "~-" ]))
                  [ (Nolabel, expression_mapper ~context argumentWrap) ])
        | Unary.Plus | Unary.BitNot | Unary.Typeof | Unary.Void | Unary.Delete
        | Unary.Await ->
            Exp.apply
              (Exp.ident (astHelperStrLidIdent [ "unaryPlaceholder" ]))
              [ (Nolabel, expression_mapper ~context argumentWrap) ])
    | Conditional { Conditional.test; consequent; alternate; _ } ->
        Exp.match_
          (expression_mapper ~context test)
          [
            {
              pc_lhs = Pat.construct (astHelperStrLidIdent [ "true" ]) None;
              pc_guard = None;
              pc_rhs = expression_mapper ~context consequent;
            };
            {
              pc_lhs = Pat.construct (astHelperStrLidIdent [ "false" ]) None;
              pc_guard = None;
              pc_rhs = expression_mapper ~context alternate;
            };
          ]
    | BigIntLiteral _ | TemplateLiteral _ | Sequence _ | Update _ | New _
    | Yield _ | TaggedTemplate _ | Class _ | TypeCast _ | MetaProperty _
    | Import _ | JSXFragment _ | ModuleRefLiteral _ | OptionalCall _
    | OptionalMember _ | Super _ | TSTypeCast _ ->
        Exp.ident (astHelperStrLidIdent [ "expressionPlaceholder" ])
  in
  { known_ids; expr }

(* top level output format are slightly different than expressions (they have to be `structure_item`s). We'd
   like to reuse the statementMapper's logic as much as possible though; so we use it, destructure to
   get what we want, then re-wrap for top level output. *)
let topStatementsMapper (statementWrap : (Loc.t, Loc.t) Flow_ast.Statement.t) =
  let { pexp_desc; _ } =
    statementMapper ~context:{ terminalExpr = None } statementWrap
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
