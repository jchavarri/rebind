open Ast_helper;

open Parsetree;

open SharedTypes;

module OrigToSafeMap = Map.Make(String);

let findWithDefault = (x, xs) =>
  try(SharedTypes.Identifiers.find(x, xs)) {
  | Not_found => x
  };

module SafeToOrigMap = Map.Make(String);

let externalTypeToString = t =>
  switch (t) {
  | String(_) => "string"
  | Float => "float"
  | Abstract(a) => a
  | Unit => "unit"
  | Fun(_, _) =>
    failwith(
      "Fun type should be resolved before calling externalTypeToString",
    )
  | Module(n) => n
  | ModuleProperty(_name, local, _remote) => local
  | Named(_, _) =>
    failwith(
      "Named type should be resolved before calling externalTypeToString",
    )
  };

let typeConstr = (externalType, safeIds) => {
  ptyp_desc:
    Ptyp_constr(
      AstUtils.astHelperStrLidIdent([
        findWithDefault(externalTypeToString(externalType), safeIds),
      ]),
      [],
    ),
  ptyp_loc: default_loc.contents,
  ptyp_attributes: [],
  ptyp_loc_stack: [],
};

let typeArrow = (a, b, label) => {
  ptyp_desc: Ptyp_arrow(label, a, b),
  ptyp_loc: default_loc.contents,
  ptyp_attributes: [],
  ptyp_loc_stack: [],
};

let typeFromExternalTypes = (typesList, safeIds) => {
  let rec rootType = list => {
    let rec process = (l, acc) =>
      switch (l, acc) {
      | ([], Some(t)) => t
      | ([Fun(name, l), ...xl], Some(a)) =>
        let funTypes = l @ [Abstract(findWithDefault(name, safeIds))];
        process(xl, Some(typeArrow(rootType(funTypes), a, Nolabel)));
      | ([Named(name, type_), ...xl], Some(a)) =>
        process(
          xl,
          Some(
            typeArrow(
              rootType([type_]),
              a,
              Labelled(findWithDefault(name, safeIds)),
            ),
          ),
        )
      | (
          [a, ...xl],
          Some({
            ptyp_loc,
            ptyp_attributes,
            ptyp_desc: Ptyp_constr(bLoc, bList),
            ptyp_loc_stack: _,
          }),
        ) =>
        process(
          xl,
          Some(
            typeArrow(
              typeConstr(a, safeIds),
              {
                ptyp_loc,
                ptyp_attributes,
                ptyp_desc: Ptyp_constr(bLoc, bList),
                ptyp_loc_stack: [],
              },
              Nolabel,
            ),
          ),
        )
      | ([a, ...xl], None) => process(xl, Some(typeConstr(a, safeIds)))
      | (
          [a, ...xl],
          Some({
            ptyp_loc,
            ptyp_attributes,
            ptyp_desc: Ptyp_arrow(bLabel, bType1, bType2),
            ptyp_loc_stack: _,
          }),
        ) =>
        process(
          xl,
          Some(
            typeArrow(
              typeConstr(a, safeIds),
              {
                ptyp_loc,
                ptyp_attributes,
                ptyp_desc: Ptyp_arrow(bLabel, bType1, bType2),
                ptyp_loc_stack: [],
              },
              Nolabel,
            ),
          ),
        )
      | ([], None) => failwith("empty list passed to descFromTypes")
      | (_, _) => failwith("impossible")
      };
    process(List.rev(list), None);
  };
  rootType(typesList);
};

let externalWithAttribute = (originalName, types, safeIds, attr) => {
  let newName = OrigToSafeMap.find(originalName, safeIds);
  let (outputAttrs, valueDescription) =
    switch (attr) {
    | Module => ([("mel.module", None)], originalName)
    | Send => ([("mel.send", None)], originalName)
    | Get => ([("mel.get", None)], originalName)
    | ObjectCreation => ([("mel.obj", None)], "")
    | Val => ([], originalName)
    | NewAttr => ([("mel.new", None)], originalName)
    | ModuleAndNew => (
        [("mel.new", None), ("mel.module", None)],
        originalName,
      )
    | ScopedModule(name, scopeProperty) => (
        [("mel.module", Some(name))],
        scopeProperty,
      )
    };
  let attrs: Parsetree.attributes =
    outputAttrs
    |> List.map(((outputAttr, constant)) => {
         let constExpr: list(Parsetree.structure_item) =
           switch (constant) {
           | Some(c) => [
               {
                 pstr_desc:
                   Pstr_eval(
                     {
                       pexp_desc:
                         Pexp_constant(
                           Pconst_string(c, Location.none, None),
                         ),
                       pexp_loc: default_loc.contents,
                       pexp_attributes: [],
                       pexp_loc_stack: [],
                     },
                     [],
                   ),
                 pstr_loc: default_loc.contents,
               },
             ]
           | None => []
           };
         {
           attr_loc: default_loc.contents,
           attr_name: {
             txt: outputAttr,
             loc: Location.none,
           },
           attr_payload: PStr(constExpr),
         };
       });
  let t: Parsetree.value_description = {
    pval_name: {
      loc: default_loc.contents,
      txt: newName,
    },
    pval_prim: [valueDescription],
    pval_loc: default_loc.contents,
    pval_type: typeFromExternalTypes(types, safeIds),
    pval_attributes: attrs,
  };
  Str.primitive(~loc=Location.none, t);
};

let rec generateSafeId = (~postFix=None, name, safeToOrig) => {
  let strPostFix =
    switch (postFix) {
    | Some(n) => string_of_int(n)
    | None => ""
    };
  let safeName = AstUtils.correctIdentifier(name) ++ strPostFix;
  switch (postFix, Utils.tryFindId(safeName, safeToOrig)) {
  | (Some(n), Some(_safeName)) =>
    generateSafeId(~postFix=Some(n + 1), name, safeToOrig)
  | (None, Some(_existingId)) =>
    generateSafeId(~postFix=Some(2), name, safeToOrig)
  | (_, None) => safeName
  };
};

let safeIdentifiers = identifiers => {
  let (origToSafe, _safeToOrig) =
    Identifiers.fold(
      (name, _externalType, (origToSafe, safeToOrig)) =>
        switch (Utils.tryFindId(name, origToSafe)) {
        | Some(_existingId) => (origToSafe, safeToOrig)
        | None =>
          let safeId = generateSafeId(name, safeToOrig);
          (
            OrigToSafeMap.add(name, safeId, origToSafe),
            SafeToOrigMap.add(safeId, name, safeToOrig),
          );
        },
      identifiers,
      (OrigToSafeMap.empty, SafeToOrigMap.empty),
    );
  origToSafe;
};

let transform = state => {
  let {identifiers, outputTypes, outputExternals} = state;
  let safeIds = safeIdentifiers(identifiers);
  (
    List.rev(outputTypes)
    |> List.map(outputType =>
         Str.type_(
           ~loc=Location.none,
           Recursive,
           [
             Type.mk(
               ~loc=Location.none,
               ~kind=Ptype_abstract,
               ~priv=Public,
               {
                 loc: default_loc.contents,
                 txt: OrigToSafeMap.find(outputType, safeIds),
               },
             ),
           ],
         )
       )
  )
  @ (
    List.rev(outputExternals)
    |> List.map(structureItem =>
         externalWithAttribute(
           structureItem.name,
           structureItem.types,
           safeIds,
           structureItem.attr,
         )
       )
  );
};
