open Ast_helper;

open Parsetree;

open SharedTypes;

module OrigToSafeMap = Map.Make(String);
let findWithDefault = (x, xs) =>
  try (SharedTypes.Identifiers.find(x, xs)) {
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
      "Function type should be resolved before calling externalTypeToString",
    )
  | Named(_, _) =>
    failwith(
      "Named type should be resolved before calling externalTypeToString",
    )
  };

let typeConstr = (externalType, safeIds) => {
  ptyp_desc:
    Ptyp_constr(
      AstUtils.astHelperStrLidIdent(
        ~correct=true,
        [findWithDefault(externalTypeToString(externalType), safeIds)],
      ),
      [],
    ),
  ptyp_loc: default_loc.contents,
  ptyp_attributes: [],
};

let typeArrow = (a, b, label) => {
  ptyp_desc: Ptyp_arrow(label, a, b),
  ptyp_loc: default_loc.contents,
  ptyp_attributes: [],
};

let typeFromExternalTypes = (typesList, safeIds) => {
  let rec rootType = list => {
    let rec process = (l, acc) =>
      switch (l, acc) {
      | ([], Some(t)) => t
      | ([Fun(name, l), ...xl], Some(a)) =>
        let funTypes = l @ [Abstract(findWithDefault(name, safeIds))];
        process(xl, Some(typeArrow(rootType(funTypes), a, "")));
      | ([Named(name, type_), ...xl], Some(a)) =>
        process(xl, Some(typeArrow(rootType([type_]), a, findWithDefault(name, safeIds))))
      | (
          [a, ...xl],
          Some({
            ptyp_loc,
            ptyp_attributes,
            ptyp_desc: Ptyp_constr(bLoc, bList),
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
              },
              "",
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
              },
              "",
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

let externalWithAttribute = (name, types, safeIds, attr) =>
  Str.primitive({
    pval_name: {
      loc: default_loc.contents,
      txt: OrigToSafeMap.find(name, safeIds),
    },
    pval_prim: [""],
    pval_loc: default_loc.contents,
    pval_type: typeFromExternalTypes(types, safeIds),
    pval_attributes: [({loc: default_loc.contents, txt: attr}, PStr([]))],
  });

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
    |> List.map(structureItem =>
         switch (structureItem) {
         | name =>
           Str.type_([
             Type.mk(
               ~kind=Ptype_abstract,
               ~priv=Public,
               {
                 loc: default_loc.contents,
                 txt: OrigToSafeMap.find(name, safeIds),
               },
             ),
           ])
         }
       )
  )
  @ (
    List.rev(outputExternals)
    |> List.map(structureItem => {
         let externalOfType =
           externalWithAttribute(
             structureItem.name,
             structureItem.types,
             safeIds,
           );
         switch (structureItem.attr) {
         | Module => externalOfType("bs.module")
         | Send => externalOfType("bs.send")
         | Get => externalOfType("bs.get")
         | ObjectCreation => externalOfType("bs.obj")
         };
       })
  );
};