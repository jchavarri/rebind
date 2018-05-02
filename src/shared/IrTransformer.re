open Ast_helper;

open Parsetree;

open SharedTypes;

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

let typeConstr = name => {
  ptyp_desc:
    Ptyp_constr(
      AstUtils.astHelperStrLidIdent(
        ~correct=true,
        [externalTypeToString(name)],
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

let rec rootType = list => {
  let rec process = (l, acc) =>
    switch (l, acc) {
    | ([], Some(t)) => t
    | ([Fun(name, l), ...xl], Some(a)) =>
      let funTypes = l @ [Abstract(name)];
      process(xl, Some(typeArrow(rootType(funTypes), a, "")));
    | ([Named(name, type_), ...xl], Some(a)) =>
      process(xl, Some(typeArrow(rootType([type_]), a, name)))
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
            typeConstr(a),
            {ptyp_loc, ptyp_attributes, ptyp_desc: Ptyp_constr(bLoc, bList)},
            "",
          ),
        ),
      )
    | ([a, ...xl], None) => process(xl, Some(typeConstr(a)))
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
            typeConstr(a),
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

let externalWithAttribute = (attr, name, types) =>
  Str.primitive({
    pval_name: {
      loc: default_loc.contents,
      txt: AstUtils.correctIdentifier(name),
    },
    pval_prim: [""],
    pval_loc: default_loc.contents,
    pval_type: rootType(types),
    pval_attributes: [({loc: default_loc.contents, txt: attr}, PStr([]))],
  });

let transform = ({outputTypes, outputExternals}) =>
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
                 txt: AstUtils.correctIdentifier(name),
               },
             ),
           ])
         }
       )
  )
  @ (
    List.rev(outputExternals)
    |> List.map(structureItem =>
         switch (structureItem.attr) {
         | Module =>
           externalWithAttribute(
             "bs.module",
             structureItem.name,
             structureItem.types,
           )
         | Send =>
           externalWithAttribute(
             "bs.send",
             structureItem.name,
             structureItem.types,
           )
         | Get =>
           externalWithAttribute(
             "bs.get",
             structureItem.name,
             structureItem.types,
           )
         | ObjectCreation =>
           externalWithAttribute(
             "bs.obj",
             structureItem.name,
             structureItem.types,
           )
         }
       )
  );