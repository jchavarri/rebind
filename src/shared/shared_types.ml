type external_base_type =
  | String of string
  | Float
  | Abstract of string
  | Unit
  | Named of string * external_base_type
  | Fun of string * external_base_type list
  | Module of string
    (* Just a helper to identify somehow expressions like require('something') *)
  | ModuleProperty of
      string * string * string (* (moduleName, localName, remoteName) *)

type external_statement_attr =
  | Module
  | Get
  | Send
  | ObjectCreation
  | Val
  | NewAttr
    (* Adding a suffix to `New` to avoid conflicts with Flow's SpiderMonkey types *)
  | ModuleAndNew
  | ScopedModule of string * string (* (moduleName, scopedProperty) *)

type external_statement = {
  attr : external_statement_attr;
  name : string;
  types : external_base_type list;
}

module Identifiers = Map.Make (String)

type state = {
  identifiers : external_base_type Identifiers.t;
  right_side_types : external_base_type list;
      (* The carried types in an expression *)
  outputTypes : string list;
  outputExternals : external_statement list;
  parentContextName : string;
}

module type Statement = sig
  val mapper : state -> Parser_flow.Ast.Statement.t -> state
end

module type T = sig
  val mapper : state -> Parser_flow.Ast.Statement.t -> state
end
