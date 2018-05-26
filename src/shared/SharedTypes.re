type externalBaseType =
  | String(string)
  | Float
  | Abstract(string)
  | Unit
  | Named(string, externalBaseType)
  | Fun(string, list(externalBaseType))
  | Module(string); /* Just a helper to identify somehow expressions like require('something') */

type externalStatementAttr =
  | Module
  | Get
  | Send
  | ObjectCreation
  | Val
  | NewAttr; /* Adding a suffix to `New` to avoid conflicts with Flow's SpiderMonkey types */

type externalStatement = {
  attr: externalStatementAttr,
  name: string,
  types: list(externalBaseType),
};

module Identifiers = Map.Make(String);

type state = {
  identifiers: Identifiers.t(externalBaseType),
  rightSideTypes: list(externalBaseType), /* The carried types in an expression */
  outputTypes: list(string),
  outputExternals: list(externalStatement),
  parentContextName: string,
};

module type Statement = {
  let mapper: (state, Parser_flow.Ast.Statement.t) => state;
};

module type T = {let mapper: (state, Parser_flow.Ast.Statement.t) => state;};