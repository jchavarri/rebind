type externalBaseType =
  | String(string)
  | Float
  | Abstract(string)
  | Unit
  | Named(string, externalBaseType)
  | Fun(string, list(externalBaseType));

type externalStatementAttr =
  | Module
  | Get
  | Send
  | ObjectCreation;

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