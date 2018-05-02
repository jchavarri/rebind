# :on: Rebind

Experimental automated generation of Reason/BuckleScript bindings from JavaScript code.

## Status

Rebind is in very early stages. While a few of the most common BuckleScript attributes have some minimal support (`[@bs.obj]`, `[@bs.send]`, `[@bs.module]`, ...), the range of JavaScript expressions that are not covered is still quite large (see table below).

## Why

This JavaScript code:

```javascript
const nano = require('nano')('http://localhost:5984');
nano.db.create('foo', (err, body, headers) => {
  console.log(err);
});
```

Requires roughly the following bindings in Reason to be generated:

```reason
type nano;
type err;
type body;
type headers;
type db;
type create;
[@bs.module] external nano : string => nano = "";
[@bs.get] external db : nano => db = "";
[@bs.send]
external create : (db, string, (err, body, headers) => unit) => create = "";
```

The process of creating bindings is a core part of introducing Reason / BuckleScript into existing JavaScript applications. However, this binding creation is a non-trivial process, and it might become a time consuming activity over time. Plus, it requires remembering the different BuckleScript attributes for each case, which, at least in my experience, has been challenging.

Rebind is an attempt to help easing this process by parsing JavaScript code, and generating as many bindings as possible based on the available syntactic information.

Over time, Rebind might become a way of coding "best practices" when it comes to write bindings, depending on the community consensus for each particular case.

TODO - Add gif with vscode

## Install

From the terminal:

`npm install https://github.com/jchavarri/rebind.git -g`

Rebind needs `refmt` to output the Reason/OCaml code, so you will have to follow the instructions from the [official Reason docs](https://reasonml.github.io/docs/en/global-installation.html) to get it installed.

## Running

From the terminal:

`rebind yourFile.js | refmt --parse binary --print re`

Rebind also supports passing some input JavaScript in stdin, like so:

`echo "var t = {a:2}" | rebind`

---

## How does it work

Rebind relies on abstract types to be able to fill the unknowns. It has no access to any type 

TODO

## Expressions supported

TODO

## Frequently asked questions

#### Does this support JSX / other fancy syntax features?

Probably not. See the table above.

#### Wait, I can use this with VSCode?

TODO - (talk Darin)

#### What about other editors?

TODO

#### Can I use it from Node / the browser?

Not yet, but it should be possible. It "just" needs to be compiled with js_of_ocaml, like refmt does, as both tools need to share the binary representation of OCaml / Reason syntax tree.

#### How can I contribute?

TODO - Move to CONTRIBUTING.md
In many ways:
- Report bugs
- Provide examples of JavaScript code + bindings that you'd like to see generated
- Pick expressions from the table above and implement

## Acknowledgements

Rebind is more than influenced by Jeason. It was originally a fork of that project, but at some point it became more and more obvious that the original vision of Jeason is much more ambitious, and those differences made hard to conciliate both projects at the technical level. Rebind wouldn't be possible without Jeason and the work @chenglou did. :sparkles:

As Jeason, Rebind contains a vendored version of the Flow parser, which allows to generate the JavaScript syntax tree.

## Similar projects

An influence for this project has been the [JavaScript to ReasonML Transpiler](https://github.com/emnh/js-to-reasonml-transpiler) by Eivind Magnus Hvidevold, that takes a different approach by running the JavaScript code at runtime and getting the type information there. This allows to get much better accuracy than Rebind, at the cost of a heavier footprint due to these runtime constraints.
