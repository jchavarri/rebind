Basic functions

  $ cat > input.js <<\EOF
  > const nano = Nano("test", 4);
  > EOF

  $ rebind input.js | tee output.ml
  let nano = nano "test" 4
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 11-15:
  1 | let nano = nano "test" 4
                 ^^^^
  Error: Unbound value nano
  Hint: Did you mean nan?
  [2]

Require statements, more than one statement

  $ cat > input.js <<\EOF
  > const nano = require('nanoff')(5, "fsdf", 43);
  > const blue = require('js-pkgblue')("gf");
  > EOF

  $ rebind input.js | tee output.ml
  let nano = nanoff 5 "fsdf" 43
  let blue = js - pkgblue "gf"
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 11-17:
  1 | let nano = nanoff 5 "fsdf" 43
                 ^^^^^^
  Error: Unbound value nanoff
  [2]

Track identifiers across statements

  $ cat > input.js <<\EOF
  > const nano = require('nanoff');
  > const test = nano(5, "fsdf", 43);
  > EOF

  $ rebind input.js | tee output.ml
  let nano = nanoff
  let test = nano 5 "fsdf" 43
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 11-17:
  1 | let nano = nanoff
                 ^^^^^^
  Error: Unbound value nanoff
  [2]

Single require

  $ cat > input.js <<\EOF
  > const blue = require('blue');
  > EOF

  $ rebind input.js | tee output.ml
  let blue = blue
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 11-15:
  1 | let blue = blue
                 ^^^^
  Error: Unbound value blue
  [2]

Basic object property access

  $ cat > input.js <<\EOF
  > const nano = require('nano')('http://localhost:5984');
  > nano.db.test(23);
  > const blue = require('blue');
  > const go = blue.pill.jump(23);
  > EOF

  $ rebind input.js | tee output.ml
  let nano = nano "http://localhost:5984";;
  
  nano##db##test 23
  
  let blue = blue
  let go = blue##pill##jump 23
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 11-15:
  1 | let nano = nano "http://localhost:5984";;
                 ^^^^
  Error: Unbound value nano
  Hint: Did you mean nan?
  [2]

Functions as callbacks

  $ cat > input.js <<\EOF
  > const nano = require('nano')('http://localhost:5984');
  > nano.db.create('foo', (err, body, headers) => { // ... stuff here
  > })
  > EOF

  $ rebind input.js | tee output.ml
  let nano = nano "http://localhost:5984";;
  
  nano##db##create "foo" (fun err body headers -> ())
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 11-15:
  1 | let nano = nano "http://localhost:5984";;
                 ^^^^
  Error: Unbound value nano
  Hint: Did you mean nan?
  [2]

Object constants

  $ cat > input.js <<\EOF
  > var Airtable = require('airtable');
  > Airtable.configure({
  >     endpointUrl: 'https://api.airtable.com',
  >     apiKey: 'YOUR_API_KEY'
  > });
  > EOF

  $ rebind input.js | tee output.ml
  let airtable = airtable;;
  
  airtable##configure
    [%mel.obj
      { endpointUrl = "https://api.airtable.com"; apiKey = "YOUR_API_KEY" }]
  
  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 15-23:
  1 | let airtable = airtable;;
                     ^^^^^^^^
  Error: Unbound value airtable
  [2]

Objects, properties, callbacks, modules

  $ cat > input.js <<\EOF
  > var Airtable = require('airtable');
  > Airtable.configure({
  >     endpointUrl: 'https://api.airtable.com',
  >     apiKey: 'YOUR_API_KEY'
  > });
  > var base = Airtable.base('appXXXXXXXX');
  > base('MyBase').select({
  >   // Selecting the first 3 records in Grid view:
  >   maxRecords: 3,
  >   view: "Grid view"
  > }).eachPage(function page(records, fetchNextPage) {
  >   // This function (`page`) will get called for each page of records.
  > 
  >   records.forEach(function(record) {
  >       console.log('Retrieved', record.get('Task'));
  >   });
  > 
  >   // To fetch the next page of records, call `fetchNextPage`.
  >   // If there are more records, `page` will get called again.
  >   // If there are no more records, `done` will get called.
  >   fetchNextPage();
  > 
  > }, function done(err) {
  >   if (err) { console.error(err); return; }
  > });
  > EOF

  $ rebind input.js | tee output.ml
  let airtable = airtable;;
  
  airtable##configure
    [%mel.obj
      { endpointUrl = "https://api.airtable.com"; apiKey = "YOUR_API_KEY" }]
  
  let base = airtable##base "appXXXXXXXX";;
  
  ((base "MyBase")##select [%mel.obj { maxRecords = 3; view = "Grid view" }])##eachPage
    (fun records fetchNextPage ->
      records##forEach (fun record ->
          console##log "Retrieved" (record##get "Task"));
      fetchNextPage ())
    (fun err ->
      if err then (
        console##error err;
        ()))
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 15-23:
  1 | let airtable = airtable;;
                     ^^^^^^^^
  Error: Unbound value airtable
  [2]

Identifiers with same name

  $ cat > input.js <<\EOF
  > let jPush = JPush.jPush;
  > jPush.initPush();
  > EOF

  $ rebind input.js | tee output.ml
  let jPush = jPush##jPush;;
  
  jPush##initPush ()
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 12-17:
  1 | let jPush = jPush##jPush;;
                  ^^^^^
  Error: Unbound value jPush
  Hint: Did you mean flush?
  [2]

Stripe example

  $ cat > input.js <<\EOF
  > var stripe = require('stripe')('sk_test_...');
  > 
  > stripe.customers.create(
  >   { email: 'customer@example.com' },
  >   function(err, customer) {
  >     err; // null if no error occurred
  >     customer; // the created customer object
  >   }
  > );
  > EOF

  $ rebind input.js | tee output.ml
  let stripe = stripe "sk_test_...";;
  
  stripe##customers##create [%mel.obj { email = "customer@example.com" }]
    (fun err customer ->
      err;
      customer)
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 13-19:
  1 | let stripe = stripe "sk_test_...";;
                   ^^^^^^
  Error: Unbound value stripe
  [2]

Case with `new`

  $ cat > input.js <<\EOF
  > // From https://github.com/jchavarri/rebind/issues/3
  > const client = new AWSAppSyncClient({
  >   url: graphqlEndpoint,
  >   region: region,
  >   auth: {
  >     type: authenticationType,
  >     apiKey: apiKey
  >   }
  > });
  > EOF

  $ rebind input.js | tee output.ml
  let client = expressionPlaceholder
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 13-34:
  1 | let client = expressionPlaceholder
                   ^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound value expressionPlaceholder
  [2]

`new` from a module

  $ cat > input.js <<\EOF
  > const Blue = require('SomethingBlue');
  > const instance = new Blue("config");
  > EOF

  $ rebind input.js | tee output.ml
  let blue = somethingBlue
  let instance = expressionPlaceholder
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 11-24:
  1 | let blue = somethingBlue
                 ^^^^^^^^^^^^^
  Error: Unbound value somethingBlue
  [2]

`new` from a module using import

  $ cat > input.js <<\EOF
  > import { Blue as Red } from 'path/to/blue.js';
  > const instance = new Red("config");
  > import { Green } from 'path/to/green.js';
  > const instance2 = new Green("config");
  > import Pink from 'path/to/pink.js';
  > const instance2 = new Pink("config");
  > EOF

  $ rebind input.js | tee output.ml
  statementBail
  
  let instance = expressionPlaceholder;;
  
  statementBail
  
  let instance2 = expressionPlaceholder;;
  
  statementBail
  
  let instance2 = expressionPlaceholder
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 0-13:
  1 | statementBail
      ^^^^^^^^^^^^^
  Error: Unbound value statementBail
  [2]

Named import

  $ cat > input.js <<\EOF
  > import { Blue } from "SomethingBlue";
  > const instance = Blue("config");
  > EOF

  $ rebind input.js | tee output.ml
  statementBail
  
  let instance = blue "config"
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 0-13:
  1 | statementBail
      ^^^^^^^^^^^^^
  Error: Unbound value statementBail
  [2]

Default imports

  $ cat > input.js <<\EOF
  > // Example from https://reasonml.chat/t/how-to-improve-my-codes/663
  > import Keycloak from 'keycloak-js';
  > const keycloak = Keycloak('config/keycloak.json');
  > keycloak.init({ onLoad: 'login-required' }).success(function(authenticated) {
  >   alert(authenticated ? 'authenticated' : 'not authenticated');
  > }).error(function() {
  >   alert('failed to initialize');
  > });
  > EOF

  $ rebind input.js | tee output.ml
  statementBail
  
  let keycloak = keycloak "config/keycloak.json";;
  
  ((keycloak##init [%mel.obj { onLoad = "login-required" }])##success
     (fun authenticated ->
       alert
         (match authenticated with
         | true -> "authenticated"
         | false -> "not authenticated")))##error (fun () ->
      alert "failed to initialize")
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 0-13:
  1 | statementBail
      ^^^^^^^^^^^^^
  Error: Unbound value statementBail
  [2]

Named imports with local name

  $ cat > input.js <<\EOF
  > import { Blue as Green } from "SomethingBlue";
  > const instance = Green("config");
  > const t = green(2);
  > EOF

  $ rebind input.js | tee output.ml
  statementBail
  
  let instance = green "config"
  let t = green 2
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 0-13:
  1 | statementBail
      ^^^^^^^^^^^^^
  Error: Unbound value statementBail
  [2]

Namespaced imports

  $ cat > input.js <<\EOF
  > import * as Blue from "SomethingBlue";
  > const instance = Blue("config");
  > EOF

  $ rebind input.js | tee output.ml
  statementBail
  
  let instance = blue "config"
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 0-13:
  1 | statementBail
      ^^^^^^^^^^^^^
  Error: Unbound value statementBail
  [2]

Unexpected token error (no need to run melc)

  $ cat > input.js <<\EOF
  > const t = Test({: 1, : 2});
  > EOF

  $ rebind input.js | tee output.ml
  Fatal error: exception Failure("ocamlformat error")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Dune__exe__Rebind.processContent in file "src/native/rebind.ml", line 44, characters 18-46
  Called from Dune__exe__Rebind in file "src/native/rebind.ml", line 51, characters 7-24

Using a reserved word (`then`)

  $ cat > input.js <<\EOF
  > var stripe = require("stripe");
  > stripe.createToken('bank_account').then(function(result) {});
  > EOF

  $ rebind input.js | tee output.ml
  Fatal error: exception Failure("ocamlformat error")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Dune__exe__Rebind.processContent in file "src/native/rebind.ml", line 44, characters 18-46
  Called from Dune__exe__Rebind in file "src/native/rebind.ml", line 51, characters 7-24

  $ melc -ppx melppx output.ml
  // Generated by Melange
  /* This output is empty. Its source's type definitions, externals and/or unused code got optimized away. */

Dots and other strange chars in import names

  $ cat > input.js <<\EOF
  > const BN = require('bn.js');
  > 
  > var a = new BN('dead', 16);
  > var b = new BN('101010', 2);
  > 
  > var res = a.add(b);
  > 
  > import * as stripe from 'stripe-js';
  > stripe.createToken('bank_account');
  > EOF

  $ rebind input.js | tee output.ml
  let bN = bn.js
  let a = expressionPlaceholder
  let b = expressionPlaceholder
  let res = a##add b;;
  
  statementBail;;
  stripe##createToken "bank_account"
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 9-11:
  1 | let bN = bn.js
               ^^
  Error: Unbound value bn
  [2]

=== Examples from https://github.com/yawaramin/bucklescript-bindings-cookbook ===

--- Globals ---

Variable in global module

  $ cat > input.js <<\EOF
  > function calculateCircumference(radius) {
  >   return 2 * Math.PI * radius;
  > }
  > EOF

  $ rebind input.js | tee output.ml
  let calculateCircumference radius = 2 * math##pI * radius
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 40-44:
  1 | let calculateCircumference radius = 2 * math##pI * radius
                                              ^^^^
  Error: Unbound value math
  [2]

Function in global module

--- Modules ---

Function in commonJS/ES6 module

  $ cat > input.js <<\EOF
  > const path = require('path');
  > path.join('a', 'b')
  > EOF

  $ rebind input.js | tee output.ml
  let path = path;;
  
  path##join "a" "b"
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 11-15:
  1 | let path = path;;
                 ^^^^
  Error: Unbound value path
  [2]

Import entire module as a value

  $ cat > input.js <<\EOF
  > const foo = require('foo');
  > foo(1);
  > EOF

  $ rebind input.js | tee output.ml
  let foo = foo;;
  
  foo 1
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 10-13:
  1 | let foo = foo;;
                ^^^
  Error: Unbound value foo
  [2]

Import ES6 module default export

  $ cat > input.js <<\EOF
  > import foo from 'foo';
  > foo(1);
  > EOF

  $ rebind input.js | tee output.ml
  statementBail;;
  foo 1
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 0-13:
  1 | statementBail;;
      ^^^^^^^^^^^^^
  Error: Unbound value statementBail
  [2]

Function scoped inside an object in a module

  $ cat > input.js <<\EOF
  > const foo = require('foo');
  > foo.bar.baz();
  > EOF

  $ rebind input.js | tee output.ml
  let foo = foo;;
  
  foo##bar##baz ()
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 10-13:
  1 | let foo = foo;;
                ^^^
  Error: Unbound value foo
  [2]

Function with rest args

  $ cat > input.js <<\EOF
  > const dir = path.join('a', 'b', 'c')
  > EOF

  $ rebind input.js | tee output.ml
  let dir = path##join "a" "b" "c"
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 10-14:
  1 | let dir = path##join "a" "b" "c"
                ^^^^
  Error: Unbound value path
  [2]

Call a function with named arguments for readability

  $ cat > input.js <<\EOF
  > const nums = range(start, stop, step)
  > EOF

  $ rebind input.js | tee output.ml
  let nums = range start stop step
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 11-16:
  1 | let nums = range start stop step
                 ^^^^^
  Error: Unbound value range
  Hint: Did you mean raise?
  [2]

Overloaded function

  $ cat > input.js <<\EOF
  > foo('hello');
  > foo(true);
  > EOF

  $ rebind input.js | tee output.ml
  foo "hello";;
  foo Js.true_
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 0-3:
  1 | foo "hello";;
      ^^^
  Error: Unbound value foo
  [2]

Optional final argument(s)

  $ cat > input.js <<\EOF
  > const nums = range(start, stop, /* optional */ step);
  > EOF

  $ rebind input.js | tee output.ml
  let nums = range start stop step
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 11-16:
  1 | let nums = range start stop step
                 ^^^^^
  Error: Unbound value range
  Hint: Did you mean raise?
  [2]

Options object argument

  $ cat > input.js <<\EOF
  > mkdir('src/main', /* optional */ {recursive: true});
  > EOF

  $ rebind input.js | tee output.ml
  mkdir "src/main" [%mel.obj { recursive = Js.true_ }]
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 0-5:
  1 | mkdir "src/main" [%mel.obj { recursive = Js.true_ }]
      ^^^^^
  Error: Unbound value mkdir
  [2]

Model a callback

  $ cat > input.js <<\EOF
  > forEach(start, stop, item => console.log(item));
  > EOF

  $ rebind input.js | tee output.ml
  forEach start stop (fun item -> console##log item)
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 0-7:
  1 | forEach start stop (fun item -> console##log item)
      ^^^^^^^
  Error: Unbound value forEach
  [2]

--- Objects ---

Create an object

  $ cat > input.js <<\EOF
  > const person = {id: 1, name: 'Bob'};
  > EOF

  $ rebind input.js | tee output.ml
  let person = [%mel.obj { id = 1; name = "Bob" }]
  

  $ melc -ppx melppx output.ml
  // Generated by Melange
  'use strict';
  
  
  var person = {
    id: 1,
    name: "Bob"
  };
  
  exports.person = person;
  /* No side effect */

Get a prop

  $ cat > input.js <<\EOF
  > person.name;
  > EOF

  $ rebind input.js | tee output.ml
  person##name
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 0-6:
  1 | person##name
      ^^^^^^
  Error: Unbound value person
  [2]

Set a prop

  $ cat > input.js <<\EOF
  > person.id = 0;
  > EOF

  $ rebind input.js | tee output.ml
  person ## id #= 0
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 0-6:
  1 | person ## id #= 0
      ^^^^^^
  Error: Unbound value person
  [2]

Object with destructuring

  $ cat > input.js <<\EOF
  > const {id, name} = person;
  > EOF

  $ rebind input.js | tee output.ml
  let "destructuringNotImplemented" = person
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 36-42:
  1 | let "destructuringNotImplemented" = person
                                          ^^^^^^
  Error: Unbound value person
  [2]

--- Classes and OOP ---

Call a class constructor

  $ cat > input.js <<\EOF
  > const foo = new Foo();
  > EOF

  $ rebind input.js | tee output.ml
  let foo = expressionPlaceholder
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 10-31:
  1 | let foo = expressionPlaceholder
                ^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound value expressionPlaceholder
  [2]

Get an instance property

  $ cat > input.js <<\EOF
  > const bar = foo.bar;
  > EOF

  $ rebind input.js | tee output.ml
  let bar = foo##bar
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 10-13:
  1 | let bar = foo##bar
                ^^^
  Error: Unbound value foo
  [2]

Set an instance property

  $ cat > input.js <<\EOF
  > foo.bar = 1;
  > EOF

  $ rebind input.js | tee output.ml
  foo ## bar #= 1
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 0-3:
  1 | foo ## bar #= 1
      ^^^
  Error: Unbound value foo
  [2]

Call a nullary instance method

  $ cat > input.js <<\EOF
  > foo.meth();
  > EOF

  $ rebind input.js | tee output.ml
  foo##meth ()
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 0-3:
  1 | foo##meth ()
      ^^^
  Error: Unbound value foo
  [2]

Non-mutating instance method

  $ cat > input.js <<\EOF
  > const newStr = str.replace(substr, newSubstr);
  > EOF

  $ rebind input.js | tee output.ml
  let newStr = str##replace substr newSubstr
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 13-16:
  1 | let newStr = str##replace substr newSubstr
                   ^^^
  Error: Unbound value str
  [2]

Mutating instance method

  $ cat > input.js <<\EOF
  > arr.sort(compareFunction);
  > EOF

  $ rebind input.js | tee output.ml
  arr##sort compareFunction
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 0-3:
  1 | arr##sort compareFunction
      ^^^
  Error: Unbound value arr
  Hint: Did you mean asr?
  [2]

--- Null and undefined ---

Check for undefined

  $ cat > input.js <<\EOF
  > foo.bar === undefined;
  > EOF

  $ rebind input.js | tee output.ml
  foo##bar == undefined
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 0-3:
  1 | foo##bar == undefined
      ^^^
  Error: Unbound value foo
  [2]

Check for null or undefined

  $ cat > input.js <<\EOF
  > foo.bar == null;
  > EOF

  $ rebind input.js | tee output.ml
  Js.Null_undefined.test foo##bar
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 0-22:
  1 | Js.Null_undefined.test foo##bar
      ^^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound value Js.Null_undefined.test
  [2]

Single object

  $ cat > input.js <<\EOF
  > const languageMap = {
  >   Reason: "Reason",
  >   OCaml: "OCaml",
  > };
  > EOF

  $ rebind input.js | tee output.ml
  let languageMap = [%mel.obj { reason = "Reason"; oCaml = "OCaml" }]
  

  $ melc -ppx melppx output.ml
  // Generated by Melange
  'use strict';
  
  
  var languageMap = {
    reason: "Reason",
    oCaml: "OCaml"
  };
  
  exports.languageMap = languageMap;
  /* No side effect */

TODO - Appearances of `import` keyword

  $ cat > input.js <<\EOF
  > const worker = new Worker(new URL("./worker.js", import.meta.url), {
  >   type: "module",
  > });
  > EOF

  $ rebind input.js | tee output.ml
  let worker = expressionPlaceholder
  

  $ melc -ppx melppx output.ml
  File "output.ml", line 1, characters 13-34:
  1 | let worker = expressionPlaceholder
                   ^^^^^^^^^^^^^^^^^^^^^
  Error: Unbound value expressionPlaceholder
  [2]


TODO - React elements

import { Toaster } from './toaster';
 <Toaster />

TODO - React components

function OCamlLogo() {
return <span className="SquareLogo OCaml"></span>
}
let t = <OCamlLogo />

TODO - Bizarre character `/`

import * as Console from "./console";

Console(243)
