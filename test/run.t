Basic functions

  $ cat > input.js <<\EOF
  > const nano = Nano("test", 4);
  > EOF

  $ rebind input.js
  type nano
  external nano : string -> float -> nano = "Nano"[@@bs.val ]

Require statements, more than one statement

  $ cat > input.js <<\EOF
  > const nano = require('nano')(5, "fsdf", 43);
  > const blue = require('blue')("gf");
  > EOF

  $ rebind input.js
  type nano
  type blue
  external nano : float -> string -> float -> nano = ""[@@bs.module ]
  external blue : string -> blue = ""[@@bs.module ]

Track identifiers across statements

  $ cat > input.js <<\EOF
  > const nano = require('nano');
  > const test = nano(5, "fsdf", 43);
  > EOF

  $ rebind input.js
  type nano
  external nano : float -> string -> float -> nano = ""[@@bs.module ]

Single require

  $ cat > input.js <<\EOF
  > const blue = require('blue');
  > EOF

  $ rebind input.js
  type blue

Basic object property access

  $ cat > input.js <<\EOF
  > const nano = require('nano')('http://localhost:5984');
  > nano.db.test(23);
  > const blue = require('blue');
  > const go = blue.pill.jump(23);
  > EOF

  $ rebind input.js
  type nano
  type db
  type test
  type blue
  type pill
  type jump
  external nano : string -> nano = ""[@@bs.module ]
  external db : nano -> db = ""[@@bs.get ]
  external test : db -> float -> test = ""[@@bs.send ]
  external blue : blue = ""[@@bs.module ]
  external pill : blue -> pill = ""[@@bs.get ]
  external jump : pill -> float -> jump = ""[@@bs.send ]

Functions as callbacks

  $ cat > input.js <<\EOF
  > const nano = require('nano')('http://localhost:5984');
  > nano.db.create('foo', (err, body, headers) => { // ... stuff here
  > })
  > EOF

  $ rebind input.js
  type nano
  type err
  type body
  type headers
  type callback
  type db
  type create
  external nano : string -> nano = ""[@@bs.module ]
  external db : nano -> db = ""[@@bs.get ]
  external create :
    db -> string -> (err -> body -> headers -> callback) -> create = ""
  [@@bs.send ]

Object constants

  $ cat > input.js <<\EOF
  > var Airtable = require('airtable');
  > Airtable.configure({
  >     endpointUrl: 'https://api.airtable.com',
  >     apiKey: 'YOUR_API_KEY'
  > });
  > EOF

  $ rebind input.js
  type airtable2
  type configureParam
  type configure
  external makeConfigureParam :
    apiKey:string -> endpointUrl:string -> unit -> configureParam = ""[@@bs.obj
                                                                      ]
  external airtable2 : airtable2 = "airtable"[@@bs.module ]
  external configure : airtable2 -> configureParam -> configure = ""[@@bs.send
                                                                      ]

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

  $ rebind input.js
  type airtable2
  type configureParam
  type configure
  type base
  type records
  type fetchNextPage
  type page
  type err
  type done_
  type selectParam
  type select
  type eachPage
  external makeConfigureParam :
    apiKey:string -> endpointUrl:string -> unit -> configureParam = ""[@@bs.obj
                                                                      ]
  external airtable2 : airtable2 = "airtable"[@@bs.module ]
  external configure : airtable2 -> configureParam -> configure = ""[@@bs.send
                                                                      ]
  external base : airtable2 -> string -> base = ""[@@bs.send ]
  external makeSelectParam :
    view:string -> maxRecords:float -> unit -> selectParam = ""[@@bs.obj ]
  external select : base -> selectParam -> select = ""[@@bs.send ]
  external eachPage :
    select -> (records -> fetchNextPage -> page) -> (err -> done_) -> eachPage
      = ""[@@bs.send ]

Identifiers with same name

  $ cat > input.js <<\EOF
  > let jPush = JPush.jPush;
  > jPush.initPush();
  > EOF

  $ rebind input.js
  type jPush
  type jPush2
  type initPush
  external jPush : jPush = "JPush"[@@bs.val ]
  external jPush2 : jPush -> jPush2 = "jPush"[@@bs.get ]
  external initPush : jPush2 -> initPush = ""[@@bs.send ]

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

  $ rebind input.js
  type stripe
  type createParam
  type err
  type customer
  type callback
  type customers
  type create
  external stripe : string -> stripe = ""[@@bs.module ]
  external makeCreateParam : email:string -> unit -> createParam = ""[@@bs.obj
                                                                      ]
  external customers : stripe -> customers = ""[@@bs.get ]
  external create :
    customers -> createParam -> (err -> customer -> callback) -> create = ""
  [@@bs.send ]

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

  $ rebind input.js
  type graphqlEndpoint
  type region
  type authenticationType
  type apiKey
  type auth
  type aWSAppSyncClientParam
  type aWSAppSyncClient
  external makeAuth :
    apiKey:apiKey -> type:authenticationType -> unit -> auth = ""[@@bs.obj ]
  external makeAWSAppSyncClientParam :
    auth:auth ->
      region:region -> url:graphqlEndpoint -> unit -> aWSAppSyncClientParam =
      ""[@@bs.obj ]
  external aWSAppSyncClient :
    aWSAppSyncClientParam -> aWSAppSyncClient = "AWSAppSyncClient"[@@bs.new ]

`new` from a module

  $ cat > input.js <<\EOF
  > const Blue = require('SomethingBlue');
  > const instance = new Blue("config");
  > EOF

  $ rebind input.js
  type somethingBlue
  external somethingBlue : string -> somethingBlue = "SomethingBlue"[@@bs.new ]
  [@@bs.module ]

named `import`

  $ cat > input.js <<\EOF
  > import { Blue } from "SomethingBlue";
  > const instance = Blue("config");
  > EOF

  $ rebind input.js
  type blue
  external blue : string -> blue = "Blue"[@@bs.module "SomethingBlue"]

default imports

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

  $ rebind input.js
  type keycloak
  type callback
  type authenticated
  type initParam
  type init
  type success
  type error
  external keycloak : string -> keycloak = "default"[@@bs.module "keycloak-js"]
  external makeInitParam : onLoad:string -> unit -> initParam = ""[@@bs.obj ]
  external init : keycloak -> initParam -> init = ""[@@bs.send ]
  external success : init -> (authenticated -> callback) -> success = ""
  [@@bs.send ]
  external error : success -> callback -> error = ""[@@bs.send ]

named imports with local name

  $ cat > input.js <<\EOF
  > import { Blue as Green } from "SomethingBlue";
  > const instance = Green("config");
  > const t = green(2);
  > EOF

  $ rebind input.js
  type green
  type green2
  external green : string -> green = "Blue"[@@bs.module "SomethingBlue"]
  external green2 : float -> green2 = "green"[@@bs.val ]

namespaced imports

  $ cat > input.js <<\EOF
  > import * as Blue from "SomethingBlue";
  > const instance = Blue("config");
  > EOF

  $ rebind input.js
  type blue
  type somethingBlue
  external somethingBlue : string -> somethingBlue = "SomethingBlue"[@@bs.module
                                                                      ]

unexpected token error

  $ cat > input.js <<\EOF
  > const t = Test({: 1, : 2});
  > EOF

  $ rebind input.js
  Fatal error: exception Parse_error.Error(_)
  [2]

then as reserved word

  $ cat > input.js <<\EOF
  > var stripe = require("stripe");
  > stripe.createToken('bank_account').then(function(result) {});
  > EOF

  $ rebind input.js
  type stripe
  type result
  type callback
  type createToken
  type then_
  external stripe : stripe = ""[@@bs.module ]
  external createToken : stripe -> string -> createToken = ""[@@bs.send ]
  external then_ : createToken -> (result -> callback) -> then_ = "then"
  [@@bs.send ]

dots and other strange chars in import names

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

  $ rebind input.js
  type bnjs
  type add
  type stripe
  type stripejs
  type createToken
  external bnjs : string -> float -> bnjs = "bn.js"[@@bs.new ][@@bs.module ]
  external add : bnjs -> bnjs -> add = ""[@@bs.send ]
  external stripejs : stripejs = "stripe-js"[@@bs.module ]
  external createToken : stripejs -> string -> createToken = ""[@@bs.send ]

