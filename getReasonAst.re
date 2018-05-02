/* run this through npm run getReasonAst to get the AST! */

/*[@bs.module] external make : string => nano = "nano";

[@bs.val] external setTimeout : (int, unit => unit => unit, int) => float = "setTimeout";

[@bs.val] [@bs.scope ("window", "location", "ancestorOrigins")]
external length : int = "length";

[@bs.send] external map : (array('a), 'a => 'b) => array('b) = "";

type nano;
[@bs.module "nano"] external make : string => nano = "something";

type nanoB;

[@bs.module] external make : nanoB = "nanoTwo";
[@bs.send] external something : nanoB => string => string = "";
let t = 2; */

[@bs.obj] external makeConfig : (string, string, string) => configureParamA = "";
