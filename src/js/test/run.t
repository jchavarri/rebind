Prepare an input file to test some snippets to exercise common functionality

  $ cat > input.js <<\EOF
  > require(process.env.DUNE_SOURCEROOT + '/_build/default/src/js/rebind_js.bc.js');
  > console.log(rebind.generateML("const nano = Nano(\"test\", 4);"));
  > EOF

  $ node input.js
  {
    code: 'type nano\n' +
      'external nano : string -> int -> nano = "Nano"\n' +
      'let nano = nano "test" 4'
  }
