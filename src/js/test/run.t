Prepare an input file to test some snippets to exercise common functionality

  $ cat > input.js <<\EOF
  > require(process.env.DUNE_SOURCEROOT + '/_build/default/src/js/rebind_js.bc.js');
  > console.log(rebind.generateML("const nano = Nano(\"test\", 4);"));
  > EOF

  $ node input.js
  { code: 'type nano\nexternal nano : string -> float -> nano = "Nano"' }
