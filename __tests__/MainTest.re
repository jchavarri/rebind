open Jest;

let binaryPath = testName => {j|./lib/bs/native/Rebind.exe ./__tests__/input/$testName.js|j};
let pipeToRefmt = {j| | refmt --parse binary --print re|j};
let getOutput = (~error=false, testName) => {
  let command =
    error ? binaryPath(testName) : binaryPath(testName) ++ pipeToRefmt;

  Node.Child_process.execSync(
    command,
    Node.Child_process.option(~encoding="utf8", ()),
  );
};

Expect.(
  test("test1 - Basic functions", () =>
    expect(getOutput("001.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test2 - Require statements, more than one statement", () =>
    expect(getOutput("002.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test3 - Track identifiers across statements", () =>
    expect(getOutput("003.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test4 - Single require", () =>
    expect(getOutput("004.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test5 - Basic object property access", () =>
    expect(getOutput("005.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test6 - Functions as callbacks", () =>
    expect(getOutput("006.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test7 - Object constants", () =>
    expect(getOutput("007.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test8 - Objects, properties, callbacks, modules", () =>
    expect(getOutput("008.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test9 - Identifiers with same name", () =>
    expect(getOutput("009.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test10 - Stripe example", () =>
    expect(getOutput("010.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test11 - Case with `new`", () =>
    expect(getOutput("011.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test12 - `new` from a module", () =>
    expect(getOutput("012.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test13 - named `import`", () =>
    expect(getOutput("013.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test14 - default imports", () =>
    expect(getOutput("014.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test15 - named imports with local name", () =>
    expect(getOutput("015.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test16 - namespaced imports", () =>
    expect(getOutput("016.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test17 - unexpected token error", () =>
    expect(() =>
      getOutput(~error=true, "017.test")
    ) |> toThrow
  )
);

Expect.(
  test("test18 - then as reserved word", () =>
    expect(getOutput("018.test")) |> toMatchSnapshot
  )
);

Expect.(
  test("test19 - dots and other strange chars in import names", () =>
    expect(getOutput("019.test")) |> toMatchSnapshot
  )
);