open Jest;

let getOutput = testName =>
  Node.Child_process.execSync(
    {j|./_build/src/native/Main.native ./__tests__/input/$testName.js | refmt --parse binary --print re|j},
    Node.Child_process.option(~encoding="utf8", ()),
  );

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