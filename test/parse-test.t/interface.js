import acutis from "#main";

process.stdout.write(
  acutis.Acutis({}).debugInterface({
    a: { a: ["enum", 0, 1], b: ["enum", "a", "b"] },
    b: [
      "union",
      { tag: ["tag", true], a: ["list", "int"] },
      { tag: ["tag", false], a: ["dict", ["nullable", "string"]] },
    ],
    c: [
      "union",
      { tag: ["tag", 0] },
      { tag: ["tag", 1], a: ["tuple", "float", ["enum", true, false]] },
    ],
    d: [
      "union",
      { tag: ["tag", "a"], a: "float" },
      { tag: ["tag", "b"], a: ["enum_open", 0, 1] },
    ],
    e: [
      "union_open",
      { tag: ["tag", 0], a: "_" },
      { tag: ["tag", 1], b: ["enum_open", "a", "b"] },
    ],
  }),
);
