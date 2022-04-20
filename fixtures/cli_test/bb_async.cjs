const { Source, Typescheme } = require("../../");

module.exports = Source.fn(
  "Bb_async",
  Typescheme.make([["name", Typescheme.string()]]),
  Typescheme.Child.make([]),
  async (Env, { name }, _children) => Env.return_("B: " + name.toUpperCase())
);
