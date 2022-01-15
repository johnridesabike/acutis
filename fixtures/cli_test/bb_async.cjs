const { Source, Typescheme } = require("../../");

module.exports = Source.fn(
  "Bb_async",
  Typescheme.props([["name", Typescheme.string()]]),
  Typescheme.Child.props([]),
  async (Env, { name }, _children) => Env.return_("B: " + name.toUpperCase())
);
