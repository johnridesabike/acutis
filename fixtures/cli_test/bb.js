const { Source, Typescheme } = require("../../");

module.exports = Source.fn(
  "Bb",
  Typescheme.props([["name", Typescheme.string()]]),
  Typescheme.Child.props([]),
  (Env, { name }, _children) => Env.return_("B: " + name.toUpperCase())
);
