const { Source, Typescheme } = require("../../");

module.exports = Source.fn(
  "Bb",
  Typescheme.make([["name", Typescheme.string()]]),
  Typescheme.Child.make([]),
  (Env, { name }, _children) => Env.return_("B: " + name.toUpperCase())
);
