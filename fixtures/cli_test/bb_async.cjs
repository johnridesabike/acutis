const { Deprecated_Source } = require("../../lib/js/src/AcutisJs");

module.exports = Deprecated_Source.funcWithString(
  "Bb_async",
  "B: {{ name }}",
  (ast) =>
    async (env, { name }, children) =>
      env.render(ast, { name: name.toUpperCase() }, children)
);
