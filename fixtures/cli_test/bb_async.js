const { Source } = require("../../");

module.exports = Source.funcWithString(
  "Bb_async",
  "B: {{ name }}",
  (ast) => async (env, { name }, children) =>
    env.render(ast, { name: name.toUpperCase() }, children)
);
