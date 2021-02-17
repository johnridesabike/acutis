const { Source } = require("../../");

module.exports = Source.funcWithString(
  "Bb",
  "B: {{ name }}",
  (ast) => (env, { name }, children) =>
    env.render(ast, { name: name.toUpperCase() }, children)
);
