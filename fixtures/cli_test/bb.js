const { Deprecated_Source } = require("../../");

module.exports = Deprecated_Source.funcWithString(
  "Bb",
  "B: {{ name }}",
  (ast) =>
    (env, { name }, children) =>
      env.render(ast, { name: name.toUpperCase() }, children)
);
