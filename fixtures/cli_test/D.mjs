import { Deprecated_Source } from "../../lib/es6/src/AcutisJs.mjs";

export default Deprecated_Source.funcWithString(
  "D",
  "D: {{ name }}",
  (ast) =>
    (env, { name }, children) =>
      env.render(ast, { name: name.toUpperCase() }, children)
);
