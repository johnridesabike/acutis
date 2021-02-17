import { Source } from "../../lib/es6/src/AcutisJs.mjs";

export default Source.funcWithString(
  "D",
  "D: {{ name }}",
  (ast) => (env, { name }, children) =>
    env.render(ast, { name: name.toUpperCase() }, children)
);
