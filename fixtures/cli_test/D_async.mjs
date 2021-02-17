import { Source } from "../../lib/es6/src/AcutisJs.mjs";

export default Source.funcWithString(
  "D_async",
  "D: {{ name }}",
  (ast) => async (env, { name }, children) =>
    env.render(ast, { name: name.toUpperCase() }, children)
);
