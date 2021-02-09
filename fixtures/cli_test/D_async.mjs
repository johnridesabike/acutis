import { Compile } from "../../lib/es6/src/AcutisJs.mjs";

const ast = Compile.makeAst("D: {{ name }}", import.meta.url);

export default async (env, { name }, children) => {
  return env.render(ast, { name: name.toUpperCase() }, children);
};
