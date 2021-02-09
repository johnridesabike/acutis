import { Compile } from "../../lib/es6/src/AcutisJs.mjs";

const ast = Compile.makeAst("D: {{ name }}", import.meta.url);

export default function make(env, { name }, children) {
  return env.render(ast, { name: name.toUpperCase() }, children);
}
