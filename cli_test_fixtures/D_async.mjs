import { Compile } from "../dist/acutis.mjs";

const ast = Compile.makeAst("D: {{ title }}", import.meta.url);

export default async (env, { title }, children) => {
  return env.render(ast, { title: title.toUpperCase() }, children);
};
