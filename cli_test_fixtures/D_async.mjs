import { makeAst } from "../dist/acutis.mjs";

const ast = makeAst("D: {{ title }}", import.meta.url);

export default async function make(render, { title }, children) {
  return render(ast, { title: title.toUpperCase() }, children);
}
