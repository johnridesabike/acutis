const { makeAst } = require("../");

const ast = makeAst("B: {{ title }}", module.filename);

module.exports = async (render, { title }, children) => {
  title = title.toUpperCase();
  return render(ast, { title }, children);
};
