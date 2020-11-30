const { makeAst } = require("../");

const ast = makeAst("B: {{ title }}", module.filename);

module.exports = (render, props, children) => {
  const title = props.title.toUpperCase();
  return render(ast, { title }, children);
};
