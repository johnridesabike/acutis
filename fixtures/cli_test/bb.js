const { Compile } = require("../../");

const ast = Compile.makeAst("B: {{ name }}", module.filename);

module.exports = (env, { name }, children) => {
  return env.render(ast, { name: name.toUpperCase() }, children);
};
