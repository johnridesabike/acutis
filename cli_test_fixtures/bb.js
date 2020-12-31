const { Compile } = require("../");

const ast = Compile.makeAst("B: {{ title }}", module.filename);

module.exports = (env, { title }, children) => {
  return env.render(ast, { title: title.toUpperCase() }, children);
};
