import { Source, Typescheme } from "../../lib/es6/src/AcutisJs.mjs";

export default Source.fn(
  "D_async",
  Typescheme.make([["name", Typescheme.string()]]),
  Typescheme.Child.make([]),
  async (Env, { name }, _children) => Env.return_("D: " + name.toUpperCase())
);
