import { Source, Typescheme } from "../../lib/es6/src/AcutisJs.mjs";

export default Source.fn(
  "D_async",
  Typescheme.props([["name", Typescheme.string()]]),
  Typescheme.Child.props([]),
  async (Env, { name }, _children) => Env.return_("D: " + name.toUpperCase())
);
