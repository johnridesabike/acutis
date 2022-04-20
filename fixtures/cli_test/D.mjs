import { Source, Typescheme } from "../../lib/es6/src/AcutisJs.mjs";

export default Source.fn(
  "D",
  Typescheme.make([["name", Typescheme.string()]]),
  Typescheme.Child.make([]),
  (Env, { name }, _children) => Env.return_("D: " + name.toUpperCase())
);
