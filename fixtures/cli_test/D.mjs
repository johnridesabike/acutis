import { Source, Typescheme } from "../../lib/es6/src/AcutisJs.mjs";

export default Source.fn(
  "D",
  Typescheme.props([["name", Typescheme.string()]]),
  Typescheme.Child.props([]),
  (Env, { name }, _children) => Env.$$return("D: " + name.toUpperCase())
);
