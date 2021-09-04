type t
exception DecodeError
let make: (TypeChecker.t, Js.Json.t) => t
exception RenderError
let booleanExn: t => bool
let stringExn: t => string
let intExn: t => int
let floatExn: t => float
let tupleExn: t => array<t>
let dictExn: t => Js.Dict.t<t>
let isNull: t => bool
