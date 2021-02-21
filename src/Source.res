open Acutis_Types

type stringFunc<'a> = Ast.t => template<'a>

type t<'a> =
  | String({name: string, src: string})
  | Func({name: string, f: template<'a>})
  | StringFunc({name: string, src: string, f: stringFunc<'a>})

let string = (~name, src) => String({name: name, src: src})

let func = (~name, f) => Func({name: name, f: f})

let funcWithString = (~name, src, f) => StringFunc({name: name, src: src, f: f})

let name = (String({name, _}) | Func({name, _}) | StringFunc({name, _})) => name
