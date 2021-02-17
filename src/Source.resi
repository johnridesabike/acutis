type stringFunc<'a> = (. Acutis_Types.Ast.t) => Acutis_Types.template<'a>

type t<'a> =
  private
  | String({name: string, src: string})
  | Func({name: string, f: Acutis_Types.template<'a>})
  | StringFunc({name: string, src: string, f: stringFunc<'a>})

let string: (~name: string, string) => t<'a>

let func: (~name: string, Acutis_Types.template<'a>) => t<'a>

let funcWithString: (~name: string, string, stringFunc<'a>) => t<'a>

let name: t<_> => string
