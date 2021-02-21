/*
  We use a polymorphic variant because it has a nicer JS representation.
*/
type t<'a> = [#ok('a) | #errors(array<Acutis_Types.Errors.t>)]

@live
let mapU: (t<'a>, (. 'a) => 'b) => t<'b>
let map: (t<'a>, 'a => 'b) => t<'b>

@live
let flatMapU: (t<'a>, (. 'a) => t<'b>) => t<'b>
let flatMap: (t<'a>, 'a => t<'b>) => t<'b>

let getExn: t<'a> => 'a

@live
let getOrElseU: (t<'a>, (. array<Acutis_Types.Errors.t>) => 'a) => 'a
let getOrElse: (t<'a>, array<Acutis_Types.Errors.t> => 'a) => 'a
