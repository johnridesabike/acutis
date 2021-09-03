type t<'a>
let toArray: t<'a> => array<'a>
let fromOld: Acutis_Types.NonEmpty.t<'a> => t<'a>
let fromArrayUnsafe: array<'a> => t<'a>
let hd: t<'a> => 'a
let get: (t<'a>, int) => option<'a>
let getExn: (t<'a>, int) => 'a
let reduceHd: (t<'a>, (. 'a, 'a) => 'a) => 'a
let reduce: (t<'a>, 'b, (. 'b, 'a) => 'b) => 'b
let map: (t<'a>, (. 'a) => 'b) => t<'b>
let zip: (t<'a>, t<'b>) => t<('a, 'b)>
let zipBy: (t<'a>, t<'b>, (. 'a, 'b) => 'c) => t<'c>
