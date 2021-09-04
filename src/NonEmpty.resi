type t<'a> = private array<'a>
let fromQueue: ('a, Belt.MutableQueue.t<'a>) => t<'a>
let toArray: t<'a> => array<'a>
let one: 'a => t<'a>
let two: ('a, 'a) => t<'a>
let fromArrayExn: array<'a> => t<'a>
let hd: t<'a> => 'a
let get: (t<'a>, int) => option<'a>
let getExn: (t<'a>, int) => 'a
let reduceHd: (t<'a>, (. 'a, 'a) => 'a) => 'a
let reduce: (t<'a>, 'b, (. 'b, 'a) => 'b) => 'b
let map: (t<'a>, (. 'a) => 'b) => t<'b>
let zip: (t<'a>, t<'b>) => t<('a, 'b)>
let zipBy: (t<'a>, t<'b>, (. 'a, 'b) => 'c) => t<'c>
