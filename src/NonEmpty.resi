/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
type t<'a> = private array<'a>
let fromQueue: ('a, Belt.MutableQueue.t<'a>) => t<'a>
let toArray: t<'a> => array<'a>
let size: t<'a> => int
let one: 'a => t<'a>
let two: ('a, 'a) => t<'a>
let fromArrayExn: array<'a> => t<'a>
let hd: t<'a> => 'a
let get: (t<'a>, int) => option<'a>
let getExn: (t<'a>, int) => 'a
let forEach: (t<'a>, (. 'a) => unit) => unit
let reduceHd: (t<'a>, (. 'a, 'a) => 'a) => 'a
let reduce: (t<'a>, 'b, (. 'b, 'a) => 'b) => 'b
let map: (t<'a>, (. 'a) => 'b) => t<'b>
let unzip: t<('a, 'b)> => (t<'a>, t<'b>)
let zipUnsafe: (t<'a>, t<'b>) => t<('a, 'b)>
let zipExn: (t<'a>, t<'b>) => t<('a, 'b)>
let zipByExn: (t<'a>, t<'b>, (. 'a, 'b) => 'c) => t<'c>
