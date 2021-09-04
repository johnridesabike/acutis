/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

/* We use a polymorphic variant because it has a nicer JS representation */
type t<'a> = [#ok('a) | #errors(array<Debug.t>)]

@live
let mapU: (t<'a>, (. 'a) => 'b) => t<'b>
let map: (t<'a>, 'a => 'b) => t<'b>

@live
let flatMapU: (t<'a>, (. 'a) => t<'b>) => t<'b>
let flatMap: (t<'a>, 'a => t<'b>) => t<'b>

let getExn: t<'a> => 'a

@live
let getOrElseU: (t<'a>, (. array<Debug.t>) => 'a) => 'a
let getOrElse: (t<'a>, array<Debug.t> => 'a) => 'a
