/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
type escape = NoEscape | Escape

let escape: (escape, string) => string

module Dagmap: {
  type map<'a> = Belt.HashMap.String.t<'a>
  type t<'a, 'b>
  let make: (map<'a>, ~f: (. t<'a, 'b>, 'a) => 'b) => t<'a, 'b>
  let prelinked: map<'a> => t<'a, 'a>
  let get: (t<'a, 'b>, string, Debug.t) => 'b
  let linkAll: t<'a, 'b> => map<'b>
}
