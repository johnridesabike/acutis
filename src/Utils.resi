/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

// This is a placeholder type until we add more sophisticated error reporting.
@unboxed
type loc = Acutis_Types.loc = Loc(int)

module Dag: {
  type linked<'a>
  let toArray: linked<'a> => array<'a>
  let empty: unit => linked<'a>
  type t<'a, 'b>
  let make: array<(string, 'a)> => t<'a, 'b>
  let fromLinked: linked<'b> => t<'a, 'b>
  let getExn: (t<'a, 'b>, ~name: string, ~loc: loc, ~key: string, ~f: (. t<'a, 'b>, 'a) => 'b) => 'b
  let link: (t<'a, 'b>, ~f: (. t<'a, 'b>, 'a) => 'b) => linked<'b>
}
