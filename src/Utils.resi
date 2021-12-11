/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

// This is a placeholder type until we add more sophisticated error reporting.
@unboxed
type loc = Acutis_Types.loc = Loc(int)

module Dagmap: {
  type t<'a, 'b>
  let make: (array<(string, 'a)>, ~f: (. t<'a, 'b>, 'a) => 'b) => t<'a, 'b>
  let prelinked: array<(string, 'a)> => t<'a, 'a>
  let getExn: (t<'a, 'b>, ~name: string, ~loc: loc, ~key: string) => 'b
  let link: t<'a, 'b> => array<(string, 'b)>
}
