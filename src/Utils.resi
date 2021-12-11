/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Dagmap: {
  type map<'a> = Belt.HashMap.String.t<'a>
  type t<'a, 'b>
  let make: (map<'a>, ~f: (. t<'a, 'b>, 'a) => 'b) => t<'a, 'b>
  let prelinked: map<'a> => t<'a, 'a>
  let getExn: (t<'a, 'b>, ~name: string, ~loc: Debug.loc, ~key: string) => 'b
  let linkAll: t<'a, 'b> => map<'b>
}
