/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module T = Acutis_Types

type stringFunc<'a> = T.ast<'a> => T.template<'a>

type t<'a> =
  | String({name: string, src: string})
  | Func({name: string, f: T.template<'a>})
  | StringFunc({name: string, src: string, f: stringFunc<'a>})

let string = (~name, src) => String({name: name, src: src})

let func = (~name, f) => Func({name: name, f: f})

let funcWithString = (~name, src, f) => StringFunc({name: name, src: src, f: f})

let name = (String({name, _}) | Func({name, _}) | StringFunc({name, _})) => name
