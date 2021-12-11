/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

open Acutis_Types

type stringFunc<'a> = ast<'a> => template<'a>

type t<'a> =
  private
  | String({name: string, src: string})
  | Func({name: string, f: template<'a>})
  | StringFunc({name: string, src: string, f: stringFunc<'a>})

let string: (~name: string, string) => t<'a>

let func: (~name: string, template<'a>) => t<'a>

let funcWithString: (~name: string, string, stringFunc<'a>) => t<'a>

let name: t<_> => string
