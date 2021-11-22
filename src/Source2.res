/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module T = Acutis_Types

type t<'a> =
  | SrcStr({name: string, src: string})
  | SrcFun({name: string, f: T.template<'a>})

let string = (~name, src) => SrcStr({name: name, src: src})

let func = (~name, f) => SrcFun({name: name, f: f})

let name = (SrcStr({name, _}) | SrcFun({name, _})) => name
