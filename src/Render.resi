/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

let make: (
  Source.env<'a>,
  Compile.t<Compile.template<'a>>,
  Js.Dict.t<Js.Json.t>,
  Js.Dict.t<'a>,
) => 'a

module Sync: Source.Env with type t = Result.t<string>

let sync: (Compile.t<Compile.template<Sync.t>>, Js.Dict.t<Js.Json.t>, Js.Dict.t<Sync.t>) => Sync.t

module Async: Source.Env with type t = Js.Promise.t<Result.t<string>>

let async: (
  Compile.t<Compile.template<Async.t>>,
  Js.Dict.t<Js.Json.t>,
  Js.Dict.t<Async.t>,
) => Async.t
