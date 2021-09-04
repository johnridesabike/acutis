/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

type t<'a> = Acutis_Types.environment<'a>

@live
let sync: t<Result.t<string>>

@live
let async: t<Js.Promise.t<Result.t<string>>>
