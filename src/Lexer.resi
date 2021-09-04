/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

type t

let make: (~name: string, string) => t

let peekExn: t => Acutis_Types.Token.t

let popExn: t => Acutis_Types.Token.t

let name: t => string

let debugToArray: t => array<Acutis_Types.Token.t>
