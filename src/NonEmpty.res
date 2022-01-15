/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Array = Belt.Array
module Queue = Belt.MutableQueue

type t<'a> = array<'a>

let fromQueue = (hd, tl) => {
  let q = Queue.make()
  Queue.add(q, hd)
  Queue.transfer(tl, q)
  Queue.toArray(q)
}

let toArray = a => a

let size = Array.size

let hd = a => Array.getUnsafe(a, 0)

let one = x => [x]

let two = (x, y) => [x, y]

@raises(Invalid_argument)
let fromArrayExn = a =>
  switch a {
  | [] => raise(Invalid_argument("NonEmpty.fromArrayExn"))
  | a => a
  }

let get = Array.get

let map = Array.mapU

let unzip = Array.unzip

let zip = Array.zip

let zipBy = Array.zipByU
