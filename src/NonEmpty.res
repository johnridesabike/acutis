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

let reduceHd = (a, f) => {
  let r = ref(hd(a))
  for i in 1 to Array.length(a) - 1 {
    r.contents = f(. r.contents, Array.getUnsafe(a, i))
  }
  r.contents
}

let get = Array.get

let map = Array.mapU

let unzip = Array.unzip

let zipExn = (a, b) => {
  assert (Array.size(a) == Array.size(b))
  Array.zip(a, b)
}

let zipByExn = (a, b, f) => {
  assert (Array.size(a) == Array.size(b))
  Array.zipByU(a, b, f)
}
