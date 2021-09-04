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

let getExn = Array.getExn

let map = Array.mapU

let zip = Array.zip

let zipBy = Array.zipByU

let reduce = Array.reduceU
