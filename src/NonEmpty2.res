module Array = Belt.Array
module T = Acutis_Types

type t<'a> = array<'a>
let fromOld = (NonEmpty(h, t): T.NonEmpty.t<_>) => Array.concat([h], t)
let toArray = a => a
let fromArrayUnsafe = a => a
let hd = a => Array.getUnsafe(a, 0)
let get = Array.get
let getExn = Array.getExn
let map = Array.mapU
let zip = Array.zip
let zipBy = Array.zipByU
let reduce = Array.reduceU
let reduceHd = (a, f) => {
  let r = ref(hd(a))
  for i in 1 to Array.length(a) - 1 {
    r.contents = f(. r.contents, Array.getUnsafe(a, i))
  }
  r.contents
}
