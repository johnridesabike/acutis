open Debug
module T = Acutis_Types
module Int = Belt.Int

let location = (T.Loc(x)) => {character: x + 1}

let childTypeMismatch = (a, b, ~f) => {
  message: `This is type ${f(a)}} but expected type ${f(b)}.`,
  kind: #Type,
  exn: None,
  location: None,
  path: [],
}

let typeMismatch = (a, b, ~f, ~loc) => {
  message: `This is type ${f(a)} but expected type ${f(b)}.`,
  kind: #Type,
  exn: None,
  location: Some(location(loc)),
  path: [],
}

let tupleSizeMismatch = (a, b) => {
  message: `This is a ${Int.toString(a)}-tuple but expected a ${Int.toString(b)}-tuple.`,
  kind: #Type,
  exn: None,
  location: None,
  path: [],
}

let mapPatternSizeMismatch = (~loc) => {
  message: `Map blocks can only have two patterns per "with" clause: the item and the index.`,
  kind: #Type,
  exn: None,
  location: Some(location(loc)),
  path: [],
}

let nonNullableEchoLiteral = () => {
  message: `String, int, or float literals are not nullable and therefore allowed before a ? operator.`,
  kind: #Type,
  exn: None,
  location: None,
  path: [],
}
