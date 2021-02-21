/* We're using a polymorphic variant because it has a nicer JS representation. */
type t<'a> = [#ok('a) | #errors(array<Acutis_Types.Errors.t>)]

let mapU = (x: t<_>, f) =>
  switch x {
  | #ok(x) => #ok(f(. x))
  | #errors(_) as e => e
  }

let map = (x, f) => mapU(x, (. x) => f(x))

let flatMapU = (x: t<_>, f) =>
  switch x {
  | #ok(x) => f(. x)
  | #errors(_) as e => e
  }

let flatMap = (x, f) => flatMapU(x, (. x) => f(x))

let getExn = (x: t<_>) =>
  switch x {
  | #ok(x) => x
  | #errors(_) => raise(Not_found)
  }

let getOrElseU = (x: t<_>, onError) =>
  switch x {
  | #ok(x) => x
  | #errors(e) => onError(. e)
  }

let getOrElse = (x, f) => getOrElseU(x, (. x) => f(x))
