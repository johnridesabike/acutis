/**
   Copyright 2021 John Jackson

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

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
