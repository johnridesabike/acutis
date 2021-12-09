/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Array = Belt.Array
module List = Belt.List
module HashmapString = Belt.HashMap.String
exception Exit = Debug.Exit

@unboxed
type loc = Acutis_Types.loc = Loc(int)

module Dag = {
  let string_equal = (. a: string, b: string) => a == b

  // Mutable structures have the advantage of being able to update even when
  // the linker exits early via raising an exception.
  type rec t<'a, 'b> = {
    queue: array<string>,
    notlinked: HashmapString.t<'a>,
    linked: HashmapString.t<'b>,
    stack: list<string>,
    f: (. t<'a, 'b>, 'a) => 'b,
  }

  let make = (a, ~f) => {
    let notlinked = HashmapString.fromArray(a)
    {
      queue: HashmapString.keysToArray(notlinked),
      notlinked: notlinked,
      linked: HashmapString.make(~hintSize=Array.size(a)),
      stack: list{},
      f: f,
    }
  }

  let makePrelinked = (a, ~f) => {
    queue: [],
    notlinked: HashmapString.make(~hintSize=0),
    linked: HashmapString.fromArray(a),
    stack: list{},
    f: f,
  }

  // When we link components in the tree, ensure that it keeps the
  // directed-acyclic structure.
  @raises(Exit)
  let getExn = (g, ~name, ~loc, ~key) =>
    switch HashmapString.get(g.linked, key) {
    | Some(x) => x // It was linked already during a previous search.
    | None =>
      switch HashmapString.get(g.notlinked, key) {
      | Some(x) =>
        // Remove it from the unlinked map so a cycle isn't possible.
        HashmapString.remove(g.notlinked, key)
        let x = g.f(. {...g, stack: list{key, ...g.stack}}, x)
        HashmapString.set(g.linked, key, x)
        x
      | None =>
        // It is either being linked (thus in a cycle) or it doesn't exist.
        if List.hasU(g.stack, key, string_equal) {
          raise(Exit(Debug.cyclicDependency(~loc, ~name=key, ~stack=g.stack)))
        } else {
          raise(Exit(Debug2.missingComponent(~name, ~loc, key)))
        }
      }
    }

  let link = g => {
    Array.forEachU(g.queue, (. key) =>
      switch HashmapString.get(g.notlinked, key) {
      | Some(v) =>
        HashmapString.remove(g.notlinked, key)
        let v = g.f(. {...g, stack: list{key, ...g.stack}}, v)
        HashmapString.set(g.linked, key, v)
      | None => () // It was already processed by a dependent.
      }
    )
    HashmapString.toArray(g.linked)
  }
}
