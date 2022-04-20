/**
  Copyright (c) 2022 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Array = Belt.Array
module List = Belt.List
module HashmapString = Belt.HashMap.String
exception Exit = Debug.Exit

type escape = NoEscape | Escape

let escape = c =>
  switch c {
  | "&" => "&amp;"
  | "\"" => "&quot;"
  | "'" => "&apos;"
  | ">" => "&gt;"
  | "<" => "&lt;"
  | "/" => "&#x2F;"
  | "`" => "&#x60;"
  | "=" => "&#x3D;"
  | c => c
  }

let rec escapeAux = (str, pos, result) =>
  switch Js.String2.charAt(str, pos) {
  | "" => result
  | c => escapeAux(str, succ(pos), result ++ escape(c))
  }

let escape = (esc, str) =>
  switch esc {
  | Escape => escapeAux(str, 0, "")
  | NoEscape => str
  }

module Dagmap = {
  let string_equal = (. a: string, b: string) => a == b

  type map<'a> = HashmapString.t<'a>

  // Mutable structures have the advantage of being able to update even when
  // the linker exits early via raising an exception.
  type rec t<'a, 'b> = {
    queue: array<string>,
    notlinked: map<'a>,
    linked: map<'b>,
    stack: list<string>,
    f: (. t<'a, 'b>, 'a) => 'b,
  }

  let id = (. _, a) => a

  let make = (m, ~f) => {
    {
      queue: HashmapString.keysToArray(m),
      notlinked: HashmapString.copy(m),
      linked: HashmapString.make(~hintSize=HashmapString.size(m)),
      stack: list{},
      f: f,
    }
  }

  let prelinked = m => {
    queue: [],
    notlinked: HashmapString.make(~hintSize=0),
    linked: m,
    stack: list{},
    f: id,
  }

  // When we link components in the tree, ensure that it keeps the
  // directed-acyclic structure.
  @raises(Exit)
  let get = (g, key, debug) =>
    switch HashmapString.get(g.linked, key) {
    | Some(x) => x // It was linked already during a previous search.
    | None =>
      let stack = list{key, ...g.stack}
      switch HashmapString.get(g.notlinked, key) {
      | Some(x) =>
        // Remove it from the unlinked map so a cycle isn't possible.
        HashmapString.remove(g.notlinked, key)
        let x = g.f(. {...g, stack: stack}, x)
        HashmapString.set(g.linked, key, x)
        x
      | None =>
        // It is either being linked (thus in a cycle) or it doesn't exist.
        if List.hasU(g.stack, key, string_equal) {
          raise(Exit(Debug.cyclicDependency(debug, ~stack)))
        } else {
          raise(Exit(Debug.missingComponent(debug, key)))
        }
      }
    }

  let linkAll = g => {
    Array.forEachU(g.queue, (. key) =>
      switch HashmapString.get(g.notlinked, key) {
      | Some(v) =>
        HashmapString.remove(g.notlinked, key)
        let v = g.f(. {...g, stack: list{key, ...g.stack}}, v)
        HashmapString.set(g.linked, key, v)
      | None => () // It was already processed by a dependent.
      }
    )
    g.linked
  }
}
