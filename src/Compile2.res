/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module T = Acutis_Types
module List = Belt.List
module Array = Belt.Array
module MutMapString = Belt.MutableMap.String
exception Exit = Debug.Exit

let trimStart = string => {
  let rec aux = pos =>
    switch Js.String2.charAt(string, pos) {
    | " " | "\t" | "\r" | "\n" => aux(succ(pos))
    | _ => Js.String2.sliceToEnd(string, ~from=pos)
    }
  aux(0)
}

let trimEnd = string => {
  let rec aux = pos =>
    switch Js.String2.charAt(string, pred(pos)) {
    | " " | "\t" | "\r" | "\n" => aux(pred(pos))
    | _ => Js.String2.slice(string, ~from=0, ~to_=pos)
    }
  aux(Js.String2.length(string))
}

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
  | T.Ast.Echo.Escape => escapeAux(str, 0, "")
  | NoEscape => str
  }

module Ast = {
  type loc = T.loc
  module Echo = {
    type escape = T.Ast.Echo.escape = NoEscape | Escape
    type t =
      | Binding(loc, string, escape)
      | Child(loc, string)
      // All constants are pre-compiled into strings.
      | String(loc, string)
  }
  type rec node<'a> =
    // Trimming is optimized away
    | OText(string)
    // The first echo item that isn't null will be returned.
    | OEcho({loc: loc, nullables: array<Echo.t>, default: Echo.t})
    // Case matrices are optimized into decision trees
    | OMatch(loc, NonEmpty.t<Typechecker.Pattern.t>, Matching.t<t<'a>>)
    | OMapList(loc, Typechecker.Pattern.t, Matching.t<t<'a>>)
    | OMapDict(loc, Typechecker.Pattern.t, Matching.t<t<'a>>)
    | OComponent({
        loc: loc,
        name: string,
        props: array<(string, Typechecker.Pattern.t)>,
        children: array<(string, child<'a>)>,
        f: 'a,
      })
  and child<'a> = OChildName(string) | OChildBlock(t<'a>)
  and t<'a> = array<node<'a>>

  let echo = (. x) =>
    switch x {
    | T.Ast.Echo.Binding(l, s, e) => Echo.Binding(l, s, e)
    | Child(l, s) => Child(l, s)
    | String(l, s, e) => String(l, escape(e, s))
    | Int(l, i, e) => String(l, escape(e, Belt.Int.toString(i)))
    | Float(l, i, e) => String(l, escape(e, Belt.Float.toString(i)))
    }

  let rec nodes = (~name, a) =>
    Array.mapU(a, (. x) =>
      switch x {
      | Typechecker.Ast.TText(s, NoTrim) => OText(s)
      | TText(s, TrimStart) => OText(trimStart(s))
      | TText(s, TrimEnd) => OText(trimEnd(s))
      | TText(s, TrimBoth) => OText(trimStart(trimEnd(s)))
      | TEcho({loc, nullables, default}) =>
        OEcho({
          loc: loc,
          nullables: Array.mapU(nullables, echo),
          default: echo(. default),
        })
      | TMatch(loc, pats, cases) =>
        switch Matching.make(cases, ~loc, ~name) {
        | Ok(tree) =>
          switch Matching.ParMatch.check(tree.tree) {
          | Ok(_) => ()
          | Error(e) => raise(Exit(e))
          }
          let tree = {...tree, exits: Array.map(tree.exits, nodes(~name))}
          OMatch(loc, pats, tree)
        | Error(e) => raise(Exit(e))
        }
      | TMapList(loc, pat, cases) =>
        switch Matching.make(cases, ~loc, ~name) {
        | Ok(tree) =>
          switch Matching.ParMatch.check(tree.tree) {
          | Ok(_) => ()
          | Error(e) => raise(Exit(e))
          }
          let tree = {...tree, exits: Array.map(tree.exits, nodes(~name))}
          OMapList(loc, pat, tree)
        | Error(e) => raise(Exit(e))
        }
      | TMapDict(loc, pat, cases) =>
        switch Matching.make(cases, ~loc, ~name) {
        | Ok(tree) =>
          switch Matching.ParMatch.check(tree.tree) {
          | Ok(_) => ()
          | Error(e) => raise(Exit(e))
          }
          let tree = {...tree, exits: Array.map(tree.exits, nodes(~name))}
          OMapDict(loc, pat, tree)
        | Error(e) => raise(Exit(e))
        }
      | TComponent({loc, name, props, children, f: ()}) =>
        let children = Array.mapU(children, (. (k, v)) =>
          switch v {
          | TChildName(s) => (k, OChildName(s))
          | TChildBlock(n) => (k, OChildBlock(nodes(n, ~name)))
          }
        )
        OComponent({
          loc: loc,
          name: name,
          props: props,
          children: children,
          f: (),
        })
      }
    )

  let make = (~name, ast: Typechecker.Ast.t<_>) => nodes(ast.nodes, ~name)
}

type t<'a> = {
  prop_types: Source2.TypeScheme.props,
  child_types: Source2.TypeScheme.Child.props,
  nodes: Ast.t<'a>,
  name: string,
}

let make = (~name, ast) => {
  prop_types: ast.Typechecker.Ast.prop_types,
  child_types: ast.child_types,
  nodes: Ast.make(~name, ast),
  name: name,
}

type rec template<'a> =
  | Acutis(Ast.t<template<'a>>)
  | Function(Source2.TypeScheme.props, Source2.fnU<'a>)

type notlinked<'a> = Source2.t<Ast.t<unit>, Source2.fnU<'a>>

@raises(Exit)
let compileExn = (~name, src) => {
  let nodes = Compile.makeAstInternalExn(~name, src)
  {T.Ast.name: name, nodes: nodes}
}

let compile = (~name, src, components) =>
  try {
    let {nodes, _} = compileExn(~name, src)
    let ast = Typechecker.make(name, nodes, components, ~stack=list{})
    #ok(make(~name, ast))
  } catch {
  | Exit(e) => #errors([e])
  | e => #errors([Debug.uncaughtCompileError(e, ~name)])
  }

module Components = {
  type t<'a> = MutMapString.t<Source2.t<Typechecker.Ast.t<unit>, Source2.fnU<'a>>>

  let empty = () => MutMapString.make()

  let makeExn = a => {
    let components = Array.mapU(a, (. src) =>
      switch src {
      | Source2.Acutis(name, src) => (name, Source2.src(~name, compileExn(~name, src).nodes))
      | Function(name, p, c, f) => (name, Source2.functionU(~name, p, c, f))
      }
    )
    Typechecker.makeArray(components)
  }

  let make = a =>
    try {
      #ok(makeExn(a))
    } catch {
    | Exit(e) => #errors([e])
    | e => #errors([Debug.uncaughtCompileError(e, ~name="")])
    }

  let optimize = m =>
    MutMapString.mapU(m, (. x) =>
      switch x {
      | Source2.Acutis(name, ast) => Source2.src(~name, Ast.make(~name, ast))
      | Function(name, p, c, f) => Source2.functionU(~name, p, c, f)
      }
    )
}

module Linker = {
  let stringEq = (. a: string, b: string) => a == b

  // Mutable structures have the advantage of being able to update even when
  // the linker exits early via raising an exception.
  type t<'a> = {
    linked: MutMapString.t<template<'a>>,
    notlinked: MutMapString.t<notlinked<'a>>,
    stack: list<string>,
  }

  // When we link components in the tree, ensure that it keeps the
  // directed-acyclic structure.
  @raises(Exit)
  let rec getComponentExn = (g, name, loc) =>
    switch MutMapString.get(g.linked, name) {
    | Some(f) => f // It was linked already during a previous search.
    | None =>
      switch MutMapString.get(g.notlinked, name) {
      | Some(ast) =>
        // Remove it from the unlinked map so a cycle isn't possible.
        MutMapString.remove(g.notlinked, name)
        let f = linkComponentsExn(ast, {...g, stack: list{name, ...g.stack}})
        MutMapString.set(g.linked, name, f)
        f
      | None =>
        // It is either being linked (thus in a cycle) or it doesn't exist.
        if List.hasU(g.stack, name, stringEq) {
          raise(Exit(Debug.cyclicDependency(~loc, ~name, ~stack=g.stack)))
        } else {
          raise(Exit(Debug.componentDoesNotExist(~loc, ~name, ~stack=g.stack)))
        }
      }
    }
  // Recursively map the nodes to link the components.
  @raises(Exit)
  and mapNodesExn = (nodes, graph) =>
    Array.mapU(nodes, (. node: Ast.node<_>) =>
      switch node {
      | (OText(_) | OEcho(_)) as x => x
      | OMatch(l, b, t) =>
        let exits = Array.mapU(t.exits, (. n) => mapNodesExn(n, graph))
        OMatch(l, b, {...t, exits: exits})
      | OMapList(l, p, t) =>
        let exits = Array.mapU(t.exits, (. n) => mapNodesExn(n, graph))
        OMapList(l, p, {...t, exits: exits})
      | OMapDict(l, p, t) =>
        let exits = Array.mapU(t.exits, (. n) => mapNodesExn(n, graph))
        OMapDict(l, p, {...t, exits: exits})
      | OComponent({loc, name, props, children, f: ()}) =>
        OComponent({
          loc: loc,
          name: name,
          props: props,
          children: Array.mapU(children, (. (name, child)) =>
            switch child {
            | OChildName(_) as child => (name, child)
            | OChildBlock(nodes) => (name, OChildBlock(mapNodesExn(nodes, graph)))
            }
          ),
          f: getComponentExn(graph, name, loc),
        })
      }
    )
  @raises(Exit)
  and linkComponentsExn = (src, graph) =>
    switch src {
    | Source2.Acutis(_, ast) => Acutis(mapNodesExn(ast, graph))
    | Function(_, props, _, f) => Function(props, f)
    }

  let make = (~name, typed, components) =>
    try {
      let graph = {
        linked: Components.empty(),
        notlinked: Components.optimize(components),
        stack: list{name},
      }
      #ok({
        ...typed,
        nodes: mapNodesExn(typed.nodes, graph),
      })
    } catch {
    | Exit(e) => #errors([e])
    | e => #errors([Debug.uncaughtCompileError(e, ~name)])
    }
}

let make = (~name, src, components) =>
  compile(~name, src, components)->Result.flatMapU((. root) => Linker.make(~name, root, components))
