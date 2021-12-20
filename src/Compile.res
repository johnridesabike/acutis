/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Array = Belt.Array
module HashmapString = Belt.HashMap.String
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

type loc = Debug.loc

type echo =
  | OEBinding(loc, string, Utils.escape)
  | OEChild(loc, string)
  // All constants are pre-compiled into strings.
  | OEString(loc, string)

type rec node<'a> =
  // Trimming is optimized away
  | OText(string)
  // The first echo item that isn't null will be returned.
  | OEcho({loc: loc, nullables: array<echo>, default: echo})
  // Case matrices are optimized into decision trees
  | OMatch(loc, NonEmpty.t<Typechecker.Pattern.t>, Matching.t<nodes<'a>>)
  | OMapList(loc, Typechecker.Pattern.t, Matching.t<nodes<'a>>)
  | OMapDict(loc, Typechecker.Pattern.t, Matching.t<nodes<'a>>)
  | OComponent({
      loc: loc,
      props: array<(string, Typechecker.Pattern.t)>,
      children: array<(string, child<'a>)>,
      val: 'a,
    })
and child<'a> = OChildName(string) | OChildBlock(nodes<'a>)
and nodes<'a> = array<node<'a>>

let echo = (. x) =>
  switch x {
  | Parser.EBinding(l, s, e) => OEBinding(l, s, e)
  | EChild(l, s) => OEChild(l, s)
  | EString(l, s, e) => OEString(l, Utils.escape(e, s))
  | EInt(l, i, e) => OEString(l, Utils.escape(e, Belt.Int.toString(i)))
  | EFloat(l, i, e) => OEString(l, Utils.escape(e, Belt.Float.toString(i)))
  }

let rec nodes = (~name, a) =>
  Array.mapU(a, (. x) =>
    switch x {
    | Typechecker.TText(s, NoTrim) => OText(s)
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
        Matching.ParMatch.check(tree.tree, ~loc)
        let tree = {...tree, exits: Array.map(tree.exits, nodes(~name))}
        OMatch(loc, pats, tree)
      | Error(e) => raise(Exit(e))
      }
    | TMapList(loc, pat, cases) =>
      switch Matching.make(cases, ~loc, ~name) {
      | Ok(tree) =>
        Matching.ParMatch.check(tree.tree, ~loc)
        let tree = {...tree, exits: Array.map(tree.exits, nodes(~name))}
        OMapList(loc, pat, tree)
      | Error(e) => raise(Exit(e))
      }
    | TMapDict(loc, pat, cases) =>
      switch Matching.make(cases, ~loc, ~name) {
      | Ok(tree) =>
        Matching.ParMatch.check(tree.tree, ~loc)
        let tree = {...tree, exits: Array.map(tree.exits, nodes(~name))}
        OMapDict(loc, pat, tree)
      | Error(e) => raise(Exit(e))
      }
    | TComponent({loc, val, props, children}) =>
      let children = Array.mapU(children, (. (k, v)) =>
        switch v {
        | TChildName(s) => (k, OChildName(s))
        | TChildBlock(n) => (k, OChildBlock(nodes(n, ~name)))
        }
      )
      OComponent({loc: loc, val: val, props: props, children: children})
    }
  )

let makeNodes = (~name, ast: Typechecker.t) => nodes(ast.nodes, ~name)

type t<'a> = {
  prop_types: Typescheme.props,
  nodes: nodes<'a>,
  name: string,
}

let make = (~name, ast) => {
  prop_types: ast.Typechecker.prop_types,
  nodes: makeNodes(~name, ast),
  name: name,
}

type rec template<'a> =
  | Acutis(string, nodes<template<'a>>)
  | Function(string, Typescheme.props, Source.fnU<'a>)

module Components = {
  type t<'a> = {
    typed: Utils.Dagmap.map<Source.t<Typechecker.t, Source.fnU<'a>>>,
    optimized: Utils.Dagmap.map<Source.t<nodes<string>, Source.fnU<'a>>>,
  }

  let empty = () => {
    typed: HashmapString.make(~hintSize=0),
    optimized: HashmapString.make(~hintSize=0),
  }

  let makeExn = a => {
    let size = Array.size(a)
    let m = HashmapString.make(~hintSize=size)
    Array.forEachU(a, (. src) =>
      switch src {
      | Source.Acutis(name, src) =>
        if HashmapString.has(m, name) {
          raise(Exit(Debug.duplicateCompName(name)))
        }
        HashmapString.set(m, name, Source.src(~name, Parser.makeExn(~name, src)))
      | Function(name, p, c, f) =>
        if HashmapString.has(m, name) {
          raise(Exit(Debug.duplicateCompName(name)))
        }
        HashmapString.set(m, name, Source.fnU(~name, p, c, f))
      }
    )
    let typed = Typechecker.makeComponents(m)
    let optimized = HashmapString.make(~hintSize=size)
    HashmapString.forEachU(typed, (. _, v) =>
      switch v {
      | Source.Acutis(name, src) =>
        HashmapString.set(optimized, name, Source.src(~name, makeNodes(~name, src)))
      | Function(name, p, c, f) => HashmapString.set(optimized, name, Source.fnU(~name, p, c, f))
      }
    )
    {typed: typed, optimized: optimized}
  }

  let make = a =>
    try {
      #ok(makeExn(a))
    } catch {
    | Exit(e) => #errors([e])
    | e => #errors([Debug.uncaughtCompileError(e, ~name="")])
    }
}

// Recursively map the nodes to link the components.
@raises(Exit)
let rec linkNodesExn = (nodes, graph) =>
  Array.mapU(nodes, (. node) =>
    switch node {
    | (OText(_) | OEcho(_)) as x => x
    | OMatch(l, b, t) =>
      let exits = Array.mapU(t.exits, (. n) => linkNodesExn(n, graph))
      OMatch(l, b, {...t, exits: exits})
    | OMapList(l, p, t) =>
      let exits = Array.mapU(t.exits, (. n) => linkNodesExn(n, graph))
      OMapList(l, p, {...t, exits: exits})
    | OMapDict(l, p, t) =>
      let exits = Array.mapU(t.exits, (. n) => linkNodesExn(n, graph))
      OMapDict(l, p, {...t, exits: exits})
    | OComponent({loc, val, props, children}) =>
      OComponent({
        loc: loc,
        props: props,
        children: Array.mapU(children, (. (name, child)) =>
          switch child {
          | OChildName(_) as child => (name, child)
          | OChildBlock(nodes) => (name, OChildBlock(linkNodesExn(nodes, graph)))
          }
        ),
        val: Utils.Dagmap.getExn(graph, ~name=val, ~key=val, ~loc),
      })
    }
  )

let linkSrc = (. g, src) =>
  switch src {
  | Source.Acutis(name, ast) => Acutis(name, linkNodesExn(ast, g))
  | Function(name, props, _, f) => Function(name, props, f)
  }

let make = (~name, src, components: Components.t<_>) =>
  try {
    let nodes = Parser.makeExn(~name, src)
    let ast = Typechecker.make(name, nodes, components.typed)
    let g = Utils.Dagmap.make(components.optimized, ~f=linkSrc)
    let root = make(~name, ast)
    #ok({...root, nodes: linkNodesExn(root.nodes, g)})
  } catch {
  | Exit(e) => #errors([e])
  | e => #errors([Debug.uncaughtCompileError(e, ~name)])
  }
