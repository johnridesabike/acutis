/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module Array = Belt.Array
module HashmapString = Belt.HashMap.String
module MapString = Belt.Map.String
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

type echo =
  | OEBinding(string, Utils.escape)
  | OEChild(string)
  // All constants are pre-compiled into strings.
  | OEString(string)

type rec node<'a> =
  // Trimming is optimized away
  | OText(string)
  // The first echo item that isn't null will be returned.
  | OEcho(array<echo>, echo)
  // Case matrices are optimized into decision trees
  | OMatch(NonEmpty.t<Typechecker.Pattern.t>, Matching.t<nodes<'a>>)
  | OMapList(Typechecker.Pattern.t, Matching.t<nodes<'a>>)
  | OMapDict(Typechecker.Pattern.t, Matching.t<nodes<'a>>)
  | OComponent(Debug.t, 'a, MapString.t<Typechecker.Pattern.t>, MapString.t<child<'a>>)

and child<'a> = OChildName(string) | OChildBlock(nodes<'a>)

and nodes<'a> = array<node<'a>>

let echo = (. x) =>
  switch x {
  | Parser.EBinding(_, s, e) => OEBinding(s, e)
  | EChild(_, s) => OEChild(s)
  | EString(_, s, e) => OEString(Utils.escape(e, s))
  | EInt(_, i, e) => OEString(Utils.escape(e, Belt.Int.toString(i)))
  | EFloat(_, i, e) => OEString(Utils.escape(e, Belt.Float.toString(i)))
  }

let rec nodes = a =>
  Array.mapU(a, (. x) =>
    switch x {
    | Typechecker.TText(s, NoTrim) => OText(s)
    | TText(s, TrimStart) => OText(trimStart(s))
    | TText(s, TrimEnd) => OText(trimEnd(s))
    | TText(s, TrimBoth) => OText(trimStart(trimEnd(s)))
    | TEcho(_, nullables, default) => OEcho(Array.mapU(nullables, echo), echo(. default))
    | TMatch(debug, pats, cases) =>
      let tree = Matching.make(cases)
      Matching.partial_match_check(tree.tree, debug)
      let tree = {...tree, exits: Matching.Exit.map(tree.exits, ~f=(. x) => nodes(x))}
      OMatch(pats, tree)
    | TMapList(debug, pat, cases) =>
      let tree = Matching.make(cases)
      Matching.partial_match_check(tree.tree, debug)
      let tree = {...tree, exits: Matching.Exit.map(tree.exits, ~f=(. x) => nodes(x))}
      OMapList(pat, tree)
    | TMapDict(debug, pat, cases) =>
      let tree = Matching.make(cases)
      Matching.partial_match_check(tree.tree, debug)
      let tree = {...tree, exits: Matching.Exit.map(tree.exits, ~f=(. x) => nodes(x))}
      OMapDict(pat, tree)
    | TComponent(debug, name, props, children) =>
      let children = MapString.mapU(children, (. v) =>
        switch v {
        | TChildName(s) => OChildName(s)
        | TChildBlock(n) => OChildBlock(nodes(n))
        }
      )
      OComponent(debug, name, props, children)
    }
  )

let makeNodes = (ast: Typechecker.t) => nodes(ast.nodes)

type t<'a> = {
  prop_types: Typescheme.props,
  nodes: nodes<'a>,
  name: string,
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

  @raises(Exit)
  let makeExn = a => {
    let size = Array.size(a)
    let m = HashmapString.make(~hintSize=size)
    Array.forEachU(a, (. src) =>
      switch src {
      | Source.Acutis(name, src) =>
        if HashmapString.has(m, name) {
          raise(Exit(Debug.duplicateCompName(name)))
        }
        HashmapString.set(m, name, Source.src(~name, Parser.make(Lexer.make(~name, src))))
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
        HashmapString.set(optimized, name, Source.src(~name, makeNodes(src)))
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
    | OMatch(b, t) =>
      let exits = Matching.Exit.map(t.exits, ~f=(. n) => linkNodesExn(n, graph))
      OMatch(b, {...t, exits: exits})
    | OMapList(p, t) =>
      let exits = Matching.Exit.map(t.exits, ~f=(. n) => linkNodesExn(n, graph))
      OMapList(p, {...t, exits: exits})
    | OMapDict(p, t) =>
      let exits = Matching.Exit.map(t.exits, ~f=(. n) => linkNodesExn(n, graph))
      OMapDict(p, {...t, exits: exits})
    | OComponent(debug, name, props, children) =>
      let val = Utils.Dagmap.get(graph, name, debug)
      let children = MapString.mapU(children, (. child) =>
        switch child {
        | OChildName(_) as child => child
        | OChildBlock(nodes) => OChildBlock(linkNodesExn(nodes, graph))
        }
      )
      OComponent(debug, val, props, children)
    }
  )

@raises(Exit)
let linkSrc = (. g, src) =>
  switch src {
  | Source.Acutis(name, ast) => Acutis(name, linkNodesExn(ast, g))
  | Function(name, props, _, f) => Function(name, props, f)
  }

let make = (~name, src, components: Components.t<_>) =>
  try {
    let nodes = Parser.make(Lexer.make(~name, src))
    let ast = Typechecker.make(nodes, components.typed)
    let g = Utils.Dagmap.make(components.optimized, ~f=linkSrc)
    let nodes = makeNodes(ast)
    #ok({
      prop_types: ast.Typechecker.prop_types,
      nodes: linkNodesExn(nodes, g),
      name: name,
    })
  } catch {
  | Exit(e) => #errors([e])
  | e => #errors([Debug.uncaughtCompileError(e, ~name)])
  }
