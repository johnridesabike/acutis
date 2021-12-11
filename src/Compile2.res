/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module T = Acutis_Types
module Array = Belt.Array
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
  type loc = Debug.loc
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
        props: array<(string, Typechecker.Pattern.t)>,
        children: array<(string, child<'a>)>,
        val: 'a,
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
          switch Matching.ParMatch.check(tree.tree, ~loc) {
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
          switch Matching.ParMatch.check(tree.tree, ~loc) {
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
          switch Matching.ParMatch.check(tree.tree, ~loc) {
          | Ok(_) => ()
          | Error(e) => raise(Exit(e))
          }
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

  let make = (~name, ast: Typechecker.Ast.t) => nodes(ast.nodes, ~name)
}

type t<'a> = {
  prop_types: Typescheme.props,
  child_types: Typescheme.Child.props,
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
  | Acutis(string, Ast.t<template<'a>>)
  | Function(string, Typescheme.props, Source2.fnU<'a>)

@raises(Exit)
let compileExn = (~name, src) => {
  let nodes = Compile.makeAstInternalExn(~name, src)
  {T.Ast.name: name, nodes: nodes}
}

let compile = (~name, src, components) =>
  try {
    let {nodes, _} = compileExn(~name, src)
    let ast = Typechecker.make(name, nodes, components)
    #ok(make(~name, ast))
  } catch {
  | Exit(e) => #errors([e])
  | e => #errors([Debug.uncaughtCompileError(e, ~name)])
  }

module Components = {
  type t<'a> = array<(string, Source2.t<Typechecker.Ast.t, Source2.fnU<'a>>)>

  let empty = () => []

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

  let optimize = x =>
    Array.mapU(x, (. (_, v)) =>
      switch v {
      | Source2.Acutis(name, ast) => (name, Source2.src(~name, Ast.make(~name, ast)))
      | Function(name, p, c, f) => (name, Source2.functionU(~name, p, c, f))
      }
    )
}

// Recursively map the nodes to link the components.
@raises(Exit)
let rec linkNodesExn = (nodes, graph) =>
  Array.mapU(nodes, (. node) =>
    switch node {
    | (Ast.OText(_) | OEcho(_)) as x => x
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
  | Source2.Acutis(name, ast) => Acutis(name, linkNodesExn(ast, g))
  | Function(name, props, _, f) => Function(name, props, f)
  }

let link = (~name, typed, components: Components.t<'a>) =>
  try {
    let g = Utils.Dagmap.make(Components.optimize(components), ~f=linkSrc)
    #ok({...typed, nodes: linkNodesExn(typed.nodes, g)})
  } catch {
  | Exit(e) => #errors([e])
  | e => #errors([Debug.uncaughtCompileError(e, ~name)])
  }

let make = (~name, src, components) =>
  compile(~name, src, components)->Result.flatMapU((. root) => link(~name, root, components))
