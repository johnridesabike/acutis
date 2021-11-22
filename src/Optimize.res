/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

module T = Acutis_Types
module List = Belt.List
module Array = Belt.Array
module MapString = Belt.Map.String
module MutMapString = Belt.MutableMap.String
module Queue = Belt.MutableQueue
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
    | OMatch(loc, NonEmpty.t<TypeChecker.Pattern.t>, Matching.t<nodes<'a>>)
    | OMapList(loc, TypeChecker.Pattern.t, Matching.t<nodes<'a>>)
    | OMapDict(loc, TypeChecker.Pattern.t, Matching.t<nodes<'a>>)
    | OComponent({
        loc: loc,
        name: string,
        props: array<(string, TypeChecker.Pattern.t)>,
        children: array<(string, child<'a>)>,
        f: 'a,
      })
  and nodes<'a> = array<node<'a>>
  and child<'a> = OChildName(string) | OChildBlock(nodes<'a>)
  type t<'a> = {
    nodes: nodes<'a>,
    name: string,
    propTypes: MapString.t<TypeChecker.t>,
  }

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
      | TypeChecker.Ast.TText(s, NoTrim) => OText(s)
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

  let make = (ast: TypeChecker.Ast.t<_>) => {
    {
      nodes: nodes(ast.nodes, ~name=ast.name),
      name: ast.name,
      propTypes: ast.propTypes,
    }
  }
}

type rec ast<'a> = Ast.t<componentTemplateU<'a>>
and template<'a> = (environment<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a
and componentTemplateU<'a> = (. environment<'a>, Js.Dict.t<Props.t>, Js.Dict.t<'a>) => 'a
and environment<'a> = {
  renderRoot: (. ast<'a>, Js.Dict.t<Js.Json.t>, Js.Dict.t<'a>) => 'a,
  render: (. ast<'a>, Js.Dict.t<Props.t>, Js.Dict.t<'a>) => 'a,
  return: (. string) => 'a,
  error: (. string) => 'a,
  mapChild: (. 'a, string => string) => 'a,
  flatMapChild: (. 'a, string => 'a) => 'a,
}

type componentTemplate<'a> = (environment<'a>, Js.Dict.t<Props.t>, Js.Dict.t<'a>) => 'a

type notlinked<'a> = Ast(Ast.t<unit>) | Fun({name: string, f: componentTemplate<'a>})

let name = (Ast({name, _}) | Fun({name, _})) => name

@raises(Exit)
let compileExn = (~name, src) => {
  let nodes = Compile.makeAstInternalExn(~name, src)
  {T.Ast.name: name, nodes: nodes}
}

let compileArrayExn = a => {
  let components = Array.mapU(a, (. (name, src)) => (name, compileExn(~name, src).nodes))
  let components = TypeChecker.makeArray(components)
  let components = MutMapString.mapU(components, (. ast) => Ast(Ast.make(ast)))
  components
}

let compile = (~name, src, components) =>
  try {
    let ast = compileExn(~name, src)
    let components = Array.mapU(components, (. (name, src)) => (name, compileExn(~name, src).nodes))
    let (ast, components) = TypeChecker.make(name, ast.nodes, components)
    let components = MutMapString.mapU(components, (. ast) => Ast.make(ast))
    let ast = Ast.make(ast)
    #ok((ast, components))
  } catch {
  | Exit(e) => #errors([e])
  | e => #errors([Debug.uncaughtCompileError(e, ~name)])
  }

module Components = {
  let stringEq = (. a: string, b: string) => a == b

  // this makes components render faster.
  let uncurry = (f, . a, b, c) => f(a, b, c)

  // Mutable structures have the advantage of being able to update even when
  // the linker exits early via raising an exception.
  type graph<'a> = {
    linked: MutMapString.t<componentTemplateU<'a>>,
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
        let f = uncurry(linkComponentsExn(ast, {...g, stack: list{name, ...g.stack}}))
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
        OMatch(
          l,
          b,
          {
            ...t,
            exits: Array.mapU(t.exits, (. n) => mapNodesExn(n, graph)),
          },
        )
      | OMapList(l, p, t) =>
        OMapList(
          l,
          p,
          {
            ...t,
            exits: Array.mapU(t.exits, (. n) => mapNodesExn(n, graph)),
          },
        )
      | OMapDict(l, p, t) =>
        OMapDict(
          l,
          p,
          {
            ...t,
            exits: Array.mapU(t.exits, (. n) => mapNodesExn(n, graph)),
          },
        )
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
    | Ast(ast) =>
      let ast: Ast.t<_> = {...ast, nodes: mapNodesExn(ast.nodes, graph)}
      (env, props, children) => env.render(. ast, props, children)
    | Fun({f, _}) => f
    }

  let linkComponents = (src, graph) =>
    try {
      #ok(linkComponentsExn(src, graph))
    } catch {
    | Exit(e) => #errors([e])
    | e => #errors([Debug.uncaughtCompileError(e, ~name=name(src))])
    }

  type t<'a> = MutMapString.t<T.templateU<'a>>

  let empty = () => MutMapString.make()

  let make = a => {
    let errors = Queue.make()
    let linked = MutMapString.make()
    let notlinked = compileArrayExn(a)
    // Link each AST in a way that ensures the output graph has no cycles.
    let rec aux = () =>
      switch MutMapString.minimum(notlinked) {
      | Some((name, ast)) =>
        MutMapString.remove(notlinked, name)
        let g = {linked: linked, notlinked: notlinked, stack: list{name}}
        switch linkComponents(ast, g) {
        | #ok(f) => MutMapString.set(linked, name, uncurry(f))
        | #errors(e) => e->Queue.fromArray->Queue.transfer(errors)
        }
        aux()
      | None =>
        if Queue.isEmpty(errors) {
          #ok(linked)
        } else {
          #errors(Queue.toArray(errors))
        }
      }
    aux()
  }
}

let linkRoot = (ast: Ast.t<_>, graph) =>
  try {
    let ast = {...ast, nodes: Components.mapNodesExn(ast.nodes, graph)}
    #ok((env, props, children) => env.renderRoot(. ast, props, children))
  } catch {
  | Exit(e) => #errors([e])
  | e => #errors([Debug.uncaughtCompileError(e, ~name=ast.name)])
  }

let make = (~name, src, components) =>
  compile(~name, src, components)->Result.flatMapU((. (root, components)) =>
    linkRoot(
      root,
      {
        linked: MutMapString.make(),
        notlinked: MutMapString.mapU(components, (. x) => Ast(x)),
        stack: list{name},
      },
    )
  )
