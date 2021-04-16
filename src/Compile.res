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

/*
  Many of these functions accept both a token and a queue of tokens. The
  token comes from the "head" of the queue. This minimizes the number of
  "peek" operations needed to switch code paths.
*/

module T = Acutis_Types

module Array = Belt.Array
module Ast = T.Ast
module Ast_Pattern = T.Ast_Pattern
module List = Belt.List
module MutMapString = Belt.MutableMap.String
module NonEmpty = T.NonEmpty
module Queue = Belt.MutableQueue
module Token = T.Token

exception Exit = Debug.Exit

type t<'a> = T.template<'a>

module Pattern = {
  @raises(Exit)
  let rec parseNode = (t: Token.t, tokens): Ast_Pattern.t =>
    switch t {
    | Null(loc) => #Null(loc)
    | False(loc) => #False(loc)
    | True(loc) => #True(loc)
    | Identifier(loc, x) => #Binding(loc, x)
    | Number(loc, x) => #Number(loc, x)
    | String(loc, x) => #String(loc, x)
    | OpenParen(loc) =>
      switch Lexer.popExn(tokens) {
      | CloseParen(_) => #Tuple(loc, [])
      | t => parseTuple(loc, t, tokens)
      }
    | OpenBracket(loc) =>
      switch Lexer.popExn(tokens) {
      | CloseBracket(_) => #Array(loc, [])
      | t => parseArray(loc, t, tokens)
      }
    | OpenBrace(loc) =>
      switch Lexer.popExn(tokens) {
      | CloseBrace(_) => #Object(loc, [])
      | t => parseObject(loc, t, tokens)
      }
    | OpenPointyBracket(loc) =>
      switch Lexer.popExn(tokens) {
      | ClosePoointyBracket(_) => #Dict(loc, [])
      | t => parseDict(loc, t, tokens)
      }
    | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
    }
  @raises(Exit)
  and parseArray = (loc, t, tokens) => {
    let q = Queue.make()
    Queue.add(q, parseNode(t, tokens))

    @raises(Exit)
    let rec aux = (): Ast_Pattern.t =>
      switch Lexer.popExn(tokens) {
      | CloseBracket(_) => #Array(loc, Queue.toArray(q))
      | Comma(_) =>
        switch Lexer.popExn(tokens) {
        | Spread(_) =>
          switch Lexer.popExn(tokens) {
          | Identifier(bindingLoc, tailBinding) =>
            switch Lexer.popExn(tokens) {
            | CloseBracket(_) =>
              #ArrayWithTailBinding(loc, Queue.toArray(q), #Binding(bindingLoc, tailBinding))
            | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
            }
          | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
          }
        | t =>
          Queue.add(q, parseNode(t, tokens))
          aux()
        }
      | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
      }
    aux()
  }
  @raises(Exit)
  and parseTuple = (loc, t, tokens) => {
    let q = Queue.make()
    Queue.add(q, parseNode(t, tokens))

    @raises(Exit)
    let rec aux = (): Ast_Pattern.t =>
      switch Lexer.popExn(tokens) {
      | CloseParen(_) => #Tuple(loc, Queue.toArray(q))
      | Comma(_) =>
        Queue.add(q, parseNode(Lexer.popExn(tokens), tokens))
        aux()
      | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
      }
    aux()
  }
  @raises(Exit)
  and parseObject = (loc, t, tokens) => {
    let q = Queue.make()
    Queue.add(q, parseObjectKeyValue(t, tokens))

    @raises(Exit)
    let rec aux = (): Ast_Pattern.t =>
      switch Lexer.popExn(tokens) {
      | CloseBrace(_) => #Object(loc, Queue.toArray(q))
      | Comma(_) =>
        Queue.add(q, parseObjectKeyValue(Lexer.popExn(tokens), tokens))
        aux()
      | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
      }
    aux()
  }
  @raises(Exit)
  and parseDict = (loc, t, tokens) => {
    let q = Queue.make()
    Queue.add(q, parseObjectKeyValue(t, tokens))

    @raises(Exit)
    let rec aux = (): Ast_Pattern.t =>
      switch Lexer.popExn(tokens) {
      | ClosePoointyBracket(_) => #Dict(loc, Queue.toArray(q))
      | Comma(_) =>
        Queue.add(q, parseObjectKeyValue(Lexer.popExn(tokens), tokens))
        aux()
      | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
      }
    aux()
  }
  @raises(Exit)
  and parseObjectKeyValue = (t, tokens) =>
    switch t {
    | String(_, key) =>
      switch Lexer.popExn(tokens) {
      | Colon(_) => (key, parseNode(Lexer.popExn(tokens), tokens))
      | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
      }
    | Identifier(loc, key) =>
      switch Lexer.peekExn(tokens) {
      | Colon(_) =>
        Lexer.popExn(tokens)->ignore
        (key, parseNode(Lexer.popExn(tokens), tokens))
      | _ => (key, #Binding(loc, key))
      }
    | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
    }

  @raises(Exit)
  let make = tokens => {
    let head = parseNode(Lexer.popExn(tokens), tokens)
    let q = Queue.make()

    @raises(Exit)
    let rec aux = (): NonEmpty.t<_> => {
      switch Lexer.peekExn(tokens) {
      | Comma(_) =>
        Lexer.popExn(tokens)->ignore
        Queue.add(q, parseNode(Lexer.popExn(tokens), tokens))
        aux()
      | _ => NonEmpty(head, Queue.toArray(q))
      }
    }
    aux()
  }
}

type parseData<'a> = {
  nextToken: Token.t,
  data: 'a,
}

@raises(Exit)
let parseBindingName = tokens =>
  switch Lexer.popExn(tokens) {
  | Identifier(loc, x) => (loc, x)
  | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
  }

@raises(Exit)
let parseCommaSequence = tokens => {
  let head = parseBindingName(tokens)
  let q = Queue.make()

  @raises(Exit)
  let rec aux = (): NonEmpty.t<_> =>
    switch Lexer.peekExn(tokens) {
    | Comma(_) =>
      Lexer.popExn(tokens)->ignore
      Queue.add(q, parseBindingName(tokens))
      aux()
    | _ => NonEmpty(head, Queue.toArray(q))
    }
  aux()
}

@raises(Exit)
let parseEchoAux = (t: Token.t, tokens, esc): Ast.Echo.t =>
  switch t {
  | Identifier(loc, x) => Binding(loc, x, esc)
  | String(_, x) => String(x, esc)
  | Number(_, x) => Number(x, esc)
  | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
  }

@raises(Exit)
let parseEcho = tokens =>
  switch Lexer.popExn(tokens) {
  | Ampersand(_) => parseEchoAux(Lexer.popExn(tokens), tokens, NoEscape)
  | ComponentName(loc, x) => Child(loc, x)
  | t => parseEchoAux(t, tokens, Escape)
  }

@raises(Exit)
let parseEchoes = tokens => {
  let head = parseEcho(tokens)
  let q = Queue.make()

  @raises(Exit)
  let rec aux = (): parseData<NonEmpty.t<_>> =>
    switch Lexer.popExn(tokens) {
    | (Tilde(_) | Text(_)) as t => {nextToken: t, data: NonEmpty(head, Queue.toArray(q))}
    | Question(_) =>
      Queue.add(q, parseEcho(tokens))
      aux()
    | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
    }
  aux()
}

let endOfMatchMap = (. t: Token.t) =>
  switch t {
  | Identifier(_, "with") | Slash(_) => true
  | _ => false
  }

let endOfFile = (. t: Token.t) =>
  switch t {
  | EndOfFile(_) => true
  | _ => false
  }

let slash = (. t: Token.t) =>
  switch t {
  | Slash(_) => true
  | _ => false
  }

@raises(Exit)
let rec parse = (t, tokens, ~until) => {
  let q: Queue.t<Ast.node<_>> = Queue.make()

  @raises(Exit)
  let rec aux = (t: Token.t) =>
    switch t {
    | t if until(. t) => {nextToken: t, data: Queue.toArray(q)}
    | Text(_, x) =>
      switch Lexer.popExn(tokens) {
      | Tilde(_) =>
        Queue.add(q, Text(x, TrimEnd))
        aux(Lexer.popExn(tokens))
      | t =>
        Queue.add(q, Text(x, NoTrim))
        aux(t)
      }
    | Tilde(_) =>
      switch Lexer.popExn(tokens) {
      | Text(_, x) =>
        switch Lexer.popExn(tokens) {
        | Tilde(_) =>
          Queue.add(q, Text(x, TrimBoth))
          aux(Lexer.popExn(tokens))
        | t =>
          Queue.add(q, Text(x, TrimStart))
          aux(t)
        }
      | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
      }
    | Comment(_) => aux(Lexer.popExn(tokens))
    | Identifier(loc, "match") =>
      let identifiers = parseCommaSequence(tokens)
      let withs = parseWithBlocks(tokens, ~block="match")
      Queue.add(q, Match(loc, identifiers, withs))
      aux(Lexer.popExn(tokens))
    | Identifier(loc, "map") =>
      switch Pattern.parseNode(Lexer.popExn(tokens), tokens) {
      | #...Ast.mapPattern as pattern =>
        let withs = parseWithBlocks(tokens, ~block="map")
        Queue.add(q, Map(loc, pattern, withs))
        aux(Lexer.popExn(tokens))
      | (#Null(_) | #True(_) | #False(_) | #Tuple(_) | #String(_) | #Number(_) | #Object(_)) as x =>
        raise(Exit(Debug.badMapTypeParse(x, ~name=Lexer.name(tokens))))
      }
    | Echo(loc) =>
      let {nextToken, data: echoes} = parseEchoes(tokens)
      Queue.add(q, Echo(loc, echoes))
      aux(nextToken)
    | ComponentName(loc, name) =>
      Queue.add(q, parseComponent(loc, name, tokens))
      aux(Lexer.popExn(tokens))
    | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
    }
  aux(t)
}
@raises(Exit)
and parseWithBlock = tokens => {
  let head = Pattern.make(tokens)
  let q = Queue.make()

  @raises(Exit)
  let rec aux = () =>
    switch Lexer.popExn(tokens) {
    | Identifier(_, "with") =>
      Queue.add(q, Pattern.make(tokens))
      aux()
    | t =>
      let {nextToken, data: nodes} = parse(t, tokens, ~until=endOfMatchMap)
      {nextToken: nextToken, data: {Ast.patterns: NonEmpty(head, Queue.toArray(q)), nodes: nodes}}
    }
  aux()
}
@raises(Exit)
and parseWithBlocks = (tokens, ~block) =>
  switch Lexer.popExn(tokens) {
  | Identifier(_, "with") =>
    let {nextToken, data: head} = parseWithBlock(tokens)
    let q = Queue.make()

    @raises(Exit)
    let rec aux = (t: Token.t): NonEmpty.t<_> =>
      switch t {
      | Slash(_) =>
        switch Lexer.popExn(tokens) {
        | Identifier(_, x) if x == block => NonEmpty(head, Queue.toArray(q))
        | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
        }
      /* This is guaranteed to be a "with" clause. */
      | _with =>
        let {nextToken, data} = parseWithBlock(tokens)
        Queue.add(q, data)
        aux(nextToken)
      }
    aux(nextToken)
  | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
  }
@raises(Exit)
and parseComponent = (loc, name, tokens) => {
  let {nextToken, data: (props, children)} = parseProps(tokens)
  switch nextToken {
  | Slash(_) =>
    Component({
      loc: loc,
      name: name,
      props: Queue.toArray(props),
      children: Queue.toArray(children),
      f: (),
    })
  | t =>
    let {data: child, _} = parse(t, tokens, ~until=slash)
    switch Lexer.popExn(tokens) {
    | ComponentName(_, name') if name == name' =>
      Queue.add(children, ("Children", ChildBlock(child)))
      Component({
        loc: loc,
        name: name,
        props: Queue.toArray(props),
        children: Queue.toArray(children),
        f: (),
      })
    | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
    }
  }
}
@raises(Exit)
and parseProps = tokens => {
  let props = Queue.make()
  let children = Queue.make()

  @raises(Exit)
  let rec aux = (t: Token.t) =>
    switch t {
    | Identifier(loc, key) =>
      switch Lexer.popExn(tokens) {
      | Equals(_) =>
        let prop = Pattern.parseNode(Lexer.popExn(tokens), tokens)
        Queue.add(props, (key, prop))
        aux(Lexer.popExn(tokens))
      | t =>
        Queue.add(props, (key, #Binding(loc, key)))
        aux(t)
      }
    | ComponentName(_, name) =>
      switch Lexer.popExn(tokens) {
      | Equals(_) =>
        switch Lexer.popExn(tokens) {
        | Block(_) =>
          let {data: child, _} = parse(Lexer.popExn(tokens), tokens, ~until=slash)
          switch Lexer.popExn(tokens) {
          | Block(_) =>
            Queue.add(children, (name, Ast.ChildBlock(child)))
            aux(Lexer.popExn(tokens))
          | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
          }
        | ComponentName(_, name') =>
          Queue.add(children, (name, ChildName(name')))
          aux(Lexer.popExn(tokens))
        | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
        }
      | t =>
        Queue.add(children, (name, ChildName(name)))
        aux(t)
      }
    | t => {nextToken: t, data: (props, children)}
    }
  aux(Lexer.popExn(tokens))
}

@raises(Exit)
let makeAstInternalExn = (~name, source) => {
  let tokens = Lexer.make(source, ~name)
  let {data, _} = parse(Lexer.popExn(tokens), tokens, ~until=endOfFile)
  data
}

// This is an intermediary structure that stores the ASTs before they're
// fully linked and turned into template functions.
type notlinked<'a> =
  | Ast({name: string, nodes: Ast.nodes<unit>})
  | Func({name: string, f: T.template<'a>})
  | AstFunc({name: string, nodes: Ast.nodes<unit>, f: Source.stringFunc<'a>})

let name = x =>
  switch x {
  | Ast({name, _}) | Func({name, _}) | AstFunc({name, _}) => name
  }

@raises(Exit)
let compileExn = (src: Source.t<_>) =>
  switch src {
  | String({name, src}) => Ast({name: name, nodes: makeAstInternalExn(~name, src)})
  | Func({name, f}) => Func({name: name, f: f})
  | StringFunc({name, src, f}) =>
    AstFunc({
      name: name,
      nodes: makeAstInternalExn(~name, src),
      f: f,
    })
  }

let compile = src =>
  try {
    #ok(compileExn(src))
  } catch {
  | Exit(e) => #errors([e])
  | e => #errors([Debug.uncaughtCompileError(e, ~name=Source.name(src))])
  }

module Components = {
  let stringEq = (. a: string, b: string) => a == b

  // this makes components render faster.
  let uncurry = (f, . a, b, c) => f(a, b, c)

  // Mutable structures have the advantage of being able to update even when
  // the linker exits early via raising an exception.
  type graph<'a> = {
    linked: MutMapString.t<T.templateU<'a>>,
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
      | (Text(_) | Echo(_)) as x => x
      | Match(l, b, cases) =>
        Match(
          l,
          b,
          NonEmpty.map(cases, ~f=(. {patterns, nodes}): T.Ast.case<_> => {
            patterns: patterns,
            nodes: mapNodesExn(nodes, graph),
          }),
        )
      | Map(l, p, cases) =>
        Map(
          l,
          p,
          NonEmpty.map(cases, ~f=(. {patterns, nodes}): T.Ast.case<_> => {
            patterns: patterns,
            nodes: mapNodesExn(nodes, graph),
          }),
        )
      | Component({loc, name, props, children, f: ()}) =>
        Component({
          loc: loc,
          name: name,
          props: props,
          children: Array.mapU(children, (. (name, child)) =>
            switch child {
            | ChildName(_) as child => (name, child)
            | ChildBlock(nodes) => (name, ChildBlock(mapNodesExn(nodes, graph)))
            }
          ),
          f: getComponentExn(graph, name, loc),
        })
      }
    )
  @raises(Exit)
  and linkComponentsExn = (src, graph) =>
    switch src {
    | Ast({name, nodes}) =>
      let ast: Ast.t<_> = {name: name, nodes: mapNodesExn(nodes, graph)}
      (env: T.environment<_>, props, children) => env.render(. ast, props, children)
    | AstFunc({name, nodes, f}) =>
      let ast: Ast.t<_> = {name: name, nodes: mapNodesExn(nodes, graph)}
      f(ast)
    | Func({f, _}) => f
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
    let notlinked = MutMapString.make()
    Array.forEachU(a, (. src) => {
      let name = Source.name(src)
      MutMapString.updateU(notlinked, name, (. x) =>
        switch x {
        | None =>
          switch compile(src) {
          | #ok(x) => Some(x)
          | #errors(e) =>
            e->Queue.fromArray->Queue.transfer(errors)
            None
          }
        | Some(_) as x =>
          Queue.add(errors, Debug.duplicateCompName(name))
          x
        }
      )
    })
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

let make = (src, components) =>
  compile(src)->Result.flatMapU((. x) =>
    Components.linkComponents(
      x,
      {
        Components.linked: components,
        notlinked: MutMapString.make(),
        stack: list{Source.name(src)},
      },
    )
  )
