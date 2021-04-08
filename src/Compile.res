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
  let rec parseNode = (t: Token.t, tokens): Ast_Pattern.t =>
    switch t {
    | Null(loc) => #Null(loc)
    | False(loc) => #False(loc)
    | True(loc) => #True(loc)
    | Identifier(loc, x) => #Binding(loc, x)
    | Number(loc, x) => #Number(loc, x)
    | String(loc, x) => #String(loc, x)
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
    | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
    }
  and parseArray = (loc, t, tokens) => {
    let q = Queue.make()
    Queue.add(q, parseNode(t, tokens))
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
  and parseObject = (loc, t, tokens) => {
    let q = Queue.make()
    Queue.add(q, parseObjectKeyValue(t, tokens))
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

  let make = tokens => {
    let head = parseNode(Lexer.popExn(tokens), tokens)
    let q = Queue.make()
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

let parseBindingName = tokens =>
  switch Lexer.popExn(tokens) {
  | Identifier(loc, x) => (loc, x)
  | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
  }

let parseCommaSequence = tokens => {
  let head = parseBindingName(tokens)
  let q = Queue.make()
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

let parseEchoAux = (t: Token.t, tokens, esc): Ast.Echo.t =>
  switch t {
  | Identifier(loc, x) => Binding(loc, x, esc)
  | String(_, x) => String(x, esc)
  | Number(_, x) => Number(x, esc)
  | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
  }

let parseEcho = tokens =>
  switch Lexer.popExn(tokens) {
  | Ampersand(_) => parseEchoAux(Lexer.popExn(tokens), tokens, NoEscape)
  | ComponentName(loc, x) => Child(loc, x)
  | t => parseEchoAux(t, tokens, Escape)
  }

let parseEchoes = tokens => {
  let head = parseEcho(tokens)
  let q = Queue.make()
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

let rec parse = (t, tokens, ~until, ~g, ~getComponent) => {
  let q: Queue.t<Ast.node<_>> = Queue.make()
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
      let withs = parseWithBlocks(tokens, ~block="match", ~g, ~getComponent)
      Queue.add(q, Match(loc, identifiers, withs))
      aux(Lexer.popExn(tokens))
    | Identifier(loc, "map") =>
      switch Pattern.parseNode(Lexer.popExn(tokens), tokens) {
      | #...Ast.mapPattern as pattern =>
        let withs = parseWithBlocks(tokens, ~block="map", ~g, ~getComponent)
        Queue.add(q, Map(loc, pattern, withs))
        aux(Lexer.popExn(tokens))
      | (#Null(_) | #True(_) | #False(_) | #String(_) | #Number(_)) as x =>
        raise(Exit(Debug.badMapTypeParse(x, ~name=Lexer.name(tokens))))
      }
    | Echo(loc) =>
      let {nextToken, data: echoes} = parseEchoes(tokens)
      Queue.add(q, Echo(loc, echoes))
      aux(nextToken)
    | ComponentName(loc, name) =>
      Queue.add(q, parseComponent(loc, name, tokens, ~g, ~getComponent))
      aux(Lexer.popExn(tokens))
    | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
    }
  aux(t)
}
and parseWithBlock = (tokens, ~g, ~getComponent) => {
  let head = Pattern.make(tokens)
  let q = Queue.make()
  let rec aux = () =>
    switch Lexer.popExn(tokens) {
    | Identifier(_, "with") =>
      Queue.add(q, Pattern.make(tokens))
      aux()
    | t =>
      let {nextToken, data: ast} = parse(t, tokens, ~until=endOfMatchMap, ~g, ~getComponent)
      {nextToken: nextToken, data: {Ast.patterns: NonEmpty(head, Queue.toArray(q)), ast: ast}}
    }
  aux()
}
and parseWithBlocks = (tokens, ~block, ~g, ~getComponent) =>
  switch Lexer.popExn(tokens) {
  | Identifier(_, "with") =>
    let {nextToken, data: head} = parseWithBlock(tokens, ~g, ~getComponent)
    let q = Queue.make()
    let rec aux = (t: Token.t): NonEmpty.t<_> =>
      switch t {
      | Slash(_) =>
        switch Lexer.popExn(tokens) {
        | Identifier(_, x) if x == block => NonEmpty(head, Queue.toArray(q))
        | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
        }
      /* This is guaranteed to be a "with" clause. */
      | _with =>
        let {nextToken, data} = parseWithBlock(tokens, ~g, ~getComponent)
        Queue.add(q, data)
        aux(nextToken)
      }
    aux(nextToken)
  | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
  }
and parseComponent = (loc, name, tokens, ~g, ~getComponent) => {
  let {nextToken, data: (props, children)} = parseProps(tokens, ~g, ~getComponent)
  switch nextToken {
  | Slash(_) =>
    Component({
      loc: loc,
      name: name,
      props: Queue.toArray(props),
      children: Queue.toArray(children),
      f: getComponent(. g, name, loc),
    })
  | t =>
    let {data: child, _} = parse(t, tokens, ~until=slash, ~g, ~getComponent)
    switch Lexer.popExn(tokens) {
    | ComponentName(_, name') if name == name' =>
      Queue.add(children, ("Children", ChildBlock(child)))
      Component({
        loc: loc,
        name: name,
        props: Queue.toArray(props),
        children: Queue.toArray(children),
        f: getComponent(. g, name, loc),
      })
    | t => raise(Exit(Debug.unexpectedToken(t, ~name=Lexer.name(tokens))))
    }
  }
}
and parseProps = (tokens, ~g, ~getComponent) => {
  let props = Queue.make()
  let children = Queue.make()
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
          let {data: child, _} = parse(
            Lexer.popExn(tokens),
            tokens,
            ~until=slash,
            ~g,
            ~getComponent,
          )
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

let makeAstInternalExn = (~name, ~g, ~getComponent, source): Ast.t<_> => {
  let tokens = Lexer.make(source, ~name)
  let {data, _} = parse(Lexer.popExn(tokens), tokens, ~until=endOfFile, ~g, ~getComponent)
  {ast: data, name: name}
}

let compileExn = (src: Source.t<_>, ~g, ~getComponent) =>
  switch src {
  | String({name, src}) =>
    let ast = makeAstInternalExn(~name, ~g, ~getComponent, src)
    (env: T.environment<_>, props, children) => env.render(. ast, props, children)
  | Func({f, _}) => f
  | StringFunc({name, src, f}) => f(makeAstInternalExn(~name, ~g, ~getComponent, src))
  }

let compile = (src, ~g, ~getComponent) =>
  try {
    #ok(compileExn(~getComponent, ~g, src))
  } catch {
  | Exit(e) => #errors([e])
  | e => #errors([Debug.uncaughtCompileError(e, ~name=Source.name(src))])
  }

module Components = {
  let stringEq = (. a: string, b: string) => a == b

  // this makes components render faster.
  let uncurry = (f, . a, b, c) => f(a, b, c)

  // Mutable structures have the advantage of being able to update even when the
  // compiler exits early via raising an exception.
  type graph<'a> = {
    compiled: MutMapString.t<T.templateU<'a>>,
    srcQueue: MutMapString.t<Source.t<'a>>,
    stack: list<string>,
  }

  // When we compile a new component in the tree, ensure that it keeps the
  // directed-acyclic structure.
  let rec makeComponentGraph = (. g, name, loc) =>
    switch MutMapString.get(g.compiled, name) {
    | Some(f) => f // It was compiled already during a previous search.
    | None =>
      switch MutMapString.get(g.srcQueue, name) {
      | Some(src) =>
        // Remove it from the source queue so a cycle isn't possible.
        MutMapString.remove(g.srcQueue, name)
        let f = compileExn(
          src,
          ~g={...g, stack: list{name, ...g.stack}},
          ~getComponent=makeComponentGraph,
        )
        let result = uncurry(f)
        MutMapString.set(g.compiled, name, result)
        result
      | None =>
        // It is either being compiled (thus in a cycle) or it doesn't exist.
        if List.hasU(g.stack, name, stringEq) {
          raise(Exit(Debug.cyclicDependency(~loc, ~name, ~stack=g.stack)))
        } else {
          raise(Exit(Debug.componentDoesNotExist(~loc, ~name, ~stack=g.stack)))
        }
      }
    }

  type t<'a> = MutMapString.t<T.templateU<'a>>

  let empty = () => MutMapString.make()

  let make = a => {
    let errors = Queue.make()
    let compiled = MutMapString.make()
    let srcQueue = MutMapString.make()
    Array.forEachU(a, (. src) =>
      MutMapString.updateU(srcQueue, Source.name(src), (. x) =>
        switch x {
        | None => Some(src)
        | Some(_) as x =>
          Queue.add(errors, Debug.duplicateCompName(Source.name(src)))
          x
        }
      )
    )
    // Compile each source in a way that ensures the output graph has no cycles.
    let rec aux = () =>
      switch MutMapString.minimum(srcQueue) {
      | Some((name, src)) =>
        MutMapString.remove(srcQueue, name)
        let g = {compiled: compiled, srcQueue: srcQueue, stack: list{name}}
        switch compile(src, ~g, ~getComponent=makeComponentGraph) {
        | #ok(template) => MutMapString.set(compiled, name, uncurry(template))
        | #errors(e) => e->Queue.fromArray->Queue.transfer(errors)
        }
        aux()
      | None =>
        if Queue.isEmpty(errors) {
          #ok(compiled)
        } else {
          #errors(Queue.toArray(errors))
        }
      }
    aux()
  }
}

let make = (src, components) =>
  compile(
    src,
    ~g={
      Components.compiled: components,
      srcQueue: MutMapString.make(),
      stack: list{Source.name(src)},
    },
    ~getComponent=Components.makeComponentGraph,
  )
