/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

/*
  Many of these functions accept both a token and a queue of tokens. The
  token comes from the "head" of the queue. This minimizes the number of
  "peek" operations needed to switch code paths.
*/

module Queue = Belt.MutableQueue
module Token = Lexer.Token

exception Exit = Debug.Exit

module Ast_Pattern = {
  type binding = [#Binding(Debug.loc, string)]
  type arr_<'t> = [
    | #Array(Debug.loc, array<'t>)
    | #ArrayWithTailBinding(Debug.loc, array<'t>, binding)
  ]
  type dict_<'t> = [#Dict(Debug.loc, array<(string, 't)>)]
  type rec t = [
    | #Null(Debug.loc)
    | #Some(Debug.loc, t)
    | #False(Debug.loc)
    | #True(Debug.loc)
    | #String(Debug.loc, string)
    | #Int(Debug.loc, int)
    | #Float(Debug.loc, float)
    | #Tuple(Debug.loc, array<t>)
    | arr_<t>
    | dict_<t>
    | #Object(Debug.loc, array<(string, t)>)
    | binding
  ]
  type arr = arr_<t>
  type dict = dict_<t>

  let rec toString = (x: t) =>
    switch x {
    | #True(_) | #False(_) => "boolean"
    | #Null(_) => "null"
    | #Some(_, x) => toString(x)
    | #String(_) => "string"
    | #Int(_) => "int"
    | #Float(_) => "float"
    | #Tuple(_) => "tuple"
    | #Array(_) | #ArrayWithTailBinding(_) => "array"
    | #Object(_) => "object"
    | #Dict(_) => "dictionary"
    | #Binding(_, x) => `binding: \`${x}\``
    }

  let toLocation = (x: t) =>
    switch x {
    | #True(x)
    | #False(x)
    | #Null(x)
    | #Some(x, _)
    | #String(x, _)
    | #Int(x, _)
    | #Float(x, _)
    | #Tuple(x, _)
    | #Array(x, _)
    | #ArrayWithTailBinding(x, _, _)
    | #Object(x, _)
    | #Dict(x, _)
    | #Binding(x, _) => x
    }
}

module Ast = {
  module Echo = {
    type escape = NoEscape | Escape
    type t =
      | Binding(Debug.loc, string, escape)
      | Child(Debug.loc, string)
      | String(Debug.loc, string, escape)
      | Int(Debug.loc, int, escape)
      | Float(Debug.loc, float, escape)
  }
  type trim = TrimStart | TrimEnd | TrimBoth | NoTrim
  type mapArrayPattern = [Ast_Pattern.binding | Ast_Pattern.arr]
  type mapDictPattern = [Ast_Pattern.binding | Ast_Pattern.dict]
  type rec node<'a> =
    | Text(string, trim)
    // The first echo item that isn't null will be returned.
    | Echo({loc: Debug.loc, nullables: array<Echo.t>, default: Echo.t})
    | Match(Debug.loc, NonEmpty.t<Ast_Pattern.binding>, NonEmpty.t<case<'a>>)
    | MapArray(Debug.loc, mapArrayPattern, NonEmpty.t<case<'a>>)
    | MapDict(Debug.loc, mapDictPattern, NonEmpty.t<case<'a>>)
    | Component({
        loc: Debug.loc,
        name: string,
        props: array<(string, Ast_Pattern.t)>,
        children: array<(string, child<'a>)>,
        f: 'a,
      })
  and nodes<'a> = array<node<'a>>
  and case<'a> = {
    patterns: NonEmpty.t<NonEmpty.t<Ast_Pattern.t>>,
    nodes: nodes<'a>,
  }
  and child<'a> = ChildName(string) | ChildBlock(nodes<'a>)
  type t<'a> = {nodes: nodes<'a>, name: string}
}

module Pattern = {
  @raises(Exit)
  let rec parseNode = (t: Token.t, tokens): Ast_Pattern.t =>
    switch t {
    | Null(loc) => #Null(loc)
    | False(loc) => #False(loc)
    | True(loc) => #True(loc)
    | Identifier(loc, x) => #Binding(loc, x)
    | Int(loc, x) => #Int(loc, x)
    | Float(loc, x) => #Float(loc, x)
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
    | Bang(loc) => #Some(loc, parseNode(Lexer.popExn(tokens), tokens))
    | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
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
            | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
            }
          | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
          }
        | t =>
          Queue.add(q, parseNode(t, tokens))
          aux()
        }
      | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
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
      | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
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
      | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
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
      | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
      }
    aux()
  }
  @raises(Exit)
  and parseObjectKeyValue = (t, tokens) =>
    switch t {
    | String(_, key) =>
      switch Lexer.popExn(tokens) {
      | Colon(_) => (key, parseNode(Lexer.popExn(tokens), tokens))
      | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
      }
    | Identifier(loc, key) =>
      switch Lexer.peekExn(tokens) {
      | Colon(_) =>
        Lexer.popExn(tokens)->ignore
        (key, parseNode(Lexer.popExn(tokens), tokens))
      | _ => (key, #Binding(loc, key))
      }
    | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
    }

  @raises(Exit)
  let make = tokens => {
    let head = parseNode(Lexer.popExn(tokens), tokens)
    let q = Queue.make()

    @raises(Exit)
    let rec aux = () => {
      switch Lexer.peekExn(tokens) {
      | Comma(_) =>
        Lexer.popExn(tokens)->ignore
        Queue.add(q, parseNode(Lexer.popExn(tokens), tokens))
        aux()
      | _ => NonEmpty.fromQueue(head, q)
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
  | Identifier(loc, x) => #Binding(loc, x)
  | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
  }

@raises(Exit)
let parseCommaSequence = tokens => {
  let head = parseBindingName(tokens)
  let q = Queue.make()

  @raises(Exit)
  let rec aux = () =>
    switch Lexer.peekExn(tokens) {
    | Comma(_) =>
      Lexer.popExn(tokens)->ignore
      Queue.add(q, parseBindingName(tokens))
      aux()
    | _ => NonEmpty.fromQueue(head, q)
    }
  aux()
}

@raises(Exit)
let parseEchoAux = (t: Token.t, tokens, esc): Ast.Echo.t =>
  switch t {
  | Identifier(loc, x) => Binding(loc, x, esc)
  | String(loc, x) => String(loc, x, esc)
  | Int(loc, x) => Int(loc, x, esc)
  | Float(loc, x) => Float(loc, x, esc)
  | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
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
  let rec aux = last =>
    switch Lexer.popExn(tokens) {
    | (Tilde(_) | Text(_)) as t => {nextToken: t, data: (Queue.toArray(q), last)}
    | Question(_) =>
      Queue.add(q, last)
      aux(parseEcho(tokens))
    | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
    }
  aux(head)
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
      | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
      }
    | Comment(_) => aux(Lexer.popExn(tokens))
    | Identifier(loc, "match") =>
      let identifiers = parseCommaSequence(tokens)
      let withs = parseWithBlocks(tokens, ~block="match")
      Queue.add(q, Match(loc, identifiers, withs))
      aux(Lexer.popExn(tokens))
    | Identifier(loc, "map") =>
      switch Pattern.parseNode(Lexer.popExn(tokens), tokens) {
      | #...Ast.mapArrayPattern as pattern =>
        let withs = parseWithBlocks(tokens, ~block="map")
        Queue.add(q, MapArray(loc, pattern, withs))
        aux(Lexer.popExn(tokens))
      // Using #...Ast_Pattern.t is slightly more performant than _
      | #...Ast_Pattern.t as x =>
        raise(Exit(Debug.badMapArrayPattern(x, module(Ast_Pattern), ~name=Lexer.name(tokens))))
      }
    | Identifier(loc, "map_dict") =>
      switch Pattern.parseNode(Lexer.popExn(tokens), tokens) {
      | #...Ast.mapDictPattern as pattern =>
        let withs = parseWithBlocks(tokens, ~block="map_dict")
        Queue.add(q, MapDict(loc, pattern, withs))
        aux(Lexer.popExn(tokens))
      // Using #...Ast_Pattern.t is slightly more performant than _
      | #...Ast_Pattern.t as x =>
        raise(Exit(Debug.badMapDictPattern(x, module(Ast_Pattern), ~name=Lexer.name(tokens))))
      }
    | Echo(loc) =>
      let {nextToken, data: (nullables, default)} = parseEchoes(tokens)
      Queue.add(q, Echo({loc: loc, nullables: nullables, default: default}))
      aux(nextToken)
    | ComponentName(loc, name) =>
      Queue.add(q, parseComponent(loc, name, tokens))
      aux(Lexer.popExn(tokens))
    | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
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
      {nextToken: nextToken, data: {Ast.patterns: NonEmpty.fromQueue(head, q), nodes: nodes}}
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
    let rec aux = (t: Token.t) =>
      switch t {
      | Slash(_) =>
        switch Lexer.popExn(tokens) {
        | Identifier(_, x) if x == block => NonEmpty.fromQueue(head, q)
        | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
        }
      /* This is guaranteed to be a "with" clause. */
      | _with =>
        let {nextToken, data} = parseWithBlock(tokens)
        Queue.add(q, data)
        aux(nextToken)
      }
    aux(nextToken)
  | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
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
    | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
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
          | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
          }
        | ComponentName(_, name') =>
          Queue.add(children, (name, ChildName(name')))
          aux(Lexer.popExn(tokens))
        | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
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
let makeExn = (~name, source) => {
  let tokens = Lexer.make(source, ~name)
  let {data, _} = parse(Lexer.popExn(tokens), tokens, ~until=endOfFile)
  data
}
