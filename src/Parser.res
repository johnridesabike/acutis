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

module MapString = Belt.Map.String
module Queue = Belt.MutableQueue
module T = Lexer.Token

exception Exit = Debug.Exit

module Pattern = {
  type rec t =
    | UNullable(Debug.t, option<t>)
    | UBool(Debug.t, bool)
    | UString(Debug.t, string)
    | UInt(Debug.t, int)
    | UFloat(Debug.t, float)
    | UTuple(Debug.t, array<t>)
    | UList(Debug.t, array<t>, option<t>)
    | UDict(Debug.t, MapString.t<t>)
    | URecord(Debug.t, MapString.t<t>)
    | UBinding(Debug.t, string)

  let debug = x =>
    switch x {
    | UBool(x, _)
    | UNullable(x, _)
    | UString(x, _)
    | UInt(x, _)
    | UFloat(x, _)
    | UTuple(x, _)
    | UList(x, _, _)
    | URecord(x, _)
    | UDict(x, _)
    | UBinding(x, _) => x
    }

  @raises(Exit)
  let rec parseNode = (t, tokens) =>
    switch t {
    | T.Tkn_Null(d) => UNullable(d, None)
    | Tkn_Bang(d) => UNullable(d, Some(parseNode(Lexer.pop(tokens), tokens)))
    | Tkn_False(d) => UBool(d, false)
    | Tkn_True(d) => UBool(d, true)
    | Tkn_Identifier(d, x) => UBinding(d, x)
    | Tkn_Int(d, x) => UInt(d, x)
    | Tkn_Float(d, x) => UFloat(d, x)
    | Tkn_String(d, x) => UString(d, x)
    | Tkn_OpenParen(d) =>
      switch Lexer.pop(tokens) {
      | Tkn_CloseParen(_) => UTuple(d, [])
      | t => parseTuple(d, t, tokens)
      }
    | Tkn_OpenBracket(d) =>
      switch Lexer.pop(tokens) {
      | Tkn_CloseBracket(_) => UList(d, [], None)
      | t => parseList(d, t, tokens)
      }
    | Tkn_OpenBrace(d) =>
      switch Lexer.pop(tokens) {
      | Tkn_CloseBrace(_) => URecord(d, MapString.empty)
      | t => parseObject(d, t, tokens)
      }
    | Tkn_OpenPointyBracket(d) =>
      switch Lexer.pop(tokens) {
      | Tkn_ClosePointyBracket(_) => UDict(d, MapString.empty)
      | t => parseDict(d, t, tokens)
      }
    | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
    }

  @raises(Exit)
  and parseList = (d, t, tokens) => {
    let q = Queue.make()
    Queue.add(q, parseNode(t, tokens))

    @raises(Exit)
    let rec aux = () =>
      switch Lexer.pop(tokens) {
      | Tkn_CloseBracket(_) => UList(d, Queue.toArray(q), None)
      | Tkn_Comma(_) =>
        switch Lexer.pop(tokens) {
        | Tkn_Spread(_) =>
          let tail = parseNode(Lexer.pop(tokens), tokens)
          switch Lexer.pop(tokens) {
          | Tkn_CloseBracket(_) => UList(d, Queue.toArray(q), Some(tail))
          | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
          }
        | t =>
          Queue.add(q, parseNode(t, tokens))
          aux()
        }
      | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
      }
    aux()
  }

  @raises(Exit)
  and parseTuple = (d, t, tokens) => {
    let q = Queue.make()
    Queue.add(q, parseNode(t, tokens))

    @raises(Exit)
    let rec aux = () =>
      switch Lexer.pop(tokens) {
      | Tkn_CloseParen(_) => UTuple(d, Queue.toArray(q))
      | Tkn_Comma(_) =>
        Queue.add(q, parseNode(Lexer.pop(tokens), tokens))
        aux()
      | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
      }
    aux()
  }

  @raises(Exit)
  and parseObject = (d, t, tokens) => {
    let m = parseObjectKeyValue(t, tokens, MapString.empty)

    @raises(Exit)
    let rec aux = m =>
      switch Lexer.pop(tokens) {
      | Tkn_CloseBrace(_) => URecord(d, m)
      | Tkn_Comma(_) => aux(parseObjectKeyValue(Lexer.pop(tokens), tokens, m))
      | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
      }
    aux(m)
  }

  @raises(Exit)
  and parseDict = (d, t, tokens) => {
    let m = parseObjectKeyValue(t, tokens, MapString.empty)

    @raises(Exit)
    let rec aux = m =>
      switch Lexer.pop(tokens) {
      | Tkn_ClosePointyBracket(_) => UDict(d, m)
      | Tkn_Comma(_) => aux(parseObjectKeyValue(Lexer.pop(tokens), tokens, m))
      | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
      }
    aux(m)
  }

  @raises(Exit)
  and parseObjectKeyValue = (t, tokens, m) =>
    switch t {
    | Tkn_String(_, key) =>
      switch Lexer.pop(tokens) {
      | Tkn_Colon(_) => MapString.set(m, key, parseNode(Lexer.pop(tokens), tokens))
      | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
      }
    | Tkn_Identifier(d, key) =>
      switch Lexer.peek(tokens) {
      | Tkn_Colon(_) =>
        Lexer.pop(tokens)->ignore
        MapString.set(m, key, parseNode(Lexer.pop(tokens), tokens))
      | _ => MapString.set(m, key, UBinding(d, key))
      }
    | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
    }

  @raises(Exit)
  let make = tokens => {
    let head = parseNode(Lexer.pop(tokens), tokens)
    let q = Queue.make()

    @raises(Exit)
    let rec aux = () => {
      switch Lexer.peek(tokens) {
      | Tkn_Comma(_) =>
        Lexer.pop(tokens)->ignore
        Queue.add(q, parseNode(Lexer.pop(tokens), tokens))
        aux()
      | _ => NonEmpty.fromQueue(head, q)
      }
    }
    aux()
  }
}

type echo =
  | EBinding(Debug.t, string, Utils.escape)
  | EChild(Debug.t, string)
  | EString(Debug.t, string, Utils.escape)
  | EInt(Debug.t, int, Utils.escape)
  | EFloat(Debug.t, float, Utils.escape)

type trim = NoTrim | TrimStart | TrimEnd | TrimBoth

type rec node =
  | UText(string, trim)
  // The first echo item that isn't null will be returned.
  | UEcho(Debug.t, array<echo>, echo)
  | UMatch(Debug.t, NonEmpty.t<Pattern.t>, NonEmpty.t<case>)
  | UMapList(Debug.t, Pattern.t, NonEmpty.t<case>)
  | UMapDict(Debug.t, Pattern.t, NonEmpty.t<case>)
  | UComponent(Debug.t, string, MapString.t<Pattern.t>, MapString.t<child>)

and case = {
  patterns: NonEmpty.t<NonEmpty.t<Pattern.t>>,
  nodes: t,
}

and child = UChildName(Debug.t, string) | UChildBlock(Debug.t, t)

and t = array<node>

type parseData<'a> = {
  nextT: T.t,
  data: 'a,
}

@raises(Exit)
let parseEchoAux = (t, esc) =>
  switch t {
  | T.Tkn_Identifier(d, x) => EBinding(d, x, esc)
  | Tkn_String(d, x) => EString(d, x, esc)
  | Tkn_Int(d, x) => EInt(d, x, esc)
  | Tkn_Float(d, x) => EFloat(d, x, esc)
  | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
  }

@raises(Exit)
let parseEcho = tokens =>
  switch Lexer.pop(tokens) {
  | Tkn_Ampersand(_) => parseEchoAux(Lexer.pop(tokens), NoEscape)
  | Tkn_ComponentName(d, x) => EChild(d, x)
  | t => parseEchoAux(t, Escape)
  }

@raises(Exit)
let parseEchoes = tokens => {
  let head = parseEcho(tokens)
  let q = Queue.make()

  @raises(Exit)
  let rec aux = last =>
    switch Lexer.pop(tokens) {
    | (Tkn_Tilde(_) | Tkn_Text(_)) as t => {nextT: t, data: (Queue.toArray(q), last)}
    | Tkn_Question(_) =>
      Queue.add(q, last)
      aux(parseEcho(tokens))
    | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
    }
  aux(head)
}

let endOfMatchMap = (. t) =>
  switch t {
  | T.Tkn_Identifier(_, "with") | Tkn_Slash(_) => true
  | _ => false
  }

let endOfFile = (. t) =>
  switch t {
  | T.Tkn_EndOfFile(_) => true
  | _ => false
  }

let slash = (. t) =>
  switch t {
  | T.Tkn_Slash(_) => true
  | _ => false
  }

@raises(Exit)
let rec parse = (t, tokens, ~until) => {
  let q = Queue.make()

  @raises(Exit)
  let rec aux = t =>
    switch t {
    | t if until(. t) => {nextT: t, data: Queue.toArray(q)}
    | T.Tkn_Text(_, x) =>
      switch Lexer.pop(tokens) {
      | Tkn_Tilde(_) =>
        Queue.add(q, UText(x, TrimEnd))
        aux(Lexer.pop(tokens))
      | t =>
        Queue.add(q, UText(x, NoTrim))
        aux(t)
      }
    | Tkn_Tilde(_) =>
      switch Lexer.pop(tokens) {
      | Tkn_Text(_, x) =>
        switch Lexer.pop(tokens) {
        | Tkn_Tilde(_) =>
          Queue.add(q, UText(x, TrimBoth))
          aux(Lexer.pop(tokens))
        | t =>
          Queue.add(q, UText(x, TrimStart))
          aux(t)
        }
      | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
      }
    | Tkn_Comment(_) => aux(Lexer.pop(tokens))
    | Tkn_Identifier(d, "match") =>
      let identifiers = Pattern.make(tokens)
      let withs = parseWithBlocks(tokens, ~block="match")
      Queue.add(q, UMatch(d, identifiers, withs))
      aux(Lexer.pop(tokens))
    | Tkn_Identifier(d, "map") =>
      let pattern = Pattern.parseNode(Lexer.pop(tokens), tokens)
      let withs = parseWithBlocks(tokens, ~block="map")
      Queue.add(q, UMapList(d, pattern, withs))
      aux(Lexer.pop(tokens))
    | Tkn_Identifier(d, "map_dict") =>
      let pattern = Pattern.parseNode(Lexer.pop(tokens), tokens)
      let withs = parseWithBlocks(tokens, ~block="map_dict")
      Queue.add(q, UMapDict(d, pattern, withs))
      aux(Lexer.pop(tokens))
    | Tkn_Echo(debug) =>
      let {nextT, data: (nullables, default)} = parseEchoes(tokens)
      Queue.add(q, UEcho(debug, nullables, default))
      aux(nextT)
    | Tkn_ComponentName(d, name) =>
      Queue.add(q, parseComponent(d, name, tokens))
      aux(Lexer.pop(tokens))
    | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
    }
  aux(t)
}

@raises(Exit)
and parseWithBlock = tokens => {
  let head = Pattern.make(tokens)
  let q = Queue.make()

  @raises(Exit)
  let rec aux = () =>
    switch Lexer.pop(tokens) {
    | Tkn_Identifier(_, "with") =>
      Queue.add(q, Pattern.make(tokens))
      aux()
    | t =>
      let {nextT, data: nodes} = parse(t, tokens, ~until=endOfMatchMap)
      {nextT: nextT, data: {patterns: NonEmpty.fromQueue(head, q), nodes: nodes}}
    }
  aux()
}

@raises(Exit)
and parseWithBlocks = (tokens, ~block) =>
  switch Lexer.pop(tokens) {
  | Tkn_Identifier(_, "with") =>
    let {nextT, data: head} = parseWithBlock(tokens)
    let q = Queue.make()

    @raises(Exit)
    let rec aux = t =>
      switch t {
      | T.Tkn_Slash(_) =>
        switch Lexer.pop(tokens) {
        | Tkn_Identifier(_, x) if x == block => NonEmpty.fromQueue(head, q)
        | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
        }
      /* This is guaranteed to be a "with" clause. */
      | _with =>
        let {nextT, data} = parseWithBlock(tokens)
        Queue.add(q, data)
        aux(nextT)
      }
    aux(nextT)
  | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
  }

@raises(Exit)
and parseComponent = (debug, name, tokens) => {
  let {nextT, data: (props, children)} = parseProps(tokens)
  switch nextT {
  | Tkn_Slash(_) => UComponent(debug, name, props, children)
  | t =>
    let {data: child, _} = parse(t, tokens, ~until=slash)
    switch Lexer.pop(tokens) {
    | Tkn_ComponentName(d', name') if name == name' =>
      let children = MapString.set(children, "Children", UChildBlock(d', child))
      UComponent(debug, name, props, children)
    | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
    }
  }
}

@raises(Exit)
and parseProps = tokens => {
  @raises(Exit)
  let rec aux = (t, props, children) =>
    switch t {
    | T.Tkn_Identifier(d, key) =>
      switch Lexer.pop(tokens) {
      | Tkn_Equals(_) =>
        let prop = Pattern.parseNode(Lexer.pop(tokens), tokens)
        let props = MapString.set(props, key, prop)
        aux(Lexer.pop(tokens), props, children)
      | t =>
        let props = MapString.set(props, key, UBinding(d, key))
        aux(t, props, children)
      }
    | Tkn_ComponentName(d, name) =>
      switch Lexer.pop(tokens) {
      | Tkn_Equals(_) =>
        switch Lexer.pop(tokens) {
        | Tkn_Block(_) =>
          let {data: child, _} = parse(Lexer.pop(tokens), tokens, ~until=slash)
          switch Lexer.pop(tokens) {
          | Tkn_Block(d) =>
            let children = MapString.set(children, name, UChildBlock(d, child))
            aux(Lexer.pop(tokens), props, children)
          | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
          }
        | Tkn_ComponentName(d, name') =>
          let children = MapString.set(children, name, UChildName(d, name'))
          aux(Lexer.pop(tokens), props, children)
        | t => raise(Exit(Debug.unexpectedToken(t, module(T))))
        }
      | t =>
        let children = MapString.set(children, name, UChildName(d, name))
        aux(t, props, children)
      }
    | t => {nextT: t, data: (props, children)}
    }
  aux(Lexer.pop(tokens), MapString.empty, MapString.empty)
}

@raises(Exit)
let make = tokens => parse(Lexer.pop(tokens), tokens, ~until=endOfFile).data
