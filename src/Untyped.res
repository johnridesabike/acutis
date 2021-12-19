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

module Pattern = {
  type rec t =
    | UNull(Debug.loc)
    | USome(Debug.loc, t)
    | UFalse(Debug.loc)
    | UTrue(Debug.loc)
    | UString(Debug.loc, string)
    | UInt(Debug.loc, int)
    | UFloat(Debug.loc, float)
    | UTuple(Debug.loc, array<t>)
    | UList(Debug.loc, array<t>)
    | UListWithTailBinding(Debug.loc, array<t>, t)
    | UDict(Debug.loc, array<(string, t)>)
    | URecord(Debug.loc, array<(string, t)>)
    | UBinding(Debug.loc, string)

  let toLocation = x =>
    switch x {
    | UTrue(x)
    | UFalse(x)
    | UNull(x)
    | USome(x, _)
    | UString(x, _)
    | UInt(x, _)
    | UFloat(x, _)
    | UTuple(x, _)
    | UList(x, _)
    | UListWithTailBinding(x, _, _)
    | URecord(x, _)
    | UDict(x, _)
    | UBinding(x, _) => x
    }

  @raises(Exit)
  let rec parseNode = (t, tokens) =>
    switch t {
    | Token.Tkn_Null(loc) => UNull(loc)
    | Tkn_False(loc) => UFalse(loc)
    | Tkn_True(loc) => UTrue(loc)
    | Tkn_Identifier(loc, x) => UBinding(loc, x)
    | Tkn_Int(loc, x) => UInt(loc, x)
    | Tkn_Float(loc, x) => UFloat(loc, x)
    | Tkn_String(loc, x) => UString(loc, x)
    | Tkn_OpenParen(loc) =>
      switch Lexer.popExn(tokens) {
      | Tkn_CloseParen(_) => UTuple(loc, [])
      | t => parseTuple(loc, t, tokens)
      }
    | Tkn_OpenBracket(loc) =>
      switch Lexer.popExn(tokens) {
      | Tkn_CloseBracket(_) => UList(loc, [])
      | t => parseList(loc, t, tokens)
      }
    | Tkn_OpenBrace(loc) =>
      switch Lexer.popExn(tokens) {
      | Tkn_CloseBrace(_) => URecord(loc, [])
      | t => parseObject(loc, t, tokens)
      }
    | Tkn_OpenPointyBracket(loc) =>
      switch Lexer.popExn(tokens) {
      | Tkn_ClosePointyBracket(_) => UDict(loc, [])
      | t => parseDict(loc, t, tokens)
      }
    | Tkn_Bang(loc) => USome(loc, parseNode(Lexer.popExn(tokens), tokens))
    | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
    }
  @raises(Exit)
  and parseList = (loc, t, tokens) => {
    let q = Queue.make()
    Queue.add(q, parseNode(t, tokens))

    @raises(Exit)
    let rec aux = () =>
      switch Lexer.popExn(tokens) {
      | Tkn_CloseBracket(_) => UList(loc, Queue.toArray(q))
      | Tkn_Comma(_) =>
        switch Lexer.popExn(tokens) {
        | Tkn_Spread(_) =>
          switch Lexer.popExn(tokens) {
          | Tkn_Identifier(bindingLoc, tailBinding) =>
            switch Lexer.popExn(tokens) {
            | Tkn_CloseBracket(_) =>
              UListWithTailBinding(loc, Queue.toArray(q), UBinding(bindingLoc, tailBinding))
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
    let rec aux = () =>
      switch Lexer.popExn(tokens) {
      | Tkn_CloseParen(_) => UTuple(loc, Queue.toArray(q))
      | Tkn_Comma(_) =>
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
    let rec aux = () =>
      switch Lexer.popExn(tokens) {
      | Tkn_CloseBrace(_) => URecord(loc, Queue.toArray(q))
      | Tkn_Comma(_) =>
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
    let rec aux = () =>
      switch Lexer.popExn(tokens) {
      | Tkn_ClosePointyBracket(_) => UDict(loc, Queue.toArray(q))
      | Tkn_Comma(_) =>
        Queue.add(q, parseObjectKeyValue(Lexer.popExn(tokens), tokens))
        aux()
      | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
      }
    aux()
  }
  @raises(Exit)
  and parseObjectKeyValue = (t, tokens) =>
    switch t {
    | Tkn_String(_, key) =>
      switch Lexer.popExn(tokens) {
      | Tkn_Colon(_) => (key, parseNode(Lexer.popExn(tokens), tokens))
      | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
      }
    | Tkn_Identifier(loc, key) =>
      switch Lexer.peekExn(tokens) {
      | Tkn_Colon(_) =>
        Lexer.popExn(tokens)->ignore
        (key, parseNode(Lexer.popExn(tokens), tokens))
      | _ => (key, UBinding(loc, key))
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
      | Tkn_Comma(_) =>
        Lexer.popExn(tokens)->ignore
        Queue.add(q, parseNode(Lexer.popExn(tokens), tokens))
        aux()
      | _ => NonEmpty.fromQueue(head, q)
      }
    }
    aux()
  }
}

type echo =
  | EBinding(Debug.loc, string, Utils.escape)
  | EChild(Debug.loc, string)
  | EString(Debug.loc, string, Utils.escape)
  | EInt(Debug.loc, int, Utils.escape)
  | EFloat(Debug.loc, float, Utils.escape)

type trim = TrimStart | TrimEnd | TrimBoth | NoTrim

type rec node =
  | UText(string, trim)
  // The first echo item that isn't null will be returned.
  | UEcho({loc: Debug.loc, nullables: array<echo>, default: echo})
  | UMatch(Debug.loc, NonEmpty.t<(Debug.loc, string)>, NonEmpty.t<case>)
  | UMapList(Debug.loc, Pattern.t, NonEmpty.t<case>)
  | UMapDict(Debug.loc, Pattern.t, NonEmpty.t<case>)
  | UComponent({
      loc: Debug.loc,
      name: string,
      props: array<(string, Pattern.t)>,
      children: array<(string, child)>,
    })
and case = {
  patterns: NonEmpty.t<NonEmpty.t<Pattern.t>>,
  nodes: t,
}
and child = UChildName(Debug.loc, string) | UChildBlock(Debug.loc, t)
and t = array<node>

type parseData<'a> = {
  nextToken: Token.t,
  data: 'a,
}

@raises(Exit)
let parseBindingName = tokens =>
  switch Lexer.popExn(tokens) {
  | Tkn_Identifier(loc, x) => (loc, x)
  | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
  }

@raises(Exit)
let parseCommaSequence = tokens => {
  let head = parseBindingName(tokens)
  let q = Queue.make()

  @raises(Exit)
  let rec aux = () =>
    switch Lexer.peekExn(tokens) {
    | Tkn_Comma(_) =>
      Lexer.popExn(tokens)->ignore
      Queue.add(q, parseBindingName(tokens))
      aux()
    | _ => NonEmpty.fromQueue(head, q)
    }
  aux()
}

@raises(Exit)
let parseEchoAux = (t: Token.t, tokens, esc) =>
  switch t {
  | Tkn_Identifier(loc, x) => EBinding(loc, x, esc)
  | Tkn_String(loc, x) => EString(loc, x, esc)
  | Tkn_Int(loc, x) => EInt(loc, x, esc)
  | Tkn_Float(loc, x) => EFloat(loc, x, esc)
  | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
  }

@raises(Exit)
let parseEcho = tokens =>
  switch Lexer.popExn(tokens) {
  | Tkn_Ampersand(_) => parseEchoAux(Lexer.popExn(tokens), tokens, NoEscape)
  | Tkn_ComponentName(loc, x) => EChild(loc, x)
  | t => parseEchoAux(t, tokens, Escape)
  }

@raises(Exit)
let parseEchoes = tokens => {
  let head = parseEcho(tokens)
  let q = Queue.make()

  @raises(Exit)
  let rec aux = last =>
    switch Lexer.popExn(tokens) {
    | (Tkn_Tilde(_) | Tkn_Text(_)) as t => {nextToken: t, data: (Queue.toArray(q), last)}
    | Tkn_Question(_) =>
      Queue.add(q, last)
      aux(parseEcho(tokens))
    | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
    }
  aux(head)
}

let endOfMatchMap = (. t: Token.t) =>
  switch t {
  | Tkn_Identifier(_, "with") | Tkn_Slash(_) => true
  | _ => false
  }

let endOfFile = (. t: Token.t) =>
  switch t {
  | Tkn_EndOfFile(_) => true
  | _ => false
  }

let slash = (. t: Token.t) =>
  switch t {
  | Tkn_Slash(_) => true
  | _ => false
  }

@raises(Exit)
let rec parse = (t, tokens, ~until) => {
  let q = Queue.make()

  @raises(Exit)
  let rec aux = (t: Token.t) =>
    switch t {
    | t if until(. t) => {nextToken: t, data: Queue.toArray(q)}
    | Tkn_Text(_, x) =>
      switch Lexer.popExn(tokens) {
      | Tkn_Tilde(_) =>
        Queue.add(q, UText(x, TrimEnd))
        aux(Lexer.popExn(tokens))
      | t =>
        Queue.add(q, UText(x, NoTrim))
        aux(t)
      }
    | Tkn_Tilde(_) =>
      switch Lexer.popExn(tokens) {
      | Tkn_Text(_, x) =>
        switch Lexer.popExn(tokens) {
        | Tkn_Tilde(_) =>
          Queue.add(q, UText(x, TrimBoth))
          aux(Lexer.popExn(tokens))
        | t =>
          Queue.add(q, UText(x, TrimStart))
          aux(t)
        }
      | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
      }
    | Tkn_Comment(_) => aux(Lexer.popExn(tokens))
    | Tkn_Identifier(loc, "match") =>
      let identifiers = parseCommaSequence(tokens)
      let withs = parseWithBlocks(tokens, ~block="match")
      Queue.add(q, UMatch(loc, identifiers, withs))
      aux(Lexer.popExn(tokens))
    | Tkn_Identifier(loc, "map") =>
      let pattern = Pattern.parseNode(Lexer.popExn(tokens), tokens)
      let withs = parseWithBlocks(tokens, ~block="map")
      Queue.add(q, UMapList(loc, pattern, withs))
      aux(Lexer.popExn(tokens))
    | Tkn_Identifier(loc, "map_dict") =>
      let pattern = Pattern.parseNode(Lexer.popExn(tokens), tokens)
      let withs = parseWithBlocks(tokens, ~block="map_dict")
      Queue.add(q, UMapDict(loc, pattern, withs))
      aux(Lexer.popExn(tokens))
    | Tkn_Echo(loc) =>
      let {nextToken, data: (nullables, default)} = parseEchoes(tokens)
      Queue.add(q, UEcho({loc: loc, nullables: nullables, default: default}))
      aux(nextToken)
    | Tkn_ComponentName(loc, name) =>
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
    | Tkn_Identifier(_, "with") =>
      Queue.add(q, Pattern.make(tokens))
      aux()
    | t =>
      let {nextToken, data: nodes} = parse(t, tokens, ~until=endOfMatchMap)
      {nextToken: nextToken, data: {patterns: NonEmpty.fromQueue(head, q), nodes: nodes}}
    }
  aux()
}
@raises(Exit)
and parseWithBlocks = (tokens, ~block) =>
  switch Lexer.popExn(tokens) {
  | Tkn_Identifier(_, "with") =>
    let {nextToken, data: head} = parseWithBlock(tokens)
    let q = Queue.make()

    @raises(Exit)
    let rec aux = (t: Token.t) =>
      switch t {
      | Tkn_Slash(_) =>
        switch Lexer.popExn(tokens) {
        | Tkn_Identifier(_, x) if x == block => NonEmpty.fromQueue(head, q)
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
  | Tkn_Slash(_) =>
    UComponent({
      loc: loc,
      name: name,
      props: Queue.toArray(props),
      children: Queue.toArray(children),
    })
  | t =>
    let {data: child, _} = parse(t, tokens, ~until=slash)
    switch Lexer.popExn(tokens) {
    | Tkn_ComponentName(loc', name') if name == name' =>
      Queue.add(children, ("Children", UChildBlock(loc', child)))
      UComponent({
        loc: loc,
        name: name,
        props: Queue.toArray(props),
        children: Queue.toArray(children),
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
    | Tkn_Identifier(loc, key) =>
      switch Lexer.popExn(tokens) {
      | Tkn_Equals(_) =>
        let prop = Pattern.parseNode(Lexer.popExn(tokens), tokens)
        Queue.add(props, (key, prop))
        aux(Lexer.popExn(tokens))
      | t =>
        Queue.add(props, (key, UBinding(loc, key)))
        aux(t)
      }
    | Tkn_ComponentName(loc, name) =>
      switch Lexer.popExn(tokens) {
      | Tkn_Equals(_) =>
        switch Lexer.popExn(tokens) {
        | Tkn_Block(_) =>
          let {data: child, _} = parse(Lexer.popExn(tokens), tokens, ~until=slash)
          switch Lexer.popExn(tokens) {
          | Tkn_Block(loc) =>
            Queue.add(children, (name, UChildBlock(loc, child)))
            aux(Lexer.popExn(tokens))
          | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
          }
        | Tkn_ComponentName(loc, name') =>
          Queue.add(children, (name, UChildName(loc, name')))
          aux(Lexer.popExn(tokens))
        | t => raise(Exit(Debug.unexpectedToken(t, module(Token), ~name=Lexer.name(tokens))))
        }
      | t =>
        Queue.add(children, (name, UChildName(loc, name)))

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
