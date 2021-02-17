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

module Array = Belt.Array
module MapString = Belt.Map.String
module Queue = Belt.MutableQueue
open Acutis_Types

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
    | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
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
            | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
            }
          | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
          }
        | t =>
          Queue.add(q, parseNode(t, tokens))
          aux()
        }
      | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
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
      | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
      }
    aux()
  }
  and parseObjectKeyValue = (t, tokens) =>
    switch t {
    | String(_, key) =>
      switch Lexer.popExn(tokens) {
      | Colon(_) => (key, parseNode(Lexer.popExn(tokens), tokens))
      | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
      }
    | Identifier(loc, key) =>
      switch Lexer.peekExn(tokens) {
      | Colon(_) =>
        Lexer.popExn(tokens)->ignore
        (key, parseNode(Lexer.popExn(tokens), tokens))
      | _ => (key, #Binding(loc, key))
      }
    | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
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

let parseBindingName = tokens =>
  switch Lexer.popExn(tokens) {
  | Identifier(loc, x) => (loc, x)
  | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
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
  | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
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
  let rec aux = (): (Token.t, NonEmpty.t<_>) =>
    switch Lexer.popExn(tokens) {
    | (Tilde(_) | Text(_)) as t => (t, NonEmpty(head, Queue.toArray(q)))
    | Question(_) =>
      Queue.add(q, parseEcho(tokens))
      aux()
    | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
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

let rec parse = (t, tokens, ~until) => {
  let q: Queue.t<Ast.node> = Queue.make()
  let rec aux = (t: Token.t) =>
    switch t {
    | t if until(. t) => (t, Queue.toArray(q))
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
      | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
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
      | (#Null(_) | #True(_) | #False(_) | #String(_) | #Number(_) | #Object(_)) as x =>
        Debug.badMapTypeParseExn(x, ~name=Lexer.name(tokens))
      }
    | Echo(loc) =>
      let (t, echoes) = parseEchoes(tokens)
      Queue.add(q, Echo(loc, echoes))
      aux(t)
    | ComponentName(loc, name) =>
      Queue.add(q, parseComponent(loc, name, tokens))
      aux(Lexer.popExn(tokens))
    | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
    }
  aux(t)
}
and parseWithBlock = tokens => {
  let head = Pattern.make(tokens)
  let q = Queue.make()
  let rec aux = () =>
    switch Lexer.popExn(tokens) {
    | Identifier(_, "with") =>
      Queue.add(q, Pattern.make(tokens))
      aux()
    | t =>
      let (lastToken, ast) = parse(t, tokens, ~until=endOfMatchMap)
      (lastToken, {Ast.patterns: NonEmpty(head, Queue.toArray(q)), ast: ast})
    }
  aux()
}
and parseWithBlocks = (tokens, ~block) =>
  switch Lexer.popExn(tokens) {
  | Identifier(_, "with") =>
    let (lastToken, head) = parseWithBlock(tokens)
    let q = Queue.make()
    let rec aux = (t: Token.t): NonEmpty.t<_> =>
      switch t {
      | Slash(_) =>
        switch Lexer.popExn(tokens) {
        | Identifier(_, x) if x == block => NonEmpty(head, Queue.toArray(q))
        | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
        }
      /* This is guaranteed to be a "with" clause. */
      | _with =>
        let (lastToken, block) = parseWithBlock(tokens)
        Queue.add(q, block)
        aux(lastToken)
      }
    aux(lastToken)
  | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
  }
and parseComponent = (loc, name, tokens) => {
  let (t: Token.t, props, children) = parseProps(tokens)
  switch t {
  | Slash(_) =>
    Component({
      loc: loc,
      name: name,
      props: Queue.toArray(props),
      children: Queue.toArray(children),
    })
  | t =>
    let (_, child) = parse(t, tokens, ~until=slash)
    switch Lexer.popExn(tokens) {
    | ComponentName(_, name') if name == name' =>
      Queue.add(children, ("Children", ChildBlock(child)))
      Component({
        loc: loc,
        name: name,
        props: Queue.toArray(props),
        children: Queue.toArray(children),
      })
    | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
    }
  }
}
and parseProps = tokens => {
  let props = Queue.make()
  let children: Queue.t<(string, Ast.child)> = Queue.make()
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
          let (_, child) = parse(Lexer.popExn(tokens), tokens, ~until=slash)
          switch Lexer.popExn(tokens) {
          | Block(_) =>
            Queue.add(children, (name, ChildBlock(child)))
            aux(Lexer.popExn(tokens))
          | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
          }
        | ComponentName(_, name') =>
          Queue.add(children, (name, ChildName(name')))
          aux(Lexer.popExn(tokens))
        | t => Debug.unexpectedTokenExn(t, ~name=Lexer.name(tokens))
        }
      | t =>
        Queue.add(children, (name, ChildName(name)))
        aux(t)
      }
    | t => (t, props, children)
    }
  aux(Lexer.popExn(tokens))
}

let makeAst = (~name, source) => {
  try {
    let tokens = Lexer.make(source, ~name)
    let (_, ast) = parse(Lexer.popExn(tokens), tokens, ~until=endOfFile)
    #ok({Ast.ast: ast, name: name})
  } catch {
  | Debug.CompileError(e) => #errors([e])
  | e => #errors([Debug.compileExn(e, ~name)])
  }
}

let make = (x: Source.t<_>) =>
  switch x {
  | String({name, src}) =>
    makeAst(src, ~name)->Result.mapU((. ast, . env, props, templates) =>
      env.render(. ast, props, templates)
    )
  | StringFunc({name, src, f}) => makeAst(src, ~name)->Result.mapU((. ast) => f(. ast))
  | Func({f, _}) => #ok(f)
  }

let emptyMap = MapString.empty

let fromArray = q => {
  let errors = Queue.make()
  let result = Array.reduceU(q, MapString.empty, (. acc, src) =>
    switch make(src) {
    | #ok(f) =>
      let name = Source.name(src)
      if MapString.has(acc, name) {
        Queue.add(errors, Debug.duplicateCompName(name))
        acc
      } else {
        MapString.set(acc, name, f)
      }
    | #errors(e) =>
      e->Queue.fromArray->Queue.transfer(errors)
      acc
    }
  )
  if Queue.isEmpty(errors) {
    #ok(result)
  } else {
    #errors(Queue.toArray(errors))
  }
}
