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

module List = Belt.List
open Acutis_Types
open Ast
open Debug

let unexpectedToken = (token, tokens) =>
  raise(CompileError(unexpectedToken(~token, ~name=Lexer.name(tokens))))

module Pattern = {
  open Acutis_Types.Pattern_Ast

  let rec parseNode = (t: Token.t, tokens) =>
    switch t {
    | Identifier(loc, "null") => Null(loc)
    | Identifier(loc, "false") => False(loc)
    | Identifier(loc, "true") => True(loc)
    | Identifier(loc, x) => Binding(loc, x)
    | Number(loc, x) => Number(loc, x)
    | String(loc, x) => String(loc, x)
    | OpenBracket(loc) =>
      switch Lexer.popExn(tokens) {
      | CloseBracket(_) => Array(loc, list{})
      | t =>
        let head = parseNode(t, tokens)
        parseArray(loc, tokens, list{head})
      }
    | OpenBrace(loc) =>
      switch Lexer.popExn(tokens) {
      | CloseBrace(_) => Object(loc, list{})
      | t =>
        let head = parseObjectKeyValue(t, tokens)
        parseObject(loc, tokens, list{head})
      }
    | t => unexpectedToken(t, tokens)
    }
  and parseArray = (loc, tokens, valueList) =>
    switch Lexer.popExn(tokens) {
    | CloseBracket(_) => Array(loc, List.reverse(valueList))
    | Comma(_) =>
      switch Lexer.popExn(tokens) {
      | (Identifier(_) | Number(_) | String(_) | OpenBrace(_) | OpenBracket(_)) as t =>
        let item = parseNode(t, tokens)
        parseArray(loc, tokens, list{item, ...valueList})
      | Spread(_) =>
        switch (Lexer.popExn(tokens), Lexer.popExn(tokens)) {
        | (Identifier(bindingLoc, tailBinding), CloseBracket(_)) =>
          ArrayWithTailBinding({
            loc: loc,
            array: List.reverse(valueList),
            bindLoc: bindingLoc,
            binding: tailBinding,
          })
        | (Identifier(_, _), t) | (t, _) => unexpectedToken(t, tokens)
        }
      | t => unexpectedToken(t, tokens)
      }
    | t => unexpectedToken(t, tokens)
    }
  and parseObject = (loc, tokens, l) =>
    switch Lexer.popExn(tokens) {
    | CloseBrace(_) => Object(loc, List.reverse(l))
    | Comma(_) =>
      let x = parseObjectKeyValue(Lexer.popExn(tokens), tokens)
      parseObject(loc, tokens, list{x, ...l})
    | t => unexpectedToken(t, tokens)
    }
  and parseObjectKeyValue = (t, tokens) =>
    switch t {
    | String(loc, key) | Identifier(loc, key) =>
      switch Lexer.peekExn(tokens) {
      | Colon(_) =>
        Lexer.popExn(tokens)->ignore
        let value = parseNode(Lexer.popExn(tokens), tokens)
        (key, value)
      | _ =>
        if RegEx.isLegalBinding(key) {
          (key, Binding(loc, key))
        } else {
          raise(CompileError(illegalIdentifier(~loc, ~identifier=key, ~name=Lexer.name(tokens))))
        }
      }
    | t => unexpectedToken(t, tokens)
    }

  let make = tokens => {
    let head = parseNode(Lexer.popExn(tokens), tokens)
    let rec aux = (l): NonEmpty.t<'a> => {
      switch Lexer.peekExn(tokens) {
      | Comma(_) =>
        Lexer.popExn(tokens)->ignore
        aux(list{parseNode(Lexer.popExn(tokens), tokens), ...l})
      | _ => NonEmpty(head, List.reverse(l))
      }
    }
    aux(list{})
  }
}

let parseBindingName = tokens =>
  switch Lexer.popExn(tokens) {
  | Identifier(loc, x) when RegEx.isLegalBinding(x) => (loc, x)
  | Identifier(loc, x) =>
    raise(CompileError(illegalBindingName(~loc, ~binding=x, ~name=Lexer.name(tokens))))
  | t => unexpectedToken(t, tokens)
  }

let parseCommaSequence = tokens => {
  let head = parseBindingName(tokens)
  let rec aux = (l): NonEmpty.t<'a> =>
    switch Lexer.peekExn(tokens) {
    | Comma(_) =>
      Lexer.popExn(tokens)->ignore
      aux(list{parseBindingName(tokens), ...l})
    | _ => NonEmpty(head, List.reverse(l))
    }
  aux(list{})
}

let parseEcho = (tokens): Ast.Echo.t =>
  switch Lexer.popExn(tokens) {
  | Identifier(loc, x) when RegEx.isLegalBinding(x) => Binding(loc, x)
  | ComponentName(loc, x) => Child(loc, x)
  | String(_, x) => String(x)
  | Number(_, x) => Number(x)
  | Identifier(loc, x) =>
    raise(CompileError(illegalBindingName(~loc, ~binding=x, ~name=Lexer.name(tokens))))
  | t => unexpectedToken(t, tokens)
  }

let parseEchoes = tokens => {
  let head = parseEcho(tokens)
  let rec aux = (l): NonEmpty.t<'a> => {
    switch Lexer.popExn(tokens) {
    | EndOfExpression(_) => NonEmpty(head, List.reverse(l))
    | Question(_) => aux(list{parseEcho(tokens), ...l})
    | t => unexpectedToken(t, tokens)
    }
  }
  aux(list{})
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
  let rec aux = (t: Token.t, l) =>
    switch t {
    | t when until(. t) => (t, List.reverse(l))
    | Text(_, x) =>
      switch Lexer.popExn(tokens) {
      | Tilde(_) => aux(Lexer.popExn(tokens), list{Text(x, TrimEnd), ...l})
      | t => aux(t, list{Text(x, NoTrim), ...l})
      }
    | Tilde(_) =>
      switch Lexer.popExn(tokens) {
      | Text(_, x) =>
        switch Lexer.popExn(tokens) {
        | Tilde(_) => aux(Lexer.popExn(tokens), list{Text(x, TrimBoth), ...l})
        | t => aux(t, list{Text(x, TrimStart), ...l})
        }
      | t => unexpectedToken(t, tokens)
      }
    | Comment(_) => aux(Lexer.popExn(tokens), l)
    | Identifier(loc, "match") =>
      let identifiers = parseCommaSequence(tokens)
      let withs = parseWithBlocks(tokens, ~block="match")
      aux(Lexer.popExn(tokens), list{Match(loc, identifiers, withs), ...l})
    | Identifier(_, "map") =>
      let (loc, identifier) = parseBindingName(tokens)
      let withs = parseWithBlocks(tokens, ~block="map")
      aux(Lexer.popExn(tokens), list{Map(loc, identifier, withs), ...l})
    | Identifier(loc, "raw") =>
      aux(Lexer.popExn(tokens), list{Unescaped(loc, parseEchoes(tokens)), ...l})
    | Echo(loc) => aux(Lexer.popExn(tokens), list{Echo(loc, parseEchoes(tokens)), ...l})
    | ComponentName(loc, name) =>
      aux(Lexer.popExn(tokens), list{parseComponent(loc, name, tokens), ...l})
    | EndOfExpression(_) => aux(Lexer.popExn(tokens), l)
    | t => unexpectedToken(t, tokens)
    }
  aux(t, list{})
}
and parseWithBlock = tokens => {
  let head = Pattern.make(tokens)
  let rec aux = l =>
    switch Lexer.popExn(tokens) {
    | Identifier(_, "with") => aux(list{Pattern.make(tokens), ...l})
    | t =>
      let (lastToken, ast) = parse(t, tokens, ~until=endOfMatchMap)
      (lastToken, {patterns: NonEmpty(head, List.reverse(l)), ast: ast})
    }
  aux(list{})
}
and parseWithBlocks = (tokens, ~block) =>
  switch Lexer.popExn(tokens) {
  | Identifier(_, "with") =>
    let (lastToken, head) = parseWithBlock(tokens)
    let rec aux = (t: Token.t, l): NonEmpty.t<'a> =>
      switch t {
      | Slash(_) =>
        switch Lexer.popExn(tokens) {
        | Identifier(_, x) when x == block => NonEmpty(head, List.reverse(l))
        | t => unexpectedToken(t, tokens)
        }
      /* This is guaranteed to be a "with" clause. */
      | _with =>
        let (lastToken, block) = parseWithBlock(tokens)
        aux(lastToken, list{block, ...l})
      }
    aux(lastToken, list{})
  | t => unexpectedToken(t, tokens)
  }
and parseComponent = (loc, name, tokens) => {
  let (t: Token.t, props, children) = parseProps(
    Lexer.popExn(tokens),
    tokens,
    ~props=list{},
    ~children=list{},
  )
  switch t {
  | Slash(_) => Component({loc: loc, name: name, props: props, children: children})
  | t =>
    let (_, child) = parse(t, tokens, ~until=slash)
    switch Lexer.popExn(tokens) {
    | ComponentName(_, name') when name == name' =>
      Component({
        loc: loc,
        name: name,
        props: props,
        children: list{("Children", ChildBlock(child)), ...children},
      })
    | t => unexpectedToken(t, tokens)
    }
  }
}
and parseProps = (t: Token.t, tokens, ~props, ~children) =>
  switch t {
  | Identifier(loc, key) =>
    switch Lexer.popExn(tokens) {
    | Equals(_) =>
      let prop = Pattern.parseNode(Lexer.popExn(tokens), tokens)
      parseProps(Lexer.popExn(tokens), tokens, ~props=list{(key, prop), ...props}, ~children)
    | t => parseProps(t, tokens, ~props=list{(key, Binding(loc, key)), ...props}, ~children)
    }
  | ComponentName(_, name) =>
    switch Lexer.popExn(tokens) {
    | Equals(_) =>
      switch Lexer.popExn(tokens) {
      | Block(_) =>
        let (_, child) = parse(Lexer.popExn(tokens), tokens, ~until=slash)
        switch Lexer.popExn(tokens) {
        | Block(_) =>
          parseProps(
            Lexer.popExn(tokens),
            tokens,
            ~props,
            ~children=list{(name, ChildBlock(child)), ...children},
          )
        | t => unexpectedToken(t, tokens)
        }
      | ComponentName(_, name') =>
        parseProps(
          Lexer.popExn(tokens),
          tokens,
          ~props,
          ~children=list{(name, ChildName(name')), ...children},
        )
      | t => unexpectedToken(t, tokens)
      }
    | t => parseProps(t, tokens, ~props, ~children=list{(name, ChildName(name)), ...children})
    }
  | t => (t, props, children)
  }

let makeAst = (~name=?, source) => {
  try {
    let tokens = Lexer.make(source, ~name?)
    let (_, ast) = parse(Lexer.popExn(tokens), tokens, ~until=endOfFile)
    Valid.make(#data({ast: ast, name: name}))
  } catch {
  | CompileError(e) => Valid.make(#errors(e))
  | e => Valid.make(#errors(Debug.compileExn(e, ~name)))
  }
}

let make = (~name=?, source) => {
  let ast = makeAst(source, ~name?)
  (. env, props, templates) => env.render(. ast, props, templates)
}

module Js = {
  let makeAst = (. source, name) => makeAst(source, ~name?)
  let make = (. source, name) => make(source, ~name?)
}
