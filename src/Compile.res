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

module List = Belt.List
open Acutis_Types
open Ast
open Debug

module Pattern = {
  open Acutis_Types.Pattern_Ast

  let rec parseNode = tokens =>
    switch Lexer.popExn(tokens) {
    | Identifier(loc, "null") => Null(loc)
    | Identifier(loc, "false") => False(loc)
    | Identifier(loc, "true") => True(loc)
    | Identifier(loc, x) => Binding(loc, x)
    | Number(loc, x) => Number(loc, x)
    | String(loc, x) => String(loc, x)
    | OpenBracket(loc) =>
      switch Lexer.peekExn(tokens) {
      | CloseBracket(_) =>
        Lexer.skipExn(tokens)
        Array(loc, list{})
      | _ =>
        let firstItem = parseNode(tokens)
        parseArray(loc, tokens, list{firstItem})
      }
    | OpenBrace(loc) =>
      switch Lexer.peekExn(tokens) {
      | CloseBrace(_) =>
        Lexer.skipExn(tokens)
        Object(loc, list{})
      | _ =>
        let firstItem = parseObjectKeyValue(tokens)
        parseObject(loc, tokens, list{firstItem})
      }
    | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
    }
  and parseArray = (loc, tokens, valueList) =>
    switch Lexer.popExn(tokens) {
    | CloseBracket(_) => Array(loc, List.reverse(valueList))
    | Comma(_) =>
      switch Lexer.peekExn(tokens) {
      | Spread(_) =>
        Lexer.skipExn(tokens)
        switch Lexer.popExn(tokens) {
        | Identifier(bindingLoc, tailBinding) =>
          switch Lexer.popExn(tokens) {
          | CloseBracket(_) =>
            ArrayWithTailBinding({
              loc: loc,
              array: List.reverse(valueList),
              bindLoc: bindingLoc,
              binding: tailBinding,
            })
          | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
          }
        | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
        }
      | Identifier(_) | Number(_) | String(_) | OpenBrace(_) | OpenBracket(_) =>
        let item = parseNode(tokens)
        parseArray(loc, tokens, list{item, ...valueList})
      | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
      }
    | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
    }
  and parseObject = (loc, tokens, keyValueList) =>
    switch Lexer.popExn(tokens) {
    | CloseBrace(_) => Object(loc, List.reverse(keyValueList))
    | Comma(_) =>
      let x = parseObjectKeyValue(tokens)
      parseObject(loc, tokens, list{x, ...keyValueList})
    | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
    }
  and parseObjectKeyValue = tokens =>
    switch Lexer.popExn(tokens) {
    | String(loc, key) | Identifier(loc, key) =>
      switch Lexer.peekExn(tokens) {
      | Colon(_) =>
        Lexer.skipExn(tokens)
        let value = parseNode(tokens)
        (key, value)
      | _ =>
        if RegEx.isLegalBinding(key) {
          (key, Binding(loc, key))
        } else {
          raise(CompileError(illegalIdentifier(~loc, ~identifier=key, ~name=Lexer.name(tokens))))
        }
      }
    | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
    }

  let make = tokens => {
    let head = parseNode(tokens)
    let rec aux = (l): NonEmpty.t<'a> => {
      switch Lexer.peekExn(tokens) {
      | Comma(_) =>
        Lexer.skipExn(tokens)
        let pattern = parseNode(tokens)
        aux(list{pattern, ...l})
      | _ => NonEmpty(head, List.reverse(l))
      }
    }
    aux(list{})
  }
}

let parseCommaSequence = tokens =>
  switch Lexer.popExn(tokens) {
  | Identifier(loc, head) when RegEx.isLegalBinding(head) =>
    let rec aux = (l): NonEmpty.t<'a> =>
      switch Lexer.peekExn(tokens) {
      | Comma(_) =>
        Lexer.skipExn(tokens)
        switch Lexer.popExn(tokens) {
        | Identifier(loc, x) when RegEx.isLegalBinding(x) => aux(list{(loc, x), ...l})
        | Identifier(loc, x) =>
          raise(CompileError(illegalBindingName(~loc, ~binding=x, ~name=Lexer.name(tokens))))
        | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
        }
      | _ => NonEmpty((loc, head), List.reverse(l))
      }
    aux(list{})
  | Identifier(loc, x) =>
    raise(CompileError(illegalBindingName(~loc, ~binding=x, ~name=Lexer.name(tokens))))
  | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
  }

let parseMatchCasePatterns = tokens => {
  switch Lexer.popExn(tokens) {
  | Identifier(_, "with") => ()
  | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
  }
  let firstPattern = Pattern.make(tokens)
  let rec aux = (l): NonEmpty.t<'a> =>
    switch Lexer.peekExn(tokens) {
    | Identifier(_, "with") =>
      Lexer.skipExn(tokens)
      let x = Pattern.make(tokens)
      aux(list{x, ...l})
    | _ => NonEmpty(firstPattern, List.reverse(l))
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
  | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
  }

let parseEchoes = tokens => {
  let head = parseEcho(tokens)
  let rec aux = (l): NonEmpty.t<'a> => {
    switch Lexer.popExn(tokens) {
    | EndOfExpression(_) => NonEmpty(head, List.reverse(l))
    | Question(_) => aux(list{parseEcho(tokens), ...l})
    | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
    }
  }
  aux(list{})
}

let endOfMatchMap = (. x: Tokens.t) =>
  switch x {
  | Identifier(_, "with") | Slash(_) => true
  | _ => false
  }

let endOfFile = (. x: Tokens.t) =>
  switch x {
  | EndOfFile(_) => true
  | _ => false
  }

let slash = (. x: Tokens.t) =>
  switch x {
  | Slash(_) => true
  | _ => false
  }

let rec parse = (tokens, ~until) => {
  let rec aux = l =>
    if until(. Lexer.peekExn(tokens)) {
      List.reverse(l)
    } else {
      switch Lexer.popExn(tokens) {
      | Text(_, x) =>
        switch Lexer.peekExn(tokens) {
        | Tilde(_) =>
          Lexer.skipExn(tokens)
          aux(list{Text(x, TrimEnd), ...l})
        | _ => aux(list{Text(x, NoTrim), ...l})
        }
      | Tilde(_) =>
        switch Lexer.popExn(tokens) {
        | Text(_, x) =>
          switch Lexer.peekExn(tokens) {
          | Tilde(_) =>
            Lexer.skipExn(tokens)
            aux(list{Text(x, TrimBoth), ...l})
          | _ => aux(list{Text(x, TrimStart), ...l})
          }
        | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
        }
      | Comment(_) => aux(l)
      | Identifier(loc, "match") =>
        let identifiers = parseCommaSequence(tokens)
        let firstCase = parseCaseBlock(tokens)
        let cases = parseCaseBlocks(tokens, ~block="match")
        aux(list{Match(loc, identifiers, NonEmpty(firstCase, cases)), ...l})
      | Identifier(_, "map") =>
        let (loc, identifier) = switch Lexer.popExn(tokens) {
        | Identifier(loc, x) when RegEx.isLegalBinding(x) => (loc, x)
        | Identifier(loc, x) =>
          raise(CompileError(illegalBindingName(~loc, ~binding=x, ~name=Lexer.name(tokens))))
        | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
        }
        let firstCase = parseCaseBlock(tokens)
        let cases = parseCaseBlocks(tokens, ~block="map")
        aux(list{Map(loc, identifier, NonEmpty(firstCase, cases)), ...l})
      | Identifier(loc, "raw") => aux(list{Unescaped(loc, parseEchoes(tokens)), ...l})
      | Echo(loc) => aux(list{Echo(loc, parseEchoes(tokens)), ...l})
      | ComponentName(loc, name) => aux(list{parseComponent(loc, name, tokens), ...l})
      | EndOfExpression(_) => aux(l)
      | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
      }
    }
  aux(list{})
}
and parseCaseBlock = tokens => {
  let patterns = parseMatchCasePatterns(tokens)
  let ast = parse(tokens, ~until=endOfMatchMap)
  {patterns: patterns, ast: ast}
}
and parseCaseBlocks = (tokens, ~block) => {
  let rec aux = l =>
    switch Lexer.peekExn(tokens) {
    | Slash(_) =>
      Lexer.skipExn(tokens)
      switch Lexer.popExn(tokens) {
      | Identifier(_, x) when x == block => List.reverse(l)
      | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
      }
    | _ =>
      let x = parseCaseBlock(tokens)
      aux(list{x, ...l})
    }
  aux(list{})
}
and parseComponent = (loc, name, tokens) => {
  let (props, children) = parseProps(tokens, ~props=list{}, ~children=list{})
  switch Lexer.peekExn(tokens) {
  | Slash(_) =>
    Lexer.skipExn(tokens)
    Component({loc: loc, name: name, props: props, children: children})
  | _ =>
    let x = parse(tokens, ~until=slash)
    Lexer.skipExn(tokens)
    let children = list{("Children", ChildBlock(x)), ...children}
    switch Lexer.popExn(tokens) {
    | ComponentName(_, name') when name == name' =>
      Component({loc: loc, name: name, props: props, children: children})
    | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
    }
  }
}
and parseProps = (tokens, ~props, ~children) =>
  switch Lexer.peekExn(tokens) {
  | Identifier(loc, key) =>
    Lexer.skipExn(tokens)
    switch Lexer.peekExn(tokens) {
    | Equals(_) =>
      Lexer.skipExn(tokens)
      let prop = Pattern.parseNode(tokens)
      parseProps(tokens, ~props=list{(key, prop), ...props}, ~children)
    | _ => parseProps(tokens, ~props=list{(key, Binding(loc, key)), ...props}, ~children)
    }
  | ComponentName(_, name) =>
    Lexer.skipExn(tokens)
    switch Lexer.peekExn(tokens) {
    | Equals(_) =>
      Lexer.skipExn(tokens)
      switch Lexer.popExn(tokens) {
      | Block(_) =>
        let child = parseBlock(tokens)
        parseProps(tokens, ~props, ~children=list{(name, ChildBlock(child)), ...children})
      | ComponentName(_, name') =>
        parseProps(tokens, ~props, ~children=list{(name, ChildName(name')), ...children})
      | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
      }
    | _ => parseProps(tokens, ~props, ~children=list{(name, ChildName(name)), ...children})
    }
  | _ => (props, children)
  }
and parseBlock = tokens => {
  let result = parse(tokens, ~until=slash)
  Lexer.skipExn(tokens)
  switch Lexer.popExn(tokens) {
  | Block(_) => result
  | x => raise(CompileError(unexpectedToken(~token=x, ~name=Lexer.name(tokens))))
  }
}

let makeAst = (~name=?, source) =>
  try {
    Valid.make(
      #data({
        ast: parse(Lexer.make(source, ~name?), ~until=endOfFile),
        name: name,
      }),
    )
  } catch {
  | CompileError(e) => Valid.make(#errors(e))
  | e => Valid.make(#errors(Debug.compileExn(e, ~name)))
  }

let make = (~name=?, source) => {
  let ast = makeAst(source, ~name?)
  (. env, props, templates) => env.render(. ast, props, templates)
}

module Js = {
  let makeAst = (. source, name) => makeAst(source, ~name?)
  let make = (. source, name) => make(source, ~name?)
}
