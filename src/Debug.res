/**
   Copyright 2020 John Jackson

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

open Acutis_Types

/* Lexer errors */
exception UnexpectedEoF({loc: loc, name: option<identifier>})
exception UnterminatedComment({loc: loc, name: option<identifier>})
exception UnterminatedString({loc: loc, name: option<identifier>})
exception IllegalIdentifier({loc: loc, identifier: identifier, name: option<identifier>})
exception InvalidCharacter({loc: loc, character: string, name: option<identifier>})
exception UnexpectedCharacter({
  loc: loc,
  character: string,
  expected: array<string>,
  name: option<identifier>,
})

/* Parse errors */
exception UnexpectedToken({token: Tokens.t, name: option<identifier>})
exception IllegalBindingName({loc: loc, binding: identifier, name: option<identifier>})
exception InvalidStatement({loc: loc, statement: identifier, name: option<identifier>})

/* Render errors */
exception ComponentDoesNotExist({loc: loc, component: identifier, name: option<identifier>})
exception PatternTypeMismatch({
  data: Js.Json.tagged_t,
  pattern: Pattern_Ast.node,
  name: option<identifier>,
})
exception BindingTypeMismatch({
  data: Js.Json.tagged_t,
  pattern: Pattern_Ast.node,
  binding: identifier,
  name: option<identifier>,
})
exception NameBoundMultipleTimes({loc: loc, binding: identifier, name: option<identifier>})
exception NoMatchFound({loc: loc, name: option<identifier>})
exception PatternNumberMismatch({loc: loc, name: option<identifier>})
exception BadEchoType({
  loc: loc,
  binding: identifier,
  type_: Js.Json.tagged_t,
  name: option<identifier>,
})
exception BindingDoesNotExist({loc: loc, binding: identifier, name: option<identifier>})
exception ChildDoesNotExist({loc: loc, child: identifier, name: option<identifier>})
exception BadMapType({
  loc: loc,
  binding: identifier,
  type_: Js.Json.tagged_t,
  name: option<identifier>,
})

/* Input errors */
exception BadInputTemplate(string)
exception BadRenderInput

let errorMessage = {
  let jsonTaggedTToString = (x: Js.Json.tagged_t) =>
    switch x {
    | JSONTrue | JSONFalse => "boolean"
    | JSONNull => "null"
    | JSONString(_) => "string"
    | JSONNumber(_) => "number"
    | JSONArray(_) => "array"
    | JSONObject(_) => "object"
    }

  let makeMessage = (~kind, ~text, ~loc as Loc(loc), ~name) => {
    let loc = Belt.Int.toString(loc + 1)
    let name = switch name {
    | Some(Id(name)) => name
    | None => "[no name]"
    }
    `There's a problem with template ${name}

${kind} error at character ${loc}.
  ${text}
`
  }

  err =>
    switch err {
    /* Lexer errors */
    | UnexpectedEoF({loc, name}) =>
      makeMessage(~loc, ~name, ~kind="Syntax", ~text="Unexpected end of file.")
    | UnterminatedComment({loc, name}) =>
      makeMessage(~loc, ~name, ~kind="Syntax", ~text="Unterminated comment.")
    | UnterminatedString({loc, name}) =>
      makeMessage(~loc, ~name, ~kind="Syntax", ~text="Unterminated string.")
    | IllegalIdentifier({loc, name, identifier: Id(identifier)}) =>
      makeMessage(
        ~loc,
        ~name,
        ~kind="Parse",
        ~text=`"${identifier}" is an illegal identifier name.`,
      )
    | InvalidCharacter({loc, name, character}) =>
      makeMessage(~loc, ~name, ~kind="Syntax", ~text=`Invalid character: "${character}".`)
    | UnexpectedCharacter({loc, name, character, expected}) =>
      let message = Belt.Array.size(expected) == 1 ? "Expected" : "Expected one of"
      let expected = Js.Array2.joinWith(expected, ", ")
      makeMessage(
        ~loc,
        ~name,
        ~kind="Syntax",
        ~text=`Unexpected character: "${character}".
${message}: "${expected}".`,
      )
    /* Parse errors */
    | UnexpectedToken({token, name}) =>
      let loc = Acutis_Types.Tokens.toLocation(token)
      let token = Acutis_Types.Tokens.toString(token)
      makeMessage(~loc, ~name, ~kind="Parse", ~text=`Unexpected token: "${token}".`)
    | IllegalBindingName({loc, name, binding: Id(binding)}) =>
      makeMessage(~loc, ~name, ~kind="Parse", ~text=`"${binding}" is a reserved name`)
    | InvalidStatement({loc, name, statement: Id(statement)}) =>
      makeMessage(~loc, ~name, ~kind="Parse", ~text=`Invalid statement: "${statement}".`)
    /* Render errors */
    | ComponentDoesNotExist({loc, component: Id(component), name}) =>
      makeMessage(~loc, ~name, ~kind="Parse", ~text=`Component "${component}" does not exist.`)
    | PatternTypeMismatch({data, pattern, name}) =>
      let data = jsonTaggedTToString(data)
      let loc = Acutis_Types.Pattern_Ast.toLocation(pattern)
      let pattern = Acutis_Types.Pattern_Ast.toString(pattern)
      makeMessage(
        ~loc,
        ~name,
        ~kind="Type",
        ~text=`This pattern is type ${pattern} but the data is type ${data}.`,
      )
    | BindingTypeMismatch({data, pattern, binding: Id(binding), name}) =>
      let data = jsonTaggedTToString(data)
      let loc = Pattern_Ast.toLocation(pattern)
      let pattern = Pattern_Ast.toString(pattern)
      makeMessage(
        ~loc,
        ~name,
        ~kind="Pattern",
        ~text=`Type mismatch: "${binding}" is type ${pattern} but the data is type ${data}.`,
      )
    | NameBoundMultipleTimes({loc, binding: Id(binding), name}) =>
      makeMessage(
        ~loc,
        ~name,
        ~kind="Pattern matching",
        ~text=`"${binding}" is bound multiple times in this pattern.`,
      )

    | NoMatchFound({loc, name}) =>
      makeMessage(
        ~loc,
        ~name,
        ~kind="Pattern matching",
        ~text="None of the patterns match the data. Consider a catch-all case to avoid this.",
      )
    | PatternNumberMismatch({loc, name}) =>
      makeMessage(
        ~loc,
        ~name,
        ~kind="Pattern Matching",
        ~text="The number of patterns does not match the number of data.",
      )
    | BadEchoType({loc, binding: Id(binding), type_, name}) =>
      let type_ = jsonTaggedTToString(type_)
      makeMessage(
        ~loc,
        ~name,
        ~kind="Render",
        ~text=`"${binding}" is a ${type_}. I can only echo strings and numbers.`,
      )
    | BindingDoesNotExist({loc, binding: Id(binding), name}) =>
      makeMessage(~loc, ~name, ~kind="Render", ~text=`Binding "${binding}" does not exist.`)
    | ChildDoesNotExist({loc, child: Id(child), name}) =>
      makeMessage(~loc, ~name, ~kind="Render", ~text=`Template child "${child}" does not exist.`)
    | BadMapType({loc, binding: Id(binding), type_, name}) => {
        let type_ = jsonTaggedTToString(type_)
        makeMessage(
          ~loc,
          ~name,
          ~kind="Type",
          ~text=`"${binding}" is a ${type_}. I can only map arrays.`,
        )
      }
    /* Input errors */
    | BadInputTemplate(x) => `Input template is type ${x} but must be type string.`
    | BadRenderInput => `The render function didn't receive an AST. Did you forget to compile the source first?`
    /* Everything else... */
    | e =>
      let e = Js.String.make(e)
      `An unexpected error occured in a template. This is probably due to malformed input.

${e}`
    }
}
