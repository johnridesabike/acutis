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

module Pattern: {
  let parseNode: Lexer.t => Acutis_Types.Pattern_Ast.node

  let make: Lexer.t => Acutis_Types.Pattern_Ast.t
}

let makeAst: (~name: string=?, string) => Acutis_Types.Ast.t

let makeAstJs: (. string, option<string>) => Acutis_Types.Ast.t

let make: (~name: string=?, string) => Acutis_Types.templateFunction<'a>

let makeJs: (. string, option<string>) => Acutis_Types.templateFunction<'a>
