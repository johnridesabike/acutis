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

type stringFunc<'a> = Acutis_Types.ast<'a> => Acutis_Types.template<'a>

type t<'a> =
  private
  | String({name: string, src: string})
  | Func({name: string, f: Acutis_Types.template<'a>})
  | StringFunc({name: string, src: string, f: stringFunc<'a>})

let string: (~name: string, string) => t<'a>

let func: (~name: string, Acutis_Types.template<'a>) => t<'a>

let funcWithString: (~name: string, string, stringFunc<'a>) => t<'a>

let name: t<_> => string
