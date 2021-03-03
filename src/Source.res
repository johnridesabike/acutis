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

open Acutis_Types

type stringFunc<'a> = ast<'a> => template<'a>

type t<'a> =
  | String({name: string, src: string})
  | Func({name: string, f: template<'a>})
  | StringFunc({name: string, src: string, f: stringFunc<'a>})

let string = (~name, src) => String({name: name, src: src})

let func = (~name, f) => Func({name: name, f: f})

let funcWithString = (~name, src, f) => StringFunc({name: name, src: src, f: f})

let name = (String({name, _}) | Func({name, _}) | StringFunc({name, _})) => name
