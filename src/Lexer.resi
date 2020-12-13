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

type t

let make: (~name: string=?, string) => t

let peekExn: t => Acutis_Types.Tokens.t

let popExn: t => Acutis_Types.Tokens.t

let skipExn: t => unit

let name: t => option<string>

let debugToArray: t => array<Acutis_Types.Tokens.t>
