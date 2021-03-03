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

/* We use a polymorphic variant because it has a nicer JS representation */
type t<'a> = [#ok('a) | #errors(array<Acutis_Types.Errors.t>)]

@live
let mapU: (t<'a>, (. 'a) => 'b) => t<'b>
let map: (t<'a>, 'a => 'b) => t<'b>

@live
let flatMapU: (t<'a>, (. 'a) => t<'b>) => t<'b>
let flatMap: (t<'a>, 'a => t<'b>) => t<'b>

let getExn: t<'a> => 'a

@live
let getOrElseU: (t<'a>, (. array<Acutis_Types.Errors.t>) => 'a) => 'a
let getOrElse: (t<'a>, array<Acutis_Types.Errors.t> => 'a) => 'a
