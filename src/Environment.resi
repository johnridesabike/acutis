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

type result = Acutis_Types.Result.t<string, array<Acutis_Types.Errors.t>>

module Async: {
  type result = Js.Promise.t<result>
  type template = Acutis_Types.template<result>
  type t = Acutis_Types.environment<result>

  let make: Js.Dict.t<template> => t
}

type template = Acutis_Types.template<result>
type t = Acutis_Types.environment<result>

let make: Js.Dict.t<template> => t
