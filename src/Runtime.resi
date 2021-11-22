/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

let make: (
  ~nodes: Optimize.Ast.nodes<Optimize.componentTemplateU<'a>>,
  ~props: Js.Dict.t<Props.t>,
  ~children: Js.Dict.t<'a>,
  ~stack: Debug.Stack.t,
  ~makeEnv: (. Debug.Stack.t) => Optimize.environment<'a>,
  ~error: (. array<Debug.t>) => 'a,
  ~try_: (. (. unit) => 'a, ~catch: (. 'b) => 'a) => 'a,
  ~reduceQueue: (. Belt.MutableQueue.t<'a>) => 'a,
) => Belt.MutableQueue.t<'a>
