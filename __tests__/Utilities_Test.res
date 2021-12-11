/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
open TestFramework

let emptyComponents = Compile.Components.empty()

describe("Result", ({test, _}) => {
  test("getOrElse", ({expect, _}) => {
    let good = Compile.make(~name="Good", "{{ x }}", emptyComponents)
    let bad = Compile.make(~name="Bad", "{{", emptyComponents)
    let getOrElse = x => Result.getOrElse(x, _ => #error)
    expect.value(good->Result.map(_ => #noerror)->getOrElse).toEqual(#noerror)
    expect.value(bad->Result.map(_ => #noerror)->getOrElse).toEqual(#error)
  })

  test("getExn", ({expect, _}) => {
    let bad = Compile.make(~name="Bad", "{{", emptyComponents)
    expect.value(
      switch Result.getExn(bad) {
      | exception Not_found => #error
      | _ => #noerror
      },
    ).toEqual(#error)
  })
})
