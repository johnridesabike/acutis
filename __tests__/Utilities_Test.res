/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
open TestFramework

let emptyComponents = Compile.Deprecated_Components.make([])->Result.getExn

describe("Result", ({test, _}) => {
  test("getOrElse", ({expect, _}) => {
    let good = Deprecated_Source.string(~name="Good", "{{ x }}")->Compile.make(emptyComponents)
    let bad = Deprecated_Source.string(~name="Bad", "{{")->Compile.make(emptyComponents)
    let getOrElse = x => Result.getOrElse(x, _ => #error)
    expect.value(good->Result.map(_ => #noerror)->getOrElse).toEqual(#noerror)
    expect.value(bad->Result.map(_ => #noerror)->getOrElse).toEqual(#error)
  })

  test("getExn", ({expect, _}) => {
    let bad = Deprecated_Source.string(~name="Bad", "{{")->Compile.make(emptyComponents)
    expect.value(
      switch Result.getExn(bad) {
      | exception Not_found => #error
      | _ => #noerror
      },
    ).toEqual(#error)
  })
})
