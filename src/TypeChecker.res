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
module Array = Belt.Array
module Ast = Acutis_Types.Ast
module Ast_Pattern = Acutis_Types.Ast_Pattern
module Debug2 = TypeChecker_Debug
module MapString = Belt.Map.String
module Queue = Belt.MutableQueue

exception Exit = Debug.Exit

module NonEmpty = {
  include Acutis_Types.NonEmpty

  //let hd = (NonEmpty(hd, _)) => hd

  //let eq = (NonEmpty(hd1, tl1), NonEmpty(hd2, tl2), ~f) => f(. hd1, hd2) && Array.eqU(tl1, tl2, f)
  let size = (NonEmpty(_, tl)) => Array.size(tl) + 1

  let zipExn = (NonEmpty(hd1, tl1), NonEmpty(hd2, tl2)) => NonEmpty((hd1, hd2), Array.zip(tl1, tl2))

  /*
  let unzip = (NonEmpty((hd1, hd2), tl)) => {
    let (tl1, tl2) = Array.unzip(tl)
    (NonEmpty(hd1, tl1), NonEmpty(hd2, tl2))
  }

  let zipBy = (NonEmpty(hd1, tl1), NonEmpty(hd2, tl2), ~f) =>
    if Array.size(tl1) == Array.size(tl2) {
      NonEmpty(f(. hd1, hd2), Array.zipByU(tl1, tl2, f))
    } else {
      assert false
    }

  let forEach = (NonEmpty(hd, tl), ~f) => {
    f(. hd)
    Array.forEachU(tl, f)
  }
 */

  let reduce = (NonEmpty(hd, tl), ~init, ~f) => Array.reduceU(tl, f(. init, hd), f)
  //let reduce2 = (NonEmpty(hd, tl), ~init, ~f) => Array.reduceU(tl, init(. hd), f)
  let reduce3 = (NonEmpty(hd, tl), ~f) => Array.reduceU(tl, hd, f)
}

type rec t =
  | Polymorphic
  | Boolean
  //| LiteralTrue
  //| LiteralFalse
  | Int
  | Float
  //| LiteralInt(NonEmpty.t<int>)
  | String
  //| LiteralString(NonEmpty.t<string>)
  // | LiteralNull
  | Echo
  | Nullable(ref<t>)
  | Array(ref<t>)
  | Dict(ref<t>)
  // 0 and 1 sized tuples are legal.
  | Tuple(ref<array<ref<t>>>)
  | Object(ref<MapString.t<ref<t>>>)
// The discriminant field, common field, and variant fields cannot intersect.
//| UnionStr({discriminant: string, common: MapString.t<t>, variants: MapString.t<MapString.t<t>>})
//| UnionInt({discriminant: string, common: MapString.t<t>, variants: MapInt.t<MapString.t<t>>})

let rec toString = x =>
  switch x.contents {
  | Polymorphic => "polymorphic"
  | Boolean => "boolean"
  | Int => "int"
  | Float => "float"
  | String => "string"
  | Echo => "echoable"
  | Nullable(x) => `nullable(${toString(x)})`
  | Array(x) => `array [${toString(x)}]`
  | Dict(x) => `dictionary <${toString(x)}>`
  | Tuple(x) =>
    let x = Array.joinWith(x.contents, ", ", toString)
    `tuple (${x})`
  | Object(x) =>
    let x =
      MapString.toArray(x.contents)->Array.joinWith(", ", ((k, v)) => `"${k}": ${toString(v)}`)
    `object {${x}}`
  }

module Child = {
  type t = Child | NullableChild
  let toString = x =>
    switch x.contents {
    | Child => "Child"
    | NullableChild => "NullableChild"
    }
  let unify = (a: ref<t>, b: ref<t>) =>
    if a.contents == b.contents {
      ()
    } else {
      raise(Exit(Debug2.childTypeMismatch(a, b, ~f=toString)))
    }
}

let rec unify = (tref1, tref2, ~loc) =>
  switch (tref1.contents, tref2.contents) {
  | (Boolean, Boolean)
  | (Int, Int)
  | (Float, Float)
  | (String, String) => ()
  | (Polymorphic, t) => tref1 := t
  | (t, Polymorphic) => tref2 := t
  | (Echo, (Int | Float | String | Echo) as t) => tref1 := t
  | ((Int | Float | String) as t, Echo) => tref2 := t
  | (Nullable(t1), Nullable(t2)) => unify(t1, t2, ~loc)
  | (Nullable(t), _) =>
    unify(t, tref2, ~loc)
    tref2 := tref1.contents
  | (_, Nullable(t)) =>
    unify(t, tref1, ~loc)
    tref1 := tref2.contents
  | (Array(t1), Array(t2)) | (Dict(t1), Dict(t2)) => unify(t1, t2, ~loc)
  | (Tuple(t1), Tuple(t2)) => unifyTuple(t1, t2, ~loc)
  | (Object(t1), Object(t2)) => unifyObject(t1, t2, ~loc)
  | _ => raise(Exit(Debug2.typeMismatch(tref1, tref2, ~f=toString, ~loc)))
  }
and unifyTuple = (t1, t2, ~loc) => {
  if Array.size(t1.contents) == Array.size(t2.contents) {
    Array.zip(t1.contents, t2.contents)->Array.forEachU((. (a, b)) => unify(a, b, ~loc))
  } else {
    raise(Exit(Debug2.tupleSizeMismatch(Array.size(t1.contents), Array.size(t2.contents))))
  }
}
and unifyObject = (t1, t2, ~loc) => {
  let r = MapString.mergeU(t1.contents, t2.contents, (. _, v1, v2) =>
    switch (v1, v2) {
    | (Some(v1) as r, Some(v2)) =>
      unify(v1, v2, ~loc)
      r
    | (Some(_) as r, None) | (None, Some(_) as r) => r
    | (None, None) => None
    }
  )
  t1 := r
  t2 := r
}

module Context = {
  type t = {
    global: ref<MapString.t<ref<t>>>,
    scope: MapString.t<ref<t>>,
    children: ref<MapString.t<ref<Child.t>>>,
  }

  let make = () => {
    global: ref(MapString.empty),
    scope: MapString.empty,
    children: ref(MapString.empty),
  }

  let updateAux = (ctx, k, ~f) =>
    switch MapString.get(ctx.scope, k) {
    | None =>
      ctx.global := MapString.updateU(ctx.global.contents, k, f)
      ctx
    | Some(_) =>
      let scope = MapString.updateU(ctx.scope, k, f)
      {...ctx, scope: scope}
    }

  let set = (ctx, k, v, ~loc) =>
    updateAux(ctx, k, ~f=(. v') =>
      switch v' {
      | None => Some(v)
      | Some(v') as r =>
        unify(v', v, ~loc)
        r
      }
    )

  let setLocal = (ctx, k, v) => {...ctx, scope: MapString.set(ctx.scope, k, v)}

  let setChild = (ctx, k, v) =>
    ctx.children :=
      MapString.updateU(ctx.children.contents, k, (. v') =>
        switch v' {
        | None => Some(v)
        | Some(v') as r =>
          Child.unify(v', v)
          r
        }
      )
}

module Local = {
  let rec fromPattern = (pattern: Ast_Pattern.t, ctx) =>
    switch pattern {
    | #Null(_) => ref(Nullable(ref(Polymorphic)))
    | #False(_) | #True(_) => ref(Boolean)
    | #String(_) => ref(String)
    | #Int(_) => ref(Int)
    | #Float(_) => ref(Float)
    | #Tuple(_, t) =>
      let types = Array.mapU(t, (. x) => fromPattern(x, ctx))
      ref(Tuple(ref(types)))
    | #Array(loc, a) =>
      let t = switch a[0] {
      | None => ref(Polymorphic)
      | Some(hd) => fromPattern(hd, ctx)
      }
      Array.forEachU(a, (. x) => unify(t, fromPattern(x, ctx), ~loc))
      ref(Array(t))
    | #ArrayWithTailBinding(loc, a, #Binding(_, b)) =>
      let t = switch a[0] {
      | None => ref(Polymorphic)
      | Some(hd) => fromPattern(hd, ctx)
      }
      Array.forEachU(a, (. x) => unify(t, fromPattern(x, ctx), ~loc))
      let t = ref(Array(t))
      ctx := Context.setLocal(ctx.contents, b, t)
      t
    | #Dict(loc, d) =>
      let t = switch d[0] {
      | None => ref(Polymorphic)
      | Some((_, hd)) => fromPattern(hd, ctx)
      }
      Array.forEachU(d, (. (_, x)) => unify(t, fromPattern(x, ctx), ~loc))
      ref(Dict(t))
    | #Object(_, o) =>
      let types = o->Array.mapU((. (k, x)) => {
        let types = fromPattern(x, ctx)
        (k, types)
      })
      let types = MapString.fromArray(types)
      ref(Object(ref(types)))
    | #Binding(_, b) =>
      let t = ref(Polymorphic)
      ctx := Context.setLocal(ctx.contents, b, t)
      t
    }
  let fromPattern = (pattern, ctx) => {
    let ctx = ref(ctx)
    let t = fromPattern(pattern, ctx)
    (t, ctx.contents)
  }
}

module Global = {
  let rec fromPattern = (pattern: Ast_Pattern.t, q) =>
    switch pattern {
    | #Null(_) => ref(Nullable(ref(Polymorphic)))
    | #False(_) | #True(_) => ref(Boolean)
    | #String(_) => ref(String)
    | #Int(_) => ref(Int)
    | #Float(_) => ref(Float)
    | #Tuple(_, t) =>
      let types = Array.mapU(t, (. x) => fromPattern(x, q))
      ref(Tuple(ref(types)))
    | #Array(loc, a) =>
      let t = switch a[0] {
      | None => ref(Polymorphic)
      | Some(hd) => fromPattern(hd, q)
      }
      Array.forEachU(a, (. x) => unify(t, fromPattern(x, q), ~loc))
      ref(Array(t))
    | #ArrayWithTailBinding(loc, a, #Binding(bloc, b)) =>
      let t = switch a[0] {
      | None => ref(Polymorphic)
      | Some(hd) => fromPattern(hd, q)
      }
      Array.forEachU(a, (. x) => unify(t, fromPattern(x, q), ~loc))
      let t = ref(Array(t))
      Queue.add(q, (bloc, b, t))
      t
    | #Dict(loc, d) =>
      let t = switch d[0] {
      | None => ref(Polymorphic)
      | Some((_, hd)) => fromPattern(hd, q)
      }
      Array.forEachU(d, (. (_, x)) => unify(t, fromPattern(x, q), ~loc))
      ref(Dict(t))
    | #Object(_, o) =>
      let types = o->Array.mapU((. (k, x)) => {
        let types = fromPattern(x, q)
        (k, types)
      })
      let types = MapString.fromArray(types)
      ref(Object(ref(types)))
    | #Binding(loc, b) =>
      let t = ref(Polymorphic)
      Queue.add(q, (loc, b, t))
      t
    }
  let fromPattern = (pattern, ctx: Context.t) => {
    let q = Queue.make()
    let t = fromPattern(pattern, q)
    let ctx = Queue.reduceU(q, ctx, (. ctx, (loc, k, v)) =>
      switch MapString.get(ctx.scope, k) {
      | None =>
        ctx.global :=
          MapString.updateU(ctx.global.contents, k, (. x) =>
            switch x {
            | None => Some(v)
            | Some(v') as vopt =>
              unify(v, v', ~loc)
              vopt
            }
          )
        ctx
      | Some(v') =>
        unify(v, v', ~loc)
        ctx
      }
    )
    (t, ctx)
  }

  let unifyMatchCases = (
    bindingArray: NonEmpty.t<Ast_Pattern.binding>,
    cases: NonEmpty.t<ref<t>>,
    ctx: Context.t,
  ): Context.t => {
    NonEmpty.zipExn(bindingArray, cases)->NonEmpty.reduce(~init=ctx, ~f=(.
      ctx,
      (#Binding(loc, k), t),
    ) => Context.set(ctx, k, t, ~loc))
  }

  let unifyMapArrayCases = (
    id: Ast.mapArrayPattern,
    NonEmpty(case_hd, case_tl): NonEmpty.t<ref<t>>,
    ctx: Context.t,
    ~loc,
  ): Context.t => {
    let int = ref(Int)
    let index = switch case_tl {
    | [] => int
    | [tl] => tl
    | _ => raise(Exit(Debug2.mapPatternSizeMismatch(~loc)))
    }
    unify(index, int, ~loc)
    switch id {
    | #Binding(loc, binding) => Context.set(ctx, binding, ref(Array(case_hd)), ~loc)
    | (#Array(loc, _) | #ArrayWithTailBinding(loc, _, _)) as a =>
      let (a_t, _) = fromPattern(a, ctx)
      unify(a_t, ref(Array(case_hd)), ~loc)
      ctx
    }
  }

  let unifyMapDictCases = (
    id: Ast.mapDictPattern,
    NonEmpty(case_hd, case_tl): NonEmpty.t<ref<t>>,
    ctx: Context.t,
    ~loc,
  ): Context.t => {
    let int = ref(String)
    let index = switch case_tl {
    | [] => int
    | [tl] => tl
    | _ => raise(Exit(Debug2.mapPatternSizeMismatch(~loc)))
    }
    unify(index, int, ~loc)
    switch id {
    | #Binding(loc, binding) => Context.set(ctx, binding, ref(Array(case_hd)), ~loc)
    | #Dict(loc, _) as d =>
      let (t, _) = fromPattern(d, ctx)
      unify(t, ref(Dict(case_hd)), ~loc)
      ctx
    }
  }
}

// Until we upgrade the AST
let fixEchoes = (NonEmpty(hd, tl): NonEmpty.t<_>) => {
  let q = Queue.make()
  let rec aux = (hd, i) =>
    switch tl[i] {
    | None => (Queue.toArray(q), hd)
    | Some(next) =>
      Queue.add(q, hd)
      aux(next, succ(i))
    }
  aux(hd, 0)
}

let unifyEchoes = (echoes: NonEmpty.t<Ast.Echo.t>, ctx): Context.t => {
  let (nullables, default) = fixEchoes(echoes)
  let rec aux = (i, ctx) => {
    switch nullables[i] {
    | None =>
      switch default {
      | Binding(loc, binding, _) => Context.set(ctx, binding, ref(Echo), ~loc)
      | Child(_, child) =>
        Context.setChild(ctx, child, ref(Child.Child))
        ctx
      | String(_, _, _) | Int(_, _, _) | Float(_, _, _) => ctx
      }
    | Some(Binding(loc, binding, _)) =>
      let ctx = Context.set(ctx, binding, ref(Nullable(ref(Echo))), ~loc)
      aux(succ(i), ctx)
    | Some(String(_, _, _) | Int(_, _, _) | Float(_, _, _)) =>
      raise(Exit(Debug2.nonNullableEchoLiteral()))
    | Some(Child(_, child)) =>
      Context.setChild(ctx, child, ref(Child.NullableChild))
      aux(succ(i), ctx)
    }
  }
  aux(0, ctx)
}

let unifyNestedNonEmpty = (cases, ~loc): NonEmpty.t<ref<t>> => {
  let x = NonEmpty.reduce3(cases, ~f=(. merged, case) => {
    NonEmpty.zipExn(merged, case)->NonEmpty.map(~f=(. (casea, caseb)) => {
      unify(casea, caseb, ~loc)
      casea
    })
  })
  x
}

let rec makeCase = (cases: NonEmpty.t<Ast.case<_>>, ctx: Context.t, ~loc): NonEmpty.t<ref<t>> => {
  NonEmpty.map(cases, ~f=(. {patterns, nodes}) => {
    let scopes = Queue.make()
    let casetypes = NonEmpty.map(patterns, ~f=(. pattern) => {
      NonEmpty.map(pattern, ~f=(. p) => {
        let (t, ctx) = Local.fromPattern(p, ctx)
        Queue.add(scopes, ctx.scope)
        t
      })
    })->NonEmpty.reduce3(~f=(. merged, pattern) => {
      NonEmpty.zipExn(merged, pattern)->NonEmpty.map(~f=(. (a, b)) => {
        unify(a, b, ~loc)
        a
      })
    })
    let scope = Queue.reduceU(scopes, MapString.empty, (. a, b) => {
      MapString.mergeU(a, b, (. _k, a, b) =>
        switch (a, b) {
        | (None, None) => None
        | (Some(_) as x, None) | (None, Some(_) as x) => x
        | (Some(a) as x, Some(b)) =>
          unify(a, b, ~loc)
          x
        }
      )
    })
    let ctx = {
      ...ctx,
      scope: scope,
    }
    ctx.global := checkNodes(nodes, ctx).global.contents
    casetypes
  })->unifyNestedNonEmpty(~loc)
}
and checkNodes = (nodes: Ast.nodes<_>, ctx: Context.t): Context.t => {
  Array.reduceU(nodes, ctx, (. ctx, node) =>
    switch node {
    | Text(_) => ctx
    | Echo(_, echoes) => unifyEchoes(echoes, ctx)
    | Component({loc, props, _}) =>
      let (t, ctx) = Global.fromPattern(#Object(loc, props), ctx)
      ignore(t) // We need to unify this with the Component's signature
      ctx
    | Match(loc, bindingArray, cases) =>
      let casetype = makeCase(cases, ctx, ~loc)
      Global.unifyMatchCases(bindingArray, casetype, ctx)
    | MapArray(loc, pattern, cases) =>
      let casetype = makeCase(cases, ctx, ~loc)
      Global.unifyMapArrayCases(pattern, casetype, ctx, ~loc)
    | MapDict(loc, pattern, cases) =>
      let casetype = makeCase(cases, ctx, ~loc)
      Global.unifyMapDictCases(pattern, casetype, ctx, ~loc)
    }
  )
}

let make = nodes => {
  let ctx = Context.make()
  checkNodes(nodes, ctx).global.contents
}

let rec validate = (types, json) =>
  switch (types.contents, Js.Json.classify(json)) {
  | (Polymorphic, _)
  | (Boolean, JSONTrue | JSONFalse)
  | (Int | Float, JSONNumber(_))
  | (String, JSONString(_))
  | (Echo, JSONNumber(_) | JSONString(_))
  | (Nullable(_), JSONNull) => ()
  | (Nullable(x), _) => validate(x, json)
  | (Array(x), JSONArray(json)) => Array.forEach(json, json => validate(x, json))
  | (Dict(x), JSONObject(json)) => json->Js.Dict.values->Array.forEach(json => validate(x, json))
  | (Tuple(x), JSONArray(json)) =>
    if Array.size(x.contents) == Array.size(json) {
      Array.zip(x.contents, json)->Array.forEach(((types, json)) => validate(types, json))
    } else {
      assert false
    }
  | (Object(x), JSONObject(json)) =>
    MapString.forEach(x.contents, (k, v) =>
      switch Js.Dict.get(json, k) {
      | None => assert false
      | Some(json) => validate(v, json)
      }
    )
  | _ => assert false
  }
