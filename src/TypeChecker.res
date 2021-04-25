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
module MapString = Belt.Map.String
module Queue = Belt.MutableQueue

module NonEmpty = {
  include Acutis_Types.NonEmpty

  //let hd = (NonEmpty(hd, _)) => hd

  //let eq = (NonEmpty(hd1, tl1), NonEmpty(hd2, tl2), ~f) => f(. hd1, hd2) && Array.eqU(tl1, tl2, f)

  let zipExn = (NonEmpty(hd1, tl1), NonEmpty(hd2, tl2)) =>
    if Array.size(tl1) == Array.size(tl2) {
      NonEmpty((hd1, hd2), Array.zip(tl1, tl2))
    } else {
      assert false
    }

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
  // 0 and 1 sized tuples are legal.
  | Tuple(ref<array<ref<t>>>)
  | Dict(ref<t>)
  | Record(ref<MapString.t<ref<t>>>)
// The discriminant field, common field, and variant fields cannot intersect.
//| UnionStr({discriminant: string, common: MapString.t<t>, variants: MapString.t<MapString.t<t>>})
//| UnionInt({discriminant: string, common: MapString.t<t>, variants: MapInt.t<MapString.t<t>>})

module Child = {
  type t = Child | NullableChild
  let unify = (a: ref<t>, b: ref<t>) =>
    if a.contents == b.contents {
      ()
    } else {
      assert false
    }
}

type rec debug = [
  | #Polymorphic
  | #Boolean
  | #Int
  | #Float
  | #String
  | #Echo
  | #Nullable(debug)
  | #Array(debug)
  | #Tuple(array<debug>)
  | #Dict(debug)
  | #Record(array<(string, debug)>)
]
let rec debug = (x): debug =>
  switch x.contents {
  | Polymorphic => #Polymorphic
  | Boolean => #Boolean
  | Int => #Int
  | Float => #Float
  | String => #String
  | Echo => #Echo
  | Nullable(x) => #Nullable(debug(x))
  | Array(x) => #Array(debug(x))
  | Tuple(x) => #Tuple(Array.map(x.contents, debug))
  | Dict(x) => #Dict(debug(x))
  | Record(x) => #Record(MapString.map(x.contents, debug)->MapString.toArray)
  }

exception Fail(debug, debug)

let rec unify = (tref1, tref2): unit =>
  switch (tref1.contents, tref2.contents) {
  | (Boolean, Boolean)
  | (Int, Int)
  | (Float, Float)
  | (String, String) => ()
  | (Polymorphic, t) => tref1 := t
  | (t, Polymorphic) => tref2 := t
  | (Echo, (Int | Float | String | Echo) as t) => tref1 := t
  | ((Int | Float | String) as t, Echo) => tref2 := t
  | (Nullable(t1), Nullable(t2)) => unify(t1, t2)
  | (Nullable(t), _) =>
    unify(t, tref2)
    tref2 := tref1.contents
  | (_, Nullable(t)) =>
    unify(t, tref1)
    tref1 := tref2.contents
  | (Array(t1), Array(t2)) | (Dict(t1), Dict(t2)) => unify(t1, t2)
  | (Tuple(t1), Tuple(t2)) => unifyTuple(t1, t2)
  | (Record(t1), Record(t2)) => unifyRecord(t1, t2)
  | _ =>
    let d1 = debug(tref1)
    let d2 = debug(tref2)
    raise(Fail(d1, d2))
  }
and unifyTuple = (t1, t2) => {
  if Array.size(t1.contents) == Array.size(t2.contents) {
    Array.zip(t1.contents, t2.contents)->Array.forEachU((. (a, b)) => unify(a, b))
  } else {
    assert false
  }
}
and unifyRecord = (t1, t2) => {
  let r = MapString.mergeU(t1.contents, t2.contents, (. _, v1, v2) =>
    switch (v1, v2) {
    | (Some(v1) as r, Some(v2)) =>
      unify(v1, v2)
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

  let set = (ctx, k, v) =>
    updateAux(ctx, k, ~f=(. v') =>
      switch v' {
      | None => Some(v)
      | Some(v') as r =>
        unify(v', v)
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
    | #Array(_, a) =>
      let t = switch a[0] {
      | None => ref(Polymorphic)
      | Some(hd) => fromPattern(hd, ctx)
      }
      Array.forEachU(a, (. x) => unify(t, fromPattern(x, ctx)))
      ref(Array(t))
    | #ArrayWithTailBinding(_, a, #Binding(_, b)) =>
      let t = switch a[0] {
      | None => ref(Polymorphic)
      | Some(hd) => fromPattern(hd, ctx)
      }
      Array.forEachU(a, (. x) => unify(t, fromPattern(x, ctx)))
      let t = ref(Array(t))
      ctx := Context.setLocal(ctx.contents, b, t)
      t
    | #Dict(_, d) =>
      let t = switch d[0] {
      | None => ref(Polymorphic)
      | Some((_, hd)) => fromPattern(hd, ctx)
      }
      Array.forEachU(d, (. (_, x)) => unify(t, fromPattern(x, ctx)))
      ref(Dict(t))
    | #Object(_, o) =>
      let types = o->Array.mapU((. (k, x)) => {
        let types = fromPattern(x, ctx)
        (k, types)
      })
      let types = MapString.fromArray(types)
      ref(Record(ref(types)))
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
    | #Array(_, a) =>
      let t = switch a[0] {
      | None => ref(Polymorphic)
      | Some(hd) => fromPattern(hd, q)
      }
      Array.forEachU(a, (. x) => unify(t, fromPattern(x, q)))
      ref(Array(t))
    | #ArrayWithTailBinding(_, a, #Binding(_, b)) =>
      let t = switch a[0] {
      | None => ref(Polymorphic)
      | Some(hd) => fromPattern(hd, q)
      }
      Array.forEachU(a, (. x) => unify(t, fromPattern(x, q)))
      let t = ref(Array(t))
      Queue.add(q, (b, t))
      t
    | #Dict(_, d) =>
      let t = switch d[0] {
      | None => ref(Polymorphic)
      | Some((_, hd)) => fromPattern(hd, q)
      }
      Array.forEachU(d, (. (_, x)) => unify(t, fromPattern(x, q)))
      ref(Dict(t))
    | #Object(_, o) =>
      let types = o->Array.mapU((. (k, x)) => {
        let types = fromPattern(x, q)
        (k, types)
      })
      let types = MapString.fromArray(types)
      ref(Record(ref(types)))
    | #Binding(_, b) =>
      let t = ref(Polymorphic)
      Queue.add(q, (b, t))
      t
    }
  let fromPattern = (pattern, ctx: Context.t) => {
    let q = Queue.make()
    let t = fromPattern(pattern, q)
    let ctx = Queue.reduceU(q, ctx, (. ctx, (k, v)) =>
      switch MapString.get(ctx.scope, k) {
      | None =>
        ctx.global :=
          MapString.updateU(ctx.global.contents, k, (. x) =>
            switch x {
            | None => Some(v)
            | Some(v') as vopt =>
              unify(v, v')
              vopt
            }
          )
        ctx
      | Some(v') =>
        unify(v, v')
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
      (#Binding(_, k), t),
    ) => Context.set(ctx, k, t))
  }

  let unifyMapCases = (
    id: Ast.mapPattern,
    NonEmpty(case_hd, case_tl): NonEmpty.t<ref<t>>,
    ctx: Context.t,
  ): Context.t => {
    let int = ref(Int)
    let index = switch case_tl {
    | [] => int
    | [tl] => tl
    | _ => assert false
    }
    unify(index, int)
    switch id {
    | #Binding(_, binding) => Context.set(ctx, binding, ref(Array(case_hd)))
    | (#Array(_) | #ArrayWithTailBinding(_)) as a =>
      let (a_t, _) = fromPattern(a, ctx)
      unify(a_t, ref(Array(case_hd)))
      ctx
    | #Dict(_) as d =>
      let (t, _) = fromPattern(d, ctx)
      unify(t, ref(Dict(case_hd)))
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
      | Binding(_, binding, _) => Context.set(ctx, binding, ref(Echo))
      | Child(_, child) =>
        Context.setChild(ctx, child, ref(Child.Child))
        ctx
      | String(_, _, _) | Int(_, _, _) | Float(_, _, _) => ctx
      }
    | Some(Binding(_, binding, _)) =>
      let ctx = Context.set(ctx, binding, ref(Nullable(ref(Echo))))
      aux(succ(i), ctx)
    | Some(String(_, _, _) | Int(_, _, _) | Float(_, _, _)) => assert false
    | Some(Child(_, child)) =>
      Context.setChild(ctx, child, ref(Child.NullableChild))
      aux(succ(i), ctx)
    }
  }
  aux(0, ctx)
}

let unifyNestedNonEmpty = (cases): NonEmpty.t<ref<t>> => {
  let x = NonEmpty.reduce3(cases, ~f=(. merged, case) => {
    NonEmpty.zipExn(merged, case)->NonEmpty.map(~f=(. (casea, caseb)) => {
      unify(casea, caseb)
      casea
    })
  })
  x
}

let rec makeCase = (cases: NonEmpty.t<Ast.case<_>>, ctx: Context.t): NonEmpty.t<ref<t>> => {
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
        unify(a, b)
        a
      })
    })
    let scope = Queue.reduceU(scopes, MapString.empty, (. a, b) => {
      MapString.mergeU(a, b, (. _k, a, b) =>
        switch (a, b) {
        | (None, None) => None
        | (Some(_) as x, None) | (None, Some(_) as x) => x
        | (Some(a) as x, Some(b)) =>
          unify(a, b)
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
  })->unifyNestedNonEmpty
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
    | Match(_, bindingArray, cases) =>
      let casetype = makeCase(cases, ctx)
      Global.unifyMatchCases(bindingArray, casetype, ctx)
    | Map(_, pattern, cases) =>
      let casetype = makeCase(cases, ctx)
      Global.unifyMapCases(pattern, casetype, ctx)
    }
  )
}

let make = nodes => {
  let ctx = Context.make()
  checkNodes(nodes, ctx).global.contents
}
