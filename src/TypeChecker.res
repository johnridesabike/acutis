/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module T = Acutis_Types
module Array = Belt.Array
module Ast = T.Ast
module Ast_Pattern = T.Ast_Pattern
module MapString = Belt.Map.String
module Queue = Belt.MutableQueue
module Option = Belt.Option
module SetString = Belt.Set.String

exception Exit = Debug.Exit
exception Exit2(string)

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
  | List(ref<t>)
  | Dict(ref<t>, ref<SetString.t>)
  // 0 and 1 sized tuples are legal.
  | Tuple(ref<array<ref<t>>>)
  | Record(ref<MapString.t<ref<t>>>)
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
  | List(x) => `[${toString(x)}]`
  | Dict(x, _) => `<${toString(x)}>`
  | Tuple(x) =>
    let x = Array.joinWith(x.contents, ", ", toString)
    `(${x})`
  | Record(x) =>
    let x =
      MapString.toArray(x.contents)->Array.joinWith(", ", ((k, v)) => `"${k}": ${toString(v)}`)
    `{${x}}`
  }

module TypedPattern = {
  type types = t
  type loc = T.loc

  type constant =
    | TPat_Bool(bool)
    | TPat_String(string)
    | TPat_Int(int)
    | TPat_Float(float)

  let compareConst = (a, b) =>
    switch (a, b) {
    | (TPat_String(a), TPat_String(b)) => compare(a, b)
    | (TPat_Float(a), TPat_Float(b)) => compare(a, b)
    | (TPat_Int(a), TPat_Int(b)) => compare(a, b)
    | (TPat_Bool(a), TPat_Bool(b)) => compare(a, b)
    | _ => assert false
    }

  let eqConst = (a, b) =>
    switch (a, b) {
    | (TPat_String(a), TPat_String(b)) => a == b
    | (TPat_Float(a), TPat_Float(b)) => a == b
    | (TPat_Int(a), TPat_Int(b)) => a == b
    | (TPat_Bool(a), TPat_Bool(b)) => a == b
    | _ => assert false
    }

  let toStringConst = x =>
    switch x {
    | TPat_Bool(true) => "true"
    | TPat_Bool(false) => "false"
    | TPat_String(s) => `"${s}"`
    | TPat_Int(i) => Belt.Int.toString(i)
    | TPat_Float(f) => Belt.Float.toString(f)
    }

  type construct = TPat_List | TPat_Nullable

  type rec t =
    | TPat_Const(loc, constant)
    | TPat_Construct(loc, construct, option<t>)
    | TPat_Tuple(loc, array<t>)
    | TPat_Record(loc, array<(string, t)>)
    | TPat_Dict(loc, array<(string, t)>)
    | TPat_Var(loc, string) // any binding
    | TPat_Any(loc) // ignored wildcard _

  let rec makeList = (a, ty, ~tail) => {
    let r = ref(tail)
    for i in Array.size(a) - 1 downto 0 {
      let p = Array.getUnsafe(a, i)
      let loc = T.Ast_Pattern.toLocation(p)
      r := TPat_Construct(loc, TPat_List, Some(TPat_Tuple(loc, [make(p, ty), r.contents])))
    }
    r.contents
  }

  and make = (p: T.Ast_Pattern.t, ty: types) =>
    switch (p, ty) {
    | (#Null(l), Nullable(_)) => TPat_Construct(l, TPat_Nullable, None)
    | (#Some(l, p), Nullable({contents})) =>
      TPat_Construct(l, TPat_Nullable, Some(TPat_Tuple(l, [make(p, contents)])))
    | (#False(l), _) => TPat_Const(l, TPat_Bool(false))
    | (#True(l), _) => TPat_Const(l, TPat_Bool(true))
    | (#String(l, s), _) => TPat_Const(l, TPat_String(s))
    | (#Int(l, i), _) => TPat_Const(l, TPat_Int(i))
    | (#Float(l, f), _) => TPat_Const(l, TPat_Float(f))
    | (#Tuple(l, t), Tuple({contents})) =>
      TPat_Tuple(l, Array.zipByU(t, contents, (. p, ty) => make(p, ty.contents)))
    | (#Array(l, a), List({contents})) =>
      makeList(a, contents, ~tail=TPat_Construct(l, TPat_List, None))
    | (#ArrayWithTailBinding(_, l, tail), List({contents})) =>
      makeList(l, contents, ~tail=make((tail :> T.Ast_Pattern.t), contents))
    | (#Dict(l, d), Dict(tys, {contents: ks})) =>
      let ks = ks->SetString.toArray->Array.mapU((. k) => (k, tys))->MapString.fromArray
      let d =
        d
        ->MapString.fromArray
        ->MapString.mergeU(ks, (. _, p, ty) =>
          switch (p, ty) {
          | (None, None) | (Some(_), None) => None
          | (Some(p), Some({contents})) => Some(make(p, contents))
          | (None, Some(_)) => Some(TPat_Any(l))
          }
        )
      TPat_Dict(l, MapString.toArray(d))
    | (#Object(l, o), Record({contents})) =>
      let r = MapString.fromArray(o)
      let r = MapString.mergeU(r, contents, (. _, p, ty) =>
        switch (p, ty) {
        | (None, None) | (Some(_), None) => None
        | (Some(p), Some({contents})) => Some(make(p, contents))
        | (None, Some(_)) => Some(TPat_Any(l))
        }
      )
      TPat_Record(l, MapString.toArray(r))
    | (#Binding(l, "_"), _) => TPat_Any(l)
    | (#Binding(l, b), _) => TPat_Var(l, b)
    | _ => assert false
    }

  let keyValuesToString = (k, v) =>
    if v == k {
      v
    } else {
      k ++ ": " ++ v
    }

  let rec toString = x =>
    switch x {
    | TPat_Const(_, x) => toStringConst(x)
    | TPat_Tuple(_, t) => "(" ++ Array.joinWith(t, ", ", toString) ++ ")"
    | TPat_Record(_, r) =>
      "{" ++ Array.joinWith(r, ", ", ((k, v)) => keyValuesToString(k, toString(v))) ++ "}"
    | TPat_Dict(_, r) =>
      "<" ++ Array.joinWith(r, ", ", ((k, v)) => keyValuesToString(k, toString(v))) ++ ">"
    | TPat_Var(_, v) => v
    | TPat_Construct(_, TPat_Nullable, None) => "null"
    | TPat_Construct(_, TPat_Nullable, Some(x)) => toString(x)
    | TPat_Construct(_, TPat_List, None) => "[]"
    | TPat_Construct(_, TPat_List, Some(l)) =>
      let rec aux = (s, ~sep, l) =>
        switch l {
        | TPat_Tuple(_, [hd, TPat_Construct(_, _, Some(tl))]) =>
          aux(s ++ sep ++ toString(hd), ~sep=", ", tl)
        | TPat_Tuple(_, [hd, TPat_Construct(_, _, None)]) => `${s}${sep}${toString(hd)}]`
        | TPat_Tuple(_, [hd, tl]) => `${s}${sep}${toString(hd)},...${toString(tl)}]`
        | l => `${s}${sep}...${toString(l)}]`
        }
      aux("[", ~sep="", l)
    | TPat_Any(_) => "_"
    }
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

// If a type is incomplete, then it can be unified more liberally. (Unused).
// type complete = Complete | Incomplete

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
  | (List(t1), List(t2)) => unify(t1, t2, ~loc)
  | (Dict(t1, ks1), Dict(t2, ks2)) =>
    let ks' = SetString.union(ks1.contents, ks2.contents)
    ks1 := ks'
    ks2 := ks'
    unify(t1, t2, ~loc)
  | (Tuple(t1), Tuple(t2)) => unifyTuple(t1, t2, ~loc)
  | (Record(t1), Record(t2)) => unifyRecord(t1, t2, ~loc)
  | _ => raise(Exit(Debug2.typeMismatch(tref1, tref2, ~f=toString, ~loc)))
  }
and unifyTuple = (t1, t2, ~loc) => {
  if Array.size(t1.contents) == Array.size(t2.contents) {
    Array.zip(t1.contents, t2.contents)->Array.forEachU((. (a, b)) => unify(a, b, ~loc))
  } else {
    raise(Exit(Debug2.tupleSizeMismatch(Array.size(t1.contents), Array.size(t2.contents))))
  }
}
and unifyRecord = (t1, t2, ~loc) => {
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
    | #Some(_, x) => ref(Nullable(fromPattern(x, ctx)))
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
      ref(List(t))
    | #ArrayWithTailBinding(loc, a, #Binding(_, b)) =>
      let t = switch a[0] {
      | None => ref(Polymorphic)
      | Some(hd) => fromPattern(hd, ctx)
      }
      Array.forEachU(a, (. x) => unify(t, fromPattern(x, ctx), ~loc))
      let t = ref(List(t))
      ctx := Context.setLocal(ctx.contents, b, t)
      t
    | #Dict(loc, d) =>
      let t = switch d[0] {
      | None => ref(Polymorphic)
      | Some((_, hd)) => fromPattern(hd, ctx)
      }
      Array.forEachU(d, (. (_, x)) => unify(t, fromPattern(x, ctx), ~loc))
      let ks = d->Array.mapU((. (k, _)) => k)->SetString.fromArray
      ref(Dict(t, ref(ks)))
    | #Object(_, o) =>
      let types = o->Array.mapU((. (k, x)) => {
        let types = fromPattern(x, ctx)
        (k, types)
      })
      let types = MapString.fromArray(types)
      ref(Record(ref(types)))
    | #Binding(_, "_") => ref(Polymorphic)
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
    | #Some(_, x) => ref(Nullable(fromPattern(x, q)))
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
      ref(List(t))
    | #ArrayWithTailBinding(loc, a, #Binding(bloc, b)) =>
      let t = switch a[0] {
      | None => ref(Polymorphic)
      | Some(hd) => fromPattern(hd, q)
      }
      Array.forEachU(a, (. x) => unify(t, fromPattern(x, q), ~loc))
      let t = ref(List(t))
      Queue.add(q, (bloc, b, t))
      t
    | #Dict(loc, d) =>
      let t = switch d[0] {
      | None => ref(Polymorphic)
      | Some((_, hd)) => fromPattern(hd, q)
      }
      Array.forEachU(d, (. (_, x)) => unify(t, fromPattern(x, q), ~loc))
      let ks = d->Array.mapU((. (k, _)) => k)->SetString.fromArray
      ref(Dict(t, ref(ks)))
    | #Object(_, o) =>
      let types = o->Array.mapU((. (k, x)) => {
        let types = fromPattern(x, q)
        (k, types)
      })
      let types = MapString.fromArray(types)
      ref(Record(ref(types)))
    | #Binding(_, "_") => ref(Polymorphic)
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

  let unifyMatchCases = (bindingArray: NonEmpty.t<Ast_Pattern.binding>, cases, ctx) => {
    NonEmpty.zip(bindingArray, cases)->NonEmpty.reduce(ctx, (. ctx, (#Binding(loc, k), t)) =>
      Context.set(ctx, k, t, ~loc)
    )
  }

  let unifyMapArrayCases = (id: Ast.mapArrayPattern, cases, ctx, ~loc): Context.t => {
    let int = ref(Int)
    let (case_hd, index) = switch cases->NonEmpty.toArray {
    | [hd] => (hd, int)
    | [hd, tl] => (hd, tl)
    | _ => raise(Exit(Debug2.mapPatternSizeMismatch(~loc)))
    }
    unify(index, int, ~loc)
    switch id {
    | #Binding(loc, binding) => Context.set(ctx, binding, ref(List(case_hd)), ~loc)
    | (#Array(loc, _) | #ArrayWithTailBinding(loc, _, _)) as a =>
      let (a_t, _) = fromPattern(a, ctx)
      unify(a_t, ref(List(case_hd)), ~loc)
      ctx
    }
  }

  let unifyMapDictCases = (id: Ast.mapDictPattern, case, ctx, ~loc): Context.t => {
    let int = ref(String)
    let (case_hd, index) = switch NonEmpty.toArray(case) {
    | [hd] => (hd, int)
    | [hd, tl] => (hd, tl)
    | _ => raise(Exit(Debug2.mapPatternSizeMismatch(~loc)))
    }
    unify(index, int, ~loc)
    switch id {
    | #Binding(loc, binding) => Context.set(ctx, binding, ref(List(case_hd)), ~loc)
    | #Dict(loc, _) as d =>
      let (t, _) = fromPattern(d, ctx)
      unify(t, ref(Dict(case_hd, ref(SetString.empty))), ~loc)
      ctx
    }
  }
}

let unifyEchoes = (nullables, default, ctx): Context.t => {
  let rec aux = (i, ctx) => {
    switch nullables[i] {
    | None =>
      switch default {
      | T.Ast.Echo.Binding(loc, binding, _) => Context.set(ctx, binding, ref(Echo), ~loc)
      | Child(_, child) =>
        Context.setChild(ctx, child, ref(Child.Child))
        ctx
      | String(_, _, _) | Int(_, _, _) | Float(_, _, _) => ctx
      }
    | Some(T.Ast.Echo.Binding(loc, binding, _)) =>
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

let unifyNestedNonEmpty = (cases, ~loc) =>
  NonEmpty.reduceHd(cases, (. casea, caseb) => {
    NonEmpty.zipBy(casea, caseb, (. casea, caseb) => {
      unify(casea, caseb, ~loc)
      casea
    })
  })

let rec makeCaseTypes = (cases, ctx, ~loc) => {
  cases
  ->NonEmpty.map((. {T.Ast2.pats: pats, nodes}) => {
    let scopes = Queue.make()
    let casetypes = NonEmpty.map(pats, (. pattern) => {
      NonEmpty.map(pattern, (. p) => {
        let (t, ctx) = Local.fromPattern(p, ctx)
        Queue.add(scopes, ctx.scope)
        t
      })
    })->NonEmpty.reduceHd((. pat1, pat2) => {
      NonEmpty.zip(pat1, pat2)->NonEmpty.map((. (a, b)) => {
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
    let ctx = {...ctx, scope: scope}
    //ctx.global := checkNodes(nodes, ctx).global.contents
    checkNodes(nodes, ctx)->ignore
    casetypes
  })
  ->unifyNestedNonEmpty(~loc)
}
and checkNodes = (nodes: Ast.nodes<_>, ctx: Context.t): Context.t => {
  Array.reduceU(nodes, ctx, (. ctx, node) =>
    switch node {
    | Text(_) => ctx
    | Echo({loc: _, nullables, default}) => unifyEchoes(nullables, default, ctx)
    | Component({loc, props, _}) =>
      let (t, ctx) = Global.fromPattern(#Object(loc, props), ctx)
      ignore(t) // We need to unify this with the Component's signature
      ctx
    | Match(loc, bindingArray, cases) =>
      let cases = T.Ast2.makeCase(cases)
      let casetypes = makeCaseTypes(cases, ctx, ~loc)
      //let typedcases = Ast2.toTyped(cases, casetypes)
      Global.unifyMatchCases(bindingArray, casetypes, ctx)
    | MapArray(loc, pattern, cases) =>
      let cases = T.Ast2.makeCase(cases)
      let casetypes = makeCaseTypes(cases, ctx, ~loc)
      Global.unifyMapArrayCases(pattern, casetypes, ctx, ~loc)
    | MapDict(loc, pattern, cases) =>
      let cases = T.Ast2.makeCase(cases)
      let casetypes = makeCaseTypes(cases, ctx, ~loc)
      Global.unifyMapDictCases(pattern, casetypes, ctx, ~loc)
    }
  )
}

let make = nodes => {
  let ctx = Context.make()
  checkNodes(nodes, ctx).global.contents
}

let makeTypedCases = cases => {
  let ctx = Context.make()
  let tys = makeCaseTypes(cases, ctx, ~loc=Loc(0))
  NonEmpty.map(cases, (. {pats, nodes}) => {
    T.Ast2.pats: NonEmpty.map(pats, (. pats) =>
      NonEmpty.zipBy(pats, tys, (. p, ty) => TypedPattern.make(p, ty.contents))
    ),
    nodes: nodes,
  })
}
