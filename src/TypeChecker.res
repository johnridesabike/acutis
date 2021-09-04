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
module Debug2 = TypeChecker_Debug
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
  | List(x) => `list [${toString(x)}]`
  | Dict(x, _) => `dictionary <${toString(x)}>`
  | Tuple(x) =>
    let x = Array.joinWith(x.contents, ", ", toString)
    `tuple (${x})`
  | Record(x) =>
    let x =
      MapString.toArray(x.contents)->Array.joinWith(", ", ((k, v)) => `"${k}": ${toString(v)}`)
    `record {${x}}`
  }

module TypedPattern = {
  type types = t

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
    | TPat_Const(constant)
    | TPat_Construct(construct, option<t>)
    | TPat_Tuple(array<t>)
    | TPat_Record(array<(string, t)>)
    | TPat_Dict(array<(string, t)>)
    | TPat_Var(string)

  let rec makeList = (a, i, ty, ~tail) =>
    switch a[i] {
    | None => tail
    | Some(p) =>
      TPat_Construct(TPat_List, Some(TPat_Tuple([make(p, ty), makeList(a, succ(i), ty, ~tail)])))
    }

  and make = (p: T.Ast_Pattern.t, ty: types) =>
    switch (p, ty) {
    | (#Null(_), Nullable(_)) => TPat_Construct(TPat_Nullable, None)
    | (p, Nullable({contents})) =>
      TPat_Construct(TPat_Nullable, Some(TPat_Tuple([make(p, contents)])))
    | (#False(_), _) => TPat_Const(TPat_Bool(false))
    | (#True(_), _) => TPat_Const(TPat_Bool(true))
    | (#String(_, s), _) => TPat_Const(TPat_String(s))
    | (#Int(_, i), _) => TPat_Const(TPat_Int(i))
    | (#Float(_, f), _) => TPat_Const(TPat_Float(f))
    | (#Tuple(_, t), Tuple({contents})) =>
      TPat_Tuple(Array.zipByU(t, contents, (. p, ty) => make(p, ty.contents)))
    | (#Array(_, l), List({contents})) =>
      makeList(l, 0, contents, ~tail=TPat_Construct(TPat_List, None))
    | (#ArrayWithTailBinding(_, l, #Binding(_, b)), List({contents})) =>
      makeList(l, 0, contents, ~tail=TPat_Var(b))
    | (#Dict(_, d), Dict(tys, {contents: ks})) =>
      let ks = ks->SetString.toArray->Array.mapU((. k) => (k, tys))->MapString.fromArray
      let d =
        d
        ->MapString.fromArray
        ->MapString.mergeU(ks, (. _, p, ty) =>
          switch (p, ty) {
          | (None, None) | (Some(_), None) => None
          | (Some(p), Some({contents})) => Some(make(p, contents))
          | (None, Some(_)) => Some(TPat_Var("_"))
          }
        )
      TPat_Dict(MapString.toArray(d))
    | (#Object(_, o), Record({contents})) =>
      let r = MapString.fromArray(o)
      let r = MapString.mergeU(r, contents, (. _, p, ty) =>
        switch (p, ty) {
        | (None, None) | (Some(_), None) => None
        | (Some(p), Some({contents})) => Some(make(p, contents))
        | (None, Some(_)) => Some(TPat_Var("_"))
        }
      )
      TPat_Record(MapString.toArray(r))
    | (#Binding(_, b), _) => TPat_Var(b)
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
    | TPat_Const(x) => toStringConst(x)
    | TPat_Tuple(t) => "(" ++ Array.joinWith(t, ", ", toString) ++ ")"
    | TPat_Record(r) =>
      "{" ++ Array.joinWith(r, ", ", ((k, v)) => keyValuesToString(k, toString(v))) ++ "}"
    | TPat_Dict(r) =>
      "<" ++ Array.joinWith(r, ", ", ((k, v)) => keyValuesToString(k, toString(v))) ++ ">"
    | TPat_Var(v) => v
    | TPat_Construct(TPat_Nullable, None) => "null"
    | TPat_Construct(TPat_Nullable, Some(x)) => toString(x)
    | TPat_Construct(TPat_List, None) => "[]"
    | TPat_Construct(TPat_List, Some(l)) =>
      let rec aux = (s, ~sep, tl) =>
        switch tl {
        | TPat_Var(x) => `${s}${sep} ...${x}]`
        | TPat_Tuple([hd, TPat_Construct(_, None)]) => `${s}${sep} ${toString(hd)}]`
        | TPat_Tuple([hd, TPat_Construct(_, Some(tl))]) =>
          aux(s ++ sep ++ toString(hd), ~sep=",", tl)
        | _ => assert false
        }
      aux("[", ~sep="", l)
    }
}

module Ast2 = {
  type pattern = T.Ast_Pattern.t
  type case<'pat, 'a> = {pats: NonEmpty.t<NonEmpty.t<'pat>>, nodes: T.Ast.nodes<'a>}
  let make = c =>
    c->NonEmpty.map((. {T.Ast.patterns: patterns, nodes}) => {
      pats: patterns,
      nodes: nodes,
    })

  let toTyped = (c, tys) =>
    NonEmpty.map(c, (. {pats, nodes}) => {
      pats: NonEmpty.map(pats, (. pats) =>
        NonEmpty.zipBy(pats, tys, (. p, ty) => TypedPattern.make(p, ty.contents))
      ),
      nodes: nodes,
    })
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

// Until we upgrade the AST
let fixEchoes = a => {
  let hd = NonEmpty.hd(a)
  let q = Queue.make()
  let rec aux = (hd, i) =>
    switch NonEmpty.get(a, i) {
    | None => (Queue.toArray(q), hd)
    | Some(next) =>
      Queue.add(q, hd)
      aux(next, succ(i))
    }
  aux(hd, 1)
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

let unifyNestedNonEmpty = (cases, ~loc) =>
  NonEmpty.reduceHd(cases, (. casea, caseb) => {
    NonEmpty.zipBy(casea, caseb, (. casea, caseb) => {
      unify(casea, caseb, ~loc)
      casea
    })
  })

let rec makeCaseTypes = (cases, ctx, ~loc) => {
  cases
  ->NonEmpty.map((. {Ast2.pats: pats, nodes}) => {
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
    | Echo(_, echoes) => unifyEchoes(echoes, ctx)
    | Component({loc, props, _}) =>
      let (t, ctx) = Global.fromPattern(#Object(loc, props), ctx)
      ignore(t) // We need to unify this with the Component's signature
      ctx
    | Match(loc, bindingArray, cases) =>
      let cases = Ast2.make(cases)
      let casetypes = makeCaseTypes(cases, ctx, ~loc)
      //let typedcases = Ast2.toTyped(cases, casetypes)
      Global.unifyMatchCases(bindingArray, casetypes, ctx)
    | MapArray(loc, pattern, cases) =>
      let cases = Ast2.make(cases)
      let casetypes = makeCaseTypes(cases, ctx, ~loc)
      Global.unifyMapArrayCases(pattern, casetypes, ctx, ~loc)
    | MapDict(loc, pattern, cases) =>
      let cases = Ast2.make(cases)
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
  let casetypes = makeCaseTypes(cases, ctx, ~loc=Loc(0))
  Ast2.toTyped(cases, casetypes)
}
