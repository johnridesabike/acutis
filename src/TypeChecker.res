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
module T = Acutis_Types
module Array = Belt.Array
module Ast = T.Ast
module Ast_Pattern = T.Ast_Pattern
module Debug2 = TypeChecker_Debug
module MapString = Belt.Map.String
module Queue = Belt.MutableQueue
module Option = Belt.Option

exception Exit = Debug.Exit
exception Exit2(string)

module NonEmpty = {
  include T.NonEmpty

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
  | Array(x) => `array [${toString(x)}]`
  | Dict(x) => `dictionary <${toString(x)}>`
  | Tuple(x) =>
    let x = Array.joinWith(x.contents, ", ", toString)
    `tuple (${x})`
  | Record(x) =>
    let x =
      MapString.toArray(x.contents)->Array.joinWith(", ", ((k, v)) => `"${k}": ${toString(v)}`)
    `record {${x}}`
  }

module Exhaustive = {
  type types = t

  module FloatSet = {
    module Id = Belt.Id.MakeComparableU({
      type t = float
      let cmp = (. a: t, b: t) => compare(a, b)
    })
    type t = Belt.Set.t<Id.t, Id.identity>
    let empty = Belt.Set.make(~id=module(Id))
  }

  type rec t =
    | Exhaustive
    | Polymorphic
    | Echo
    | True
    | False
    | Int(Belt.Set.Int.t)
    | Float(FloatSet.t)
    | String(Belt.Set.String.t)
    | Nullable(t)
    | Array
    | ArrayWithTailBinding
    | Dict
    | Tuple(tupleTree)
    | Record(recordTree)
  and tupleTree =
    | TupleNil
    | TupleNode({val: t, child: tupleTree, sibling: tupleTree})
  and recordTree =
    | RecordNil
    | RecordNode({key: string, val: t, child: recordTree, sibling: recordTree})

  let rec getUnusedInt = (s, i) =>
    if Belt.Set.Int.has(s, i) {
      getUnusedInt(Belt.Set.Int.remove(s, i), succ(i))
    } else {
      i
    }

  let rec getUnusedFloat = (s, i) =>
    if Belt.Set.has(s, i) {
      getUnusedFloat(Belt.Set.remove(s, i), i +. 0.125)
    } else {
      i
    }

  /*
  let rec isExhaustiveAux = (t: t'): option<Ast_Pattern.t> =>
    switch t {
    | Polymorphic | Echo => Some(#Binding(Loc(1), "_"))
    | Boolean(Nil) => None
    | Boolean(TrueAndFalse) => Some(#True(Loc(0)))
    | Boolean(True) => Some(#False(Loc(0)))
    | Boolean(False) => Some(#True(Loc(0)))
    | Int(s) => Some(#Int(Loc(0), getUnusedInt(s, 0)))
    | Float(s) => Some(#Float(Loc(0), getUnusedFloat(s, 0.0)))
    | String(_s) => Some(#Binding(Loc(1), "_"))
    | Nullable(_, NotNull) => Some(#Null(Loc(0)))
    | Nullable(t, Null) => isExhaustive(t)
    | Array(_a) => Some(#Array(Loc(0), [#Binding(T.Loc(0), "_")]))
    | ArrayWithTailBinding(l) => isExhaustiveArray(l)
    | Dict(_) => Some(#Dict(Loc(0), [("a", #Binding(T.Loc(0), "_"))]))
    | Tuple(t) => isExhaustiveTuple(t, 0)
    | Record(t) => isExhaustiveRecord(t)
    }
  and isExhaustiveArray = l =>
    Belt.List.map(l, a => Array.keepMap(a, isExhaustive))
    ->Belt.List.keep(a => Array.size(a) != 0)
    ->Belt.List.head
    ->Belt.Option.map(_ =>
      #ArrayWithTailBinding(T.Loc(0), [#Binding(T.Loc(0), "_")], #Binding(T.Loc(0), "_"))
    )
  and isExhaustiveTuple = (a, i) =>
    switch a[i] {
    | None => None
    | Some(t) =>
      switch isExhaustive(t) {
      | None => isExhaustiveTuple(a, succ(i))
      | Some(_) as e => e
      }
    }
  and isExhaustiveRecord = m =>
    switch MapString.minimum(m) {
    | None => None
    | Some((k, v)) =>
      switch isExhaustive(v) {
      | None => isExhaustiveRecord(MapString.remove(m, k))
      | Some(_) as e => e
      }
    }
  and isExhaustive = (t): option<Ast_Pattern.t> =>
    switch t {
    | Binding => None
    | Structure(t) => isExhaustiveAux(t)
    }
 */

  let rec make = (p: Ast_Pattern.t): t =>
    switch p {
    | #Binding(_) => Exhaustive
    | #True(_) => True
    | #False(_) => False
    | #Int(_, i) => Int(Belt.Set.Int.empty->Belt.Set.Int.add(i))
    | #Float(_, f) => Float(FloatSet.empty->Belt.Set.add(f))
    | #String(_, s) => String(Belt.Set.String.empty->Belt.Set.String.add(s))
    | #Null(_) => Nullable(Polymorphic)
    | #Array(_, _a) => Array
    | #ArrayWithTailBinding(_, _a, _) => ArrayWithTailBinding
    | #Dict(_, _d) => Dict
    | #Tuple(_, t) => Tuple(makeTupleTree(t, 0, ~sibling=TupleNil))
    | #Object(_, a) => Record(makeRecordTree(MapString.fromArray(a), ~sibling=RecordNil))
    }
  and makeTupleTree = (a, i, ~sibling) =>
    switch a[i] {
    | None => TupleNil
    | Some(x) =>
      TupleNode({
        val: make(x),
        child: makeTupleTree(a, succ(i), ~sibling=TupleNil),
        sibling: sibling,
      })
    }
  and makeRecordTree = (d, ~sibling) =>
    switch MapString.minimum(d) {
    | None => RecordNil
    | Some((k, v)) =>
      RecordNode({
        key: k,
        val: make(v),
        child: makeRecordTree(MapString.remove(d, k), ~sibling=RecordNil),
        sibling: sibling,
      })
    }

  exception Fail(t, t)

  let rec unify = (a: t, b: t) =>
    switch (a, b) {
    | (Exhaustive, _)
    | (True, True)
    | (False, False) =>
      None
    | (_, Exhaustive) | (True, False) | (False, True) => Some(Exhaustive)
    | (Int(a), Int(b)) =>
      let c = Belt.Set.Int.union(a, b)
      if Belt.Set.Int.eq(a, c) {
        None
      } else {
        Some(Int(c))
      }
    | (Float(a), Float(b)) =>
      let c = Belt.Set.union(a, b)
      if Belt.Set.eq(a, c) {
        None
      } else {
        Some(Float(c))
      }
    | (String(a), String(b)) =>
      let c = Belt.Set.String.union(a, b)
      if Belt.Set.String.eq(a, c) {
        None
      } else {
        Some(String(c))
      }
    | (Nullable(a), b) =>
      switch unify(a, b) {
      | None => None
      | Some(c) => Some(Nullable(c))
      }
    | (a, Nullable(_)) => Some(Nullable(a))
    | (Tuple(a), Tuple(b)) =>
      switch unifyTuple(a, b) {
      | None => None
      | Some(t) =>
        switch makeTupleExhaustive(t) {
        | None => Some(Tuple(t))
        | Some(_) as t => t
        }
      }
    | _ => assert false
    }

  and unifyTuple = (a, b) => {
    switch (a, b) {
    | (TupleNil, TupleNil) => Some(TupleNil)
    | (TupleNode(a), TupleNode(b) as bnode) =>
      switch unify(a.val, b.val) {
      // The value doesn't unify (its pattern is already covered).
      // Follow this existing value's children.
      | None =>
        switch unifyTuple(a.child, b.child) {
        // The children weren't able to unify either
        | None => None
        // The children unified, so they're a new pattern and create a new
        // child tree
        | Some(child) => Some(TupleNode({val: a.val, child: child, sibling: bnode}))
        }
      // The value unified, so this is a new pattern.
      | Some(_) =>
        // Compare it to the next sibling on this level of the tree.
        unifyTupleLevel(a.sibling, bnode)
      }
    | (TupleNil, TupleNode(_)) | (TupleNode(_), TupleNil) => assert false
    }
  }
  and unifyTupleLevel = (a, b) =>
    switch (a, b) {
    | (TupleNil | TupleNode(_), TupleNil) => Some(TupleNil)
    | (TupleNil, TupleNode(b)) => Some(TupleNode(b))
    | (TupleNode(a), TupleNode(b) as bNode) =>
      switch unify(a.val, b.val) {
      // The value doesn't unify (its pattern is already covered).
      // Follow this existing value's children.
      | None =>
        switch unifyTuple(a.child, b.child) {
        // The children weren't able to unify either.
        | None => None
        // The children unified, so they're a new pattern and create a new child tree.
        | Some(child) => Some(TupleNode({val: a.val, child: child, sibling: TupleNode(b)}))
        }
      // The value unified, so this is a new pattern.
      | Some(_) =>
        // Compare it to the next sibling.
        switch unifyTupleLevel(a.sibling, bNode) {
        | None => None
        | Some(sibling) => Some(TupleNode({...a, sibling: sibling}))
        }
      }
    }

  /*
  (true, true)
  (true, false)
  ->
  TupleNode({
    val: True,
    child: TupleNode({
      val: True,
      child: TupleNil,
      sibling: TupleNode({
        val: False,
        child: TupleNil,
        sibling: TupleNil,
      })
    }),
    sibling: TupleNil,
  })
  ->
  TupleNode({
    val: True,
    child: TupleNode({
      val: Exhaustive, // val & sibling are collapsed into Exhaustive
      val: TupleNil,
      sibling: TupleNil,
    }),
    sibling: TupleNil
  })
 */

  and makeTupleLevelExhaustive = (t, tree) =>
    switch tree {
    | TupleNil => Some(t)
    | TupleNode({val, child: TupleNil, sibling}) =>
      switch unify(t, val) {
      | None => None
      | Some(Exhaustive) as x => x
      | Some(t) => makeTupleLevelExhaustive(t, sibling)
      }
    | TupleNode({val, child, sibling}) =>
      switch makeTupleExhaustive(child) {
      | Some(Exhaustive) =>
        switch unify(t, val) {
        | None => None
        | Some(Exhaustive) as x => x
        | Some(t) => makeTupleLevelExhaustive(t, sibling)
        }
      | Some(_) | None => None
      }
    }
  and makeTupleExhaustive = tree =>
    switch tree {
    | TupleNil => None
    | TupleNode({val, child: TupleNil, sibling}) =>
      switch makeTupleLevelExhaustive(val, sibling) {
      | Some(Exhaustive) as x => x
      | Some(_) | None => None
      }
    | TupleNode({val, child, sibling}) =>
      switch makeTupleExhaustive(child) {
      | Some(Exhaustive) =>
        switch makeTupleLevelExhaustive(val, sibling) {
        | Some(Exhaustive) as x => x
        | Some(_) | None => None
        }
      | None | Some(_) => None
      }
    }

  let rec unifyPattern = (a: t, b: Ast_Pattern.t): option<t> =>
    switch (a, b) {
    | (Exhaustive, _)
    | (True, #True(_))
    | (False, #False(_))
    | (Nullable(_), #Null(_)) =>
      None
    | (_, #Binding(_)) | (True, #False(_)) | (False, #True(_)) => Some(Exhaustive)
    | (Int(a), #Int(_, b)) =>
      let c = Belt.Set.Int.add(a, b)
      if Belt.Set.Int.eq(a, c) {
        None
      } else {
        Some(Int(c))
      }
    | (Float(a), #Float(_, b)) =>
      let c = Belt.Set.add(a, b)
      if Belt.Set.eq(a, c) {
        None
      } else {
        Some(Float(c))
      }
    | (String(a), #String(_, b)) =>
      let c = Belt.Set.String.add(a, b)
      if Belt.Set.String.eq(a, c) {
        None
      } else {
        Some(String(c))
      }
    | (Nullable(a), b) =>
      switch unifyPattern(a, b) {
      | None => None
      | Some(c) => Some(Nullable(c))
      }
    | (a, #Null(_)) => Some(Nullable(a))
    | (Tuple(a), #Tuple(_, b)) =>
      switch unifyTuplePattern(a, b) {
      | None => None
      | Some(t) =>
        switch makeTupleExhaustive(t) {
        | None => Some(Tuple(t))
        | Some(_) as t => t
        }
      }
    | _ => assert false
    }
  and unifyTuplePattern = (tree, pats) => {
    let rec aux = (tree, pat_opt, i) =>
      switch (tree, pat_opt) {
      // We've reached the end of the tree and still have a pattern.
      // Return a new tree for this pattern.
      | (TupleNil, Some(_)) => Some(makeTupleTree(pats, i, ~sibling=tree))
      // We've reached the end of the tree and the end of the pattern.
      | (TupleNil, None) => None
      // We've reached a node. Compare the tuple's current value to it.
      | (TupleNode({val, child, sibling}), Some(pat)) =>
        switch unifyPattern(val, pat) {
        // The value doesn't unify (its pattern is already covered).
        // Follow this existing value's children
        | None =>
          let i = succ(i)
          switch aux(child, pats[i], i) {
          // the children weren't able to unify either.
          | None => None
          // The children unified, so they're a new pattern and create a new
          // children tree
          | Some(child) => Some(TupleNode({val: val, child: child, sibling: sibling}))
          }
        // The value unified, so it's a new pattern
        | Some(_) =>
          // Compare it to the rest of the items on this level of the tree.
          switch aux(sibling, pat_opt, i) {
          //
          | None => None
          | Some(sibling) => Some(TupleNode({val: val, child: child, sibling: sibling}))
          }
        }
      | (TupleNode(_), None) => assert false
      }
    aux(tree, pats[0], 0)
  }

  let make = (NonEmpty(hd, tl): NonEmpty.t<Ast_Pattern.t>) => {
    let hd = make(hd)
    let rec aux = (acc, i) =>
      switch tl[i] {
      | None => Some(acc)
      | Some(pat) =>
        switch unify(acc, make(pat)) {
        | None => None
        | Some(acc) => aux(acc, succ(i))
        }
      }
    aux(hd, 0)
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

// If a type is incomplete, then it can be unified more liberally.
type complete = Complete | Incomplete

let rec unify = (tref1, tref2, complete, ~loc) =>
  switch (tref1.contents, tref2.contents) {
  | (Boolean, Boolean)
  | (Int, Int)
  | (Float, Float)
  | (String, String) => ()
  | (Polymorphic, t) => tref1 := t
  | (t, Polymorphic) => tref2 := t
  | (Echo, (Int | Float | String | Echo) as t) => tref1 := t
  | ((Int | Float | String) as t, Echo) => tref2 := t
  | (Nullable(t1), Nullable(t2)) => unify(t1, t2, complete, ~loc)
  | (Nullable(t), _) if complete == Incomplete =>
    unify(t, tref2, complete, ~loc)
    tref2 := tref1.contents
  | (_, Nullable(t)) if complete == Incomplete =>
    unify(t, tref1, complete, ~loc)
    tref1 := tref2.contents
  | (Array(t1), Array(t2)) | (Dict(t1), Dict(t2)) => unify(t1, t2, complete, ~loc)
  | (Tuple(t1), Tuple(t2)) => unifyTuple(t1, t2, complete, ~loc)
  | (Record(t1), Record(t2)) => unifyRecord(t1, t2, complete, ~loc)
  | _ => raise(Exit(Debug2.typeMismatch(tref1, tref2, ~f=toString, ~loc)))
  }
and unifyTuple = (t1, t2, complete, ~loc) => {
  if Array.size(t1.contents) == Array.size(t2.contents) {
    Array.zip(t1.contents, t2.contents)->Array.forEachU((. (a, b)) => unify(a, b, complete, ~loc))
  } else {
    raise(Exit(Debug2.tupleSizeMismatch(Array.size(t1.contents), Array.size(t2.contents))))
  }
}
and unifyRecord = (t1, t2, complete, ~loc) => {
  let r = MapString.mergeU(t1.contents, t2.contents, (. _, v1, v2) =>
    switch (v1, v2) {
    | (Some(v1) as r, Some(v2)) =>
      unify(v1, v2, complete, ~loc)
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

  let set = (ctx, k, v, complete, ~loc) =>
    updateAux(ctx, k, ~f=(. v') =>
      switch v' {
      | None => Some(v)
      | Some(v') as r =>
        unify(v', v, complete, ~loc)
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
      Array.forEachU(a, (. x) => unify(t, fromPattern(x, ctx), Incomplete, ~loc))
      ref(Array(t))
    | #ArrayWithTailBinding(loc, a, #Binding(_, b)) =>
      let t = switch a[0] {
      | None => ref(Polymorphic)
      | Some(hd) => fromPattern(hd, ctx)
      }
      Array.forEachU(a, (. x) => unify(t, fromPattern(x, ctx), Incomplete, ~loc))
      let t = ref(Array(t))
      ctx := Context.setLocal(ctx.contents, b, t)
      t
    | #Dict(loc, d) =>
      let t = switch d[0] {
      | None => ref(Polymorphic)
      | Some((_, hd)) => fromPattern(hd, ctx)
      }
      Array.forEachU(d, (. (_, x)) => unify(t, fromPattern(x, ctx), Incomplete, ~loc))
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
    | #Array(loc, a) =>
      let t = switch a[0] {
      | None => ref(Polymorphic)
      | Some(hd) => fromPattern(hd, q)
      }
      Array.forEachU(a, (. x) => unify(t, fromPattern(x, q), Complete, ~loc))
      ref(Array(t))
    | #ArrayWithTailBinding(loc, a, #Binding(bloc, b)) =>
      let t = switch a[0] {
      | None => ref(Polymorphic)
      | Some(hd) => fromPattern(hd, q)
      }
      Array.forEachU(a, (. x) => unify(t, fromPattern(x, q), Complete, ~loc))
      let t = ref(Array(t))
      Queue.add(q, (bloc, b, t))
      t
    | #Dict(loc, d) =>
      let t = switch d[0] {
      | None => ref(Polymorphic)
      | Some((_, hd)) => fromPattern(hd, q)
      }
      Array.forEachU(d, (. (_, x)) => unify(t, fromPattern(x, q), Complete, ~loc))
      ref(Dict(t))
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
              unify(v, v', Complete, ~loc)
              vopt
            }
          )
        ctx
      | Some(v') =>
        unify(v, v', Complete, ~loc)
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
    ) => Context.set(ctx, k, t, Complete, ~loc))
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
    unify(index, int, Complete, ~loc)
    switch id {
    | #Binding(loc, binding) => Context.set(ctx, binding, ref(Array(case_hd)), Complete, ~loc)
    | (#Array(loc, _) | #ArrayWithTailBinding(loc, _, _)) as a =>
      let (a_t, _) = fromPattern(a, ctx)
      unify(a_t, ref(Array(case_hd)), Complete, ~loc)
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
    unify(index, int, Complete, ~loc)
    switch id {
    | #Binding(loc, binding) => Context.set(ctx, binding, ref(Array(case_hd)), Complete, ~loc)
    | #Dict(loc, _) as d =>
      let (t, _) = fromPattern(d, ctx)
      unify(t, ref(Dict(case_hd)), Complete, ~loc)
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
      | Binding(loc, binding, _) => Context.set(ctx, binding, ref(Echo), Complete, ~loc)
      | Child(_, child) =>
        Context.setChild(ctx, child, ref(Child.Child))
        ctx
      | String(_, _, _) | Int(_, _, _) | Float(_, _, _) => ctx
      }
    | Some(Binding(loc, binding, _)) =>
      let ctx = Context.set(ctx, binding, ref(Nullable(ref(Echo))), Complete, ~loc)
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
      unify(casea, caseb, Incomplete, ~loc)
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
        unify(a, b, Incomplete, ~loc)
        a
      })
    })
    let scope = Queue.reduceU(scopes, MapString.empty, (. a, b) => {
      MapString.mergeU(a, b, (. _k, a, b) =>
        switch (a, b) {
        | (None, None) => None
        | (Some(_) as x, None) | (None, Some(_) as x) => x
        | (Some(a) as x, Some(b)) =>
          unify(a, b, Incomplete, ~loc)
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
  | (Record(x), JSONObject(json)) =>
    MapString.forEach(x.contents, (k, v) =>
      switch Js.Dict.get(json, k) {
      | None => assert false
      | Some(json) => validate(v, json)
      }
    )
  | _ => assert false
  }
