/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module T = Acutis_Types
module Array = Belt.Array
module Ast_Pattern = T.Ast_Pattern
module MapString = Belt.Map.String
module Queue = Belt.MutableQueue
module SetString = Belt.Set.String

exception Exit = Debug.Exit
exception Exit2(option<string>, option<string>)

module Pattern = {
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
    | TPat_Const(T.loc, constant)
    | TPat_Construct(T.loc, construct, option<t>)
    | TPat_Tuple(T.loc, array<t>)
    | TPat_Record(T.loc, array<(string, t)>)
    | TPat_Dict(T.loc, array<(string, t)>)
    | TPat_Var(T.loc, string) // any binding
    | TPat_OptionalVar(T.loc, string) // any binding, may not be set
    | TPat_Any(T.loc) // ignored wildcard _

  let rec makeList = (a, ty, ~tail) => {
    let r = ref(tail)
    for i in Array.size(a) - 1 downto 0 {
      let p = Array.getUnsafe(a, i)
      let loc = Ast_Pattern.toLocation(p)
      r := TPat_Construct(loc, TPat_List, Some(TPat_Tuple(loc, [make(p, ty), r.contents])))
    }
    r.contents
  }

  and make = (p: Ast_Pattern.t, ty) =>
    switch (p, ty) {
    | (#Null(l), Typescheme.Nullable(_)) => TPat_Construct(l, TPat_Nullable, None)
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
      makeList(l, contents, ~tail=make((tail :> Ast_Pattern.t), contents))
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
      let r = make_record(o, contents, ~loc=l)
      TPat_Record(l, MapString.toArray(r))
    | (#Binding(l, "_"), _) => TPat_Any(l)
    | (#Binding(l, b), Nullable(_)) => TPat_OptionalVar(l, b)
    | (#Binding(l, b), _) => TPat_Var(l, b)
    | _ => raise(Exit2(Js.Json.stringifyAny(p), Js.Json.stringifyAny(Typescheme.debug(ref(ty)))))
    }

  and make_record = (x, ty, ~loc) => {
    let r = MapString.fromArray(x)
    MapString.mergeU(r, ty, (. _, p, ty) =>
      switch (p, ty) {
      | (None, None) | (Some(_), None) => None
      | (Some(p), Some({contents})) => Some(make(p, contents))
      | (None, Some(_)) => Some(TPat_Any(loc))
      }
    )
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
    | TPat_Var(_, v) | TPat_OptionalVar(_, v) => v
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

module Ast = {
  type rec node<'a> =
    | TText(string, T.Ast.trim)
    // The first echo item that isn't null will be returned.
    | TEcho({loc: T.loc, nullables: array<T.Ast.Echo.t>, default: T.Ast.Echo.t})
    | TMatch(T.loc, NonEmpty.t<Pattern.t>, NonEmpty.t<case<'a>>)
    | TMapList(T.loc, Pattern.t, NonEmpty.t<case<'a>>)
    | TMapDict(T.loc, Pattern.t, NonEmpty.t<case<'a>>)
    | TComponent({
        loc: T.loc,
        name: string,
        props: array<(string, Pattern.t)>,
        children: array<(string, child<'a>)>,
        f: 'a,
      })
  and nodes<'a> = array<node<'a>>
  and case<'a> = {pats: NonEmpty.t<NonEmpty.t<Pattern.t>>, nodes: nodes<'a>}
  and child<'a> = TChildName(string) | TChildBlock(nodes<'a>)
  type t<'a> = {
    nodes: nodes<'a>,
    prop_types: Typescheme.props,
    child_types: Typescheme.Child.props,
  }
}

// If a type is incomplete, then it can be unified more liberally. (Unused).
// type complete = Complete | Incomplete

type mode = Expand | Narrow

@raises(Exit)
let rec unify = (tref1, tref2, mode, ~loc, ~name) =>
  switch (tref1.contents, tref2.contents) {
  | (Typescheme.Boolean, Typescheme.Boolean)
  | (Int, Int)
  | (Float, Float)
  | (String, String) => ()
  | (Unknown, t) => tref1 := t
  | (t, Unknown) => tref2 := t
  | (Echo, (Int | Float | String | Echo) as t) => tref1 := t
  | ((Int | Float | String) as t, Echo) => tref2 := t
  | (Nullable(t1), Nullable(t2)) => unify(t1, t2, mode, ~loc, ~name)
  | (List(t1), List(t2)) => unify(t1, t2, mode, ~loc, ~name)
  | (Dict(t1, ks1), Dict(t2, ks2)) =>
    let ks' = SetString.union(ks1.contents, ks2.contents)
    ks1 := ks'
    ks2 := ks'
    unify(t1, t2, mode, ~loc, ~name)
  | (Tuple(t1), Tuple(t2)) => unifyTuple(t1, t2, mode, ~loc, ~name)
  | (Record(t1), Record(t2)) =>
    switch mode {
    | Expand => unifyRecord_expand(t1, t2, ~loc, ~name)
    | Narrow => unifyRecord_narrow(t1, t2, ~loc, ~name)
    }
  | _ => raise(Exit(Debug2.typeMismatch(tref1, tref2, ~f=Typescheme.toString, ~loc, ~name)))
  }

@raises(Exit)
and unifyTuple = (t1, t2, mode, ~loc, ~name) => {
  if Array.size(t1.contents) == Array.size(t2.contents) {
    Array.zip(t1.contents, t2.contents)->Array.forEachU((. (a, b)) =>
      unify(a, b, mode, ~loc, ~name)
    )
  } else {
    raise(Exit(Debug2.tupleSizeMismatch(Array.size(t1.contents), Array.size(t2.contents), ~name)))
  }
}

@raises(Exit)
and unifyRecord_expand = (t1, t2, ~loc, ~name) => {
  let r = MapString.mergeU(t1.contents, t2.contents, (. _, v1, v2) => {
    switch (v1, v2) {
    | (Some(v1) as r, Some(v2)) =>
      unify(v1, v2, Expand, ~loc, ~name)
      r
    | (Some(_) as r, None) | (None, Some(_) as r) => r
    | (None, None) => None
    }
  })
  t1 := r
  t2 := r
}

@raises(Exit)
and unifyRecord_narrow = (t1, t2, ~loc, ~name) => {
  let r = MapString.mergeU(t1.contents, t2.contents, (. _, v1, v2) =>
    switch (v1, v2) {
    | (Some(v1) as r, Some(v2)) =>
      unify(v1, v2, Expand, ~loc, ~name)
      r
    | (Some(_), None) | (None, Some(_)) => None
    | (None, None) => None
    }
  )
  if MapString.isEmpty(r) {
    raise(Exit(Debug2.cantNarrowType(t1, t2, ~f=Typescheme.record_toString)))
  } else {
    t1 := r
    t2 := r
  }
}

@raises(Exit)
let unifyRecord_exact = (t1, t2, ~loc, ~comp, ~name) => {
  let r = MapString.mergeU(t1.contents, t2.contents, (. k, v1, v2) =>
    switch (v1, v2) {
    | (Some(v1) as r, Some(v2)) =>
      unify(v1, v2, Expand, ~loc, ~name)
      r
    | (None, Some(v)) =>
      raise(Exit(Debug2.missingProp(k, v, ~name, ~comp, ~loc, ~f=Typescheme.toString)))
    | (Some(_), None) | (None, None) => None
    }
  )
  t1 := r
  t2 := r
}

@raises(Exit)
let unify_child = (a, b) =>
  if Typescheme.Child.equal(a, b) {
    ()
  } else {
    raise(Exit(Debug2.childTypeMismatch(a, b, ~f=Typescheme.Child.toString)))
  }

module Context = {
  type t = {
    global: ref<MapString.t<Typescheme.t>>,
    scope: MapString.t<Typescheme.t>,
    children: ref<MapString.t<Typescheme.Child.t>>,
  }

  let make = () => {
    global: ref(MapString.empty),
    scope: MapString.empty,
    children: ref(MapString.empty),
  }

  @raises(Exit)
  let update = (ctx, k, v, ~loc, ~name) =>
    switch MapString.get(ctx.scope, k) {
    | None =>
      ctx.global :=
        MapString.updateU(ctx.global.contents, k, (. v') =>
          switch v' {
          | None => Some(v)
          | Some(v') as r =>
            unify(v', v, Expand, ~loc, ~name)
            r
          }
        )
    | Some(v') => unify(v', v, Expand, ~loc, ~name)
    }

  @raises(Exit)
  let updateChild = (ctx, k, v) =>
    ctx.children :=
      MapString.updateU(ctx.children.contents, k, (. v') =>
        switch v' {
        | None => Some(v)
        | Some(v') as r =>
          unify_child(v', v)
          r
        }
      )

  @raises(Exit)
  let addScope = (ctx, q, ~loc, ~name) => {
    // Merge all of the bindings into a single map & typecheck them.
    let newscope = Queue.reduceU(q, MapString.empty, (. newscope, (k, v)) => {
      MapString.updateU(newscope, k, (. v') =>
        switch v' {
        | None => Some(v)
        | Some(v') as r =>
          unify(v', v, Expand, ~loc, ~name)
          r
        }
      )
    })
    // Merge the new bindings with the outer scope & shadow duplicate names.
    let scope = MapString.mergeU(ctx.scope, newscope, (. _, a, b) =>
      switch (a, b) {
      | (None, None) => None
      | (Some(_) as x, None) | (None | Some(_), Some(_) as x) => x
      }
    )
    {...ctx, scope: scope}
  }
}

module Local = {
  @raises(Exit)
  let rec fromPattern = (pattern: Ast_Pattern.t, q, ~name) => {
    open Typescheme
    switch pattern {
    | #Null(_) => ref(Nullable(ref(Unknown)))
    | #Some(_, x) => ref(Nullable(fromPattern(x, q, ~name)))
    | #False(_) | #True(_) => ref(Boolean)
    | #String(_) => ref(String)
    | #Int(_) => ref(Int)
    | #Float(_) => ref(Float)
    | #Tuple(_, t) =>
      let types = Array.mapU(t, (. x) => fromPattern(x, q, ~name))
      ref(Tuple(ref(types)))
    | #Array(loc, a) =>
      let t = ref(Unknown)
      Array.forEachU(a, (. x) => unify(t, fromPattern(x, q, ~name), Expand, ~loc, ~name))
      ref(List(t))
    | #ArrayWithTailBinding(loc, a, #Binding(_, b)) =>
      let t = ref(Unknown)
      Array.forEachU(a, (. x) => unify(t, fromPattern(x, q, ~name), Expand, ~loc, ~name))
      let t = ref(List(t))
      Queue.add(q, (b, t))
      t
    | #Dict(loc, d) =>
      let t = ref(Unknown)
      Array.forEachU(d, (. (_, x)) => unify(t, fromPattern(x, q, ~name), Expand, ~loc, ~name))
      let ks = d->Array.mapU((. (k, _)) => k)->SetString.fromArray
      ref(Dict(t, ref(ks)))
    | #Object(_, o) =>
      let types = o->Array.mapU((. (k, x)) => {
        let types = fromPattern(x, q, ~name)
        (k, types)
      })
      let types = MapString.fromArray(types)
      ref(Record(ref(types)))
    | #Binding(_, "_") => ref(Unknown)
    | #Binding(_, b) =>
      let t = ref(Unknown)
      Queue.add(q, (b, t))
      t
    }
  }
}

module Global = {
  let updateContext = (ctx, k, v, ~loc, ~name) =>
    switch MapString.get(ctx.Context.scope, k) {
    | None =>
      switch MapString.get(ctx.global.contents, k) {
      | None =>
        ctx.global := MapString.set(ctx.global.contents, k, v)
        v
      | Some(v') =>
        unify(v, v', Expand, ~loc, ~name)
        v'
      }
    | Some(v') =>
      unify(v, v', Expand, ~loc, ~name)
      v'
    }

  @raises(Exit)
  let rec fromPattern = (~default, ~name, pattern: Ast_Pattern.t, ctx) => {
    open Typescheme
    let unknown = Typescheme.unknown
    switch pattern {
    | #Null(_) => ref(Nullable(ref(Unknown)))
    | #Some(_, x) => ref(Nullable(fromPattern(~default=unknown(), x, ctx, ~name)))
    | #False(_) | #True(_) => ref(Boolean)
    | #String(_) => ref(String)
    | #Int(_) => ref(Int)
    | #Float(_) => ref(Float)
    | #Tuple(_, t) =>
      let types = Array.mapU(t, (. x) => fromPattern(~default=unknown(), x, ctx, ~name))
      ref(Tuple(ref(types)))
    | #Array(loc, a) =>
      let t = ref(Unknown)
      Array.forEachU(a, (. x) =>
        unify(t, fromPattern(~default=t, x, ctx, ~name), Narrow, ~loc, ~name)
      )
      ref(List(t))
    | #ArrayWithTailBinding(loc, a, #Binding(bloc, b)) =>
      let t = ref(Unknown)
      Array.forEachU(a, (. x) =>
        unify(t, fromPattern(~default=t, x, ctx, ~name), Narrow, ~loc, ~name)
      )
      let t = ref(List(t))
      updateContext(ctx, b, t, ~loc=bloc, ~name)
    | #Dict(loc, d) =>
      let t = ref(Unknown)
      Array.forEachU(d, (. (_, x)) =>
        unify(t, fromPattern(~default=t, x, ctx, ~name), Narrow, ~loc, ~name)
      )
      let ks = d->Array.mapU((. (k, _)) => k)->SetString.fromArray
      ref(Dict(t, ref(ks)))
    | #Object(_, o) => ref(Record(fromPattern_record(o, ctx, ~name)))
    | #Binding(_, "_") => default
    | #Binding(loc, b) => updateContext(ctx, b, default, ~loc, ~name)
    }
  }

  and fromPattern_record = (x, ctx, ~name) => {
    let types = Array.mapU(x, (. (k, x)) => (
      k,
      fromPattern(~default=Typescheme.unknown(), x, ctx, ~name),
    ))
    ref(MapString.fromArray(types))
  }

  @raises(Exit)
  let unifyMatchCases2 = (bindingArray: NonEmpty.t<Ast_Pattern.binding>, cases, ctx, ~name) => {
    if NonEmpty.size(bindingArray) != NonEmpty.size(cases) {
      let #Binding(loc, _) = NonEmpty.hd(bindingArray)
      raise(Exit(Debug2.patternNumberMismatch(~loc, ~name)))
    } else {
      NonEmpty.zipExn(bindingArray, cases)->NonEmpty.map((. (#Binding(loc, k) as b, ty)) => {
        Context.update(ctx, k, ty, ~loc, ~name)
        Pattern.make(b, ty.contents)
      })
    }
  }
  @raises(Exit)
  let unifyMapArrayCases2 = (pat: T.Ast.mapArrayPattern, tys, ctx, ~loc, ~name) => {
    let int = Typescheme.int()
    let (ty, ty_index) = switch NonEmpty.toArray(tys) {
    | [hd] => (Typescheme.list(hd), int)
    | [hd, tl] => (Typescheme.list(hd), tl)
    | _ => raise(Exit(Debug2.mapPatternSizeMismatch(~loc, ~name)))
    }
    unify(ty_index, int, Expand, ~loc, ~name)
    switch pat {
    | #Binding(loc, binding) => Context.update(ctx, binding, ty, ~loc, ~name)
    | (#Array(loc, _) | #ArrayWithTailBinding(loc, _, _)) as a =>
      let a_t = fromPattern(~default=Typescheme.unknown(), a, ctx, ~name)
      unify(a_t, ty, Expand, ~loc, ~name)
    }
    Pattern.make((pat :> T.Ast_Pattern.t), ty.contents)
  }

  @raises(Exit)
  let unifyMapDictCases2 = (pat: T.Ast.mapDictPattern, tys, ctx, ~loc, ~name) => {
    let str = Typescheme.string()
    let (ty, ty_index) = switch NonEmpty.toArray(tys) {
    | [hd] => (Typescheme.dict(hd), str)
    | [hd, tl] => (Typescheme.dict(hd), tl)
    | _ => raise(Exit(Debug2.mapPatternSizeMismatch(~loc, ~name)))
    }
    unify(ty_index, str, Expand, ~loc, ~name)
    switch pat {
    | #Binding(loc, binding) => Context.update(ctx, binding, ty, ~loc, ~name)
    | #Dict(loc, _) as d =>
      let t = fromPattern(~default=Typescheme.unknown(), d, ctx, ~name)
      unify(t, ty, Expand, ~loc, ~name)
    }
    Pattern.make((pat :> T.Ast_Pattern.t), ty.contents)
  }
}

@raises(Exit)
let unifyEchoes = (nullables, default, ctx, ~name) => {
  open Typescheme

  @raises(Exit)
  let rec aux = i => {
    switch nullables[i] {
    | None =>
      switch default {
      | T.Ast.Echo.Binding(loc, binding, _) => Context.update(ctx, binding, echo(), ~loc, ~name)
      | Child(_, child) => Context.updateChild(ctx, child, Child.child())
      | String(_, _, _) | Int(_, _, _) | Float(_, _, _) => ()
      }
    | Some(T.Ast.Echo.Binding(loc, binding, _)) =>
      Context.update(ctx, binding, nullable(echo()), ~loc, ~name)
      aux(succ(i))
    | Some(String(_, _, _) | Int(_, _, _) | Float(_, _, _)) =>
      raise(Exit(Debug2.nonNullableEchoLiteral()))
    | Some(Child(_, child)) =>
      Context.updateChild(ctx, child, Child.nullable())
      aux(succ(i))
    }
  }
  aux(0)
}

@raises(Exit)
let unifyNestedNonEmpty = (cases: NonEmpty.t<NonEmpty.t<(_, _)>>, ~name) => {
  let (_, r) = NonEmpty.reduceHd(cases, (. casea, caseb) => {
    NonEmpty.zipByExn(casea, caseb, (. (_, casea), (loc, caseb)) => {
      unify(casea, caseb, Expand, ~loc, ~name)
      (loc, casea)
    })
  })->NonEmpty.unzip
  r
}

let getTypes = x =>
  switch x {
  | Source2.Acutis(_, {Ast.prop_types: prop_types, child_types, _}) => (prop_types, child_types)
  | Function(_, props, children, _) => (props, children)
  }

@raises(Exit)
let rec makeCases = (cases, ctx, ~loc, ~name, g) => {
  let (casetypes, cases) =
    cases
    ->NonEmpty.map((. {T.Ast.patterns: pats, nodes}) => {
      let bindings = Queue.make()
      let casetypes =
        pats
        ->NonEmpty.map((. pattern) =>
          NonEmpty.map(pattern, (. p) => (
            T.Ast_Pattern.toLocation(p),
            Local.fromPattern(p, bindings, ~name),
          ))
        )
        ->NonEmpty.reduceHd((. pat1, pat2) =>
          NonEmpty.zipByExn(pat1, pat2, (. (loc, a), (_, b)) => {
            unify(a, b, Expand, ~loc, ~name)
            (loc, a)
          })
        )
      let ctx = Context.addScope(ctx, bindings, ~loc, ~name)
      (casetypes, (pats, nodes, ctx))
    })
    ->NonEmpty.unzip
  // We need to make the typed patterns AFTER the entire expression is
  // type-checked so records and dictionary fields expand correctly.
  let casetypes = unifyNestedNonEmpty(casetypes, ~name)
  let cases = NonEmpty.map(cases, (. (pats, nodes, ctx)) => {
    Ast.pats: NonEmpty.map(pats, (. pats) =>
      NonEmpty.zipByExn(pats, casetypes, (. p, ty) => Pattern.make(p, ty.contents))
    ),
    nodes: makeNodes(nodes, ctx, ~name, g),
  })
  (casetypes, cases)
}

@raises(Exit)
and makeNodes = (nodes, ctx, ~name, g) =>
  Array.mapU(nodes, (. node) =>
    switch node {
    | T.Ast.Text(s, t) => Ast.TText(s, t)
    | Echo({loc, nullables, default}) =>
      unifyEchoes(nullables, default, ctx, ~name)->ignore
      Ast.TEcho({loc: loc, nullables: nullables, default: default})
    | Component({loc, props, children, name: cname, f: ()}) =>
      let (propTypes, propTypesChildren) = getTypes(Utils.Dagmap.getExn(g, ~name, ~key=cname, ~loc))
      let t = Global.fromPattern_record(props, ctx, ~name)
      // The original proptypes should not mutate.
      unifyRecord_exact(t, ref(Typescheme.copy_record(propTypes)), ~loc, ~comp=cname, ~name)
      let children =
        children
        ->MapString.fromArray
        ->MapString.mergeU(propTypesChildren, (. _, c, ty) =>
          switch (c, ty) {
          | (None, None) | (None, Some({contents: NullableChild})) => None
          | (None, Some({contents: Child})) => assert false // error message goes here
          | (Some(_), None) => assert false // error message goes here
          | (Some(ChildName(c)), Some({contents: ty})) =>
            Context.updateChild(ctx, c, ref(ty))
            Some(Ast.TChildName(c))
          | (Some(ChildBlock(nodes)), Some(_)) =>
            Some(TChildBlock(makeNodes(nodes, ctx, ~name=cname, g)))
          }
        )
        ->MapString.toArray
      let props = Pattern.make_record(props, t.contents, ~loc)->MapString.toArray
      TComponent({loc: loc, props: props, children: children, name: cname, f: ()})
    | Match(loc, bindingArray, cases) =>
      // Add a default wildcard for patterns without indices
      let (caseTypes, cases) = makeCases(cases, ctx, ~loc, ~name, g)
      let patterns = Global.unifyMatchCases2(bindingArray, caseTypes, ctx, ~name)
      TMatch(loc, patterns, cases)
    | MapArray(loc, pattern, cases) =>
      // Add a default wildcard for patterns without indices
      let cases = NonEmpty.map(cases, (. case) => {
        ...case,
        patterns: NonEmpty.map(case.patterns, (. pattern) =>
          switch NonEmpty.toArray(pattern) {
          | [hd] => NonEmpty.two(hd, #Binding(loc, "_"))
          | _ => pattern
          }
        ),
      })
      let (casetypes, cases) = makeCases(cases, ctx, ~loc, ~name, g)
      let pattern = Global.unifyMapArrayCases2(pattern, casetypes, ctx, ~loc, ~name)
      TMapList(loc, pattern, cases)
    | MapDict(loc, pattern, cases) =>
      // Add a default wildcard for patterns without indices
      let cases = NonEmpty.map(cases, (. case) => {
        ...case,
        patterns: NonEmpty.map(case.patterns, (. pattern) =>
          switch NonEmpty.toArray(pattern) {
          | [hd] => NonEmpty.two(hd, #Binding(loc, "_"))
          | _ => pattern
          }
        ),
      })
      let (casetypes, cases) = makeCases(cases, ctx, ~loc, ~name, g)
      let pattern = Global.unifyMapDictCases2(pattern, casetypes, ctx, ~loc, ~name)
      TMapDict(loc, pattern, cases)
    }
  )

and make = (name, ast, g) => {
  let ctx = Context.make()
  let nodes = makeNodes(ast, ctx, ~name, g)
  let ast = {
    Ast.nodes: nodes,
    prop_types: ctx.global.contents,
    child_types: ctx.children.contents,
  }
  ast
}

let makeSrc = (. g, x) =>
  switch x {
  | Source2.Acutis(name, ast) => Source2.src(~name, make(name, ast, g))
  | Function(name, p, c, f) => Source2.functionU(~name, p, c, f)
  }

let makeArray = a => a->Utils.Dagmap.make(~f=makeSrc)->Utils.Dagmap.link

let make = (name, ast, components) => make(name, ast, Utils.Dagmap.prelinked(components))
