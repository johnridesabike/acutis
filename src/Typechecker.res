/**
  Copyright (c) 2021 John Jackson. 

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Array = Belt.Array
module UPat = Parser.Pattern
module MapString = Belt.Map.String
module Queue = Belt.MutableQueue
module SetString = Belt.Set.String
module Ty = Typescheme

exception Exit = Debug.Exit

type mode =
  | Expand_both // Record a and b both take on each other's fields.
  | Expand_left_to_right // Record a expands into b only.
  | Narrow_left // Record a is narrowed to a subset of a and b.

@raises(Exit)
let rec unify = (tref1, tref2, mode, debug) =>
  switch (tref1.contents, tref2.contents) {
  | (Ty.Boolean, Ty.Boolean) | (Int, Int) | (Float, Float) | (String, String) => ()
  | (Unknown, t) => tref1 := t
  | (t, Unknown) => tref2 := t
  | (Echo, (Int | Float | String | Echo) as t) => tref1 := t
  | ((Int | Float | String) as t, Echo) => tref2 := t
  | (Nullable(t1), Nullable(t2)) => unify(t1, t2, mode, debug)
  | (List(t1), List(t2)) => unify(t1, t2, mode, debug)
  | (Dict(t1, ks1), Dict(t2, ks2)) =>
    let ks' = SetString.union(ks1.contents, ks2.contents)
    ks1 := ks'
    ks2 := ks'
    unify(t1, t2, mode, debug)
  | (Tuple(t1), Tuple(t2)) =>
    if Array.size(t1.contents) == Array.size(t2.contents) {
      Array.zipByU(t1.contents, t2.contents, (. a, b) => unify(a, b, mode, debug))->ignore
    } else {
      raise(Exit(Debug.tupleSizeMismatch(debug, Array.size(t1.contents), Array.size(t2.contents))))
    }
  | (Record(t1), Record(t2)) =>
    switch mode {
    | Expand_both => unifyRecord_expand_bidirectional(t1, t2, debug)
    | Expand_left_to_right => unifyRecord_expand_leftToRight(t1, t2, debug)
    | Narrow_left => unifyRecord_narrow_left(t1, t2, debug)
    }
  | _ => raise(Exit(Debug.typeMismatch(debug, tref1, tref2, Ty.toString)))
  }

@raises(Exit)
and unifyRecord_expand_bidirectional = (a, b, debug) => {
  let r = MapString.mergeU(a.contents, b.contents, (. _, v1, v2) => {
    switch (v1, v2) {
    | (Some(v1) as x, Some(v2)) =>
      unify(v1, v2, Expand_both, debug)
      x
    | (Some(_) as x, None) | (None, Some(_) as x) => x
    | (None, None) => None
    }
  })
  a := r
  b := r
}

@raises(Exit)
and unifyRecord_expand_leftToRight = (a, b, debug) => {
  let r = MapString.mergeU(a.contents, b.contents, (. _, v1, v2) => {
    switch (v1, v2) {
    | (Some(v1) as x, Some(v2)) =>
      unify(v1, v2, Expand_left_to_right, debug)
      x
    | (Some(_) as x, None) | (None, Some(_) as x) => x
    | (None, None) => None
    }
  })
  b := r
}

@raises(Exit)
and unifyRecord_narrow_left = (a, b, debug) => {
  let r = MapString.mergeU(a.contents, b.contents, (. _, v1, v2) =>
    switch (v1, v2) {
    | (Some(v1) as x, Some(v2)) =>
      unify(v1, v2, Narrow_left, debug)
      x
    | (Some(_), None) | (None, Some(_)) | (None, None) => None
    }
  )
  if MapString.isEmpty(r) {
    raise(Exit(Debug.cantNarrowType(debug, a, b, Ty.record_toString)))
  } else {
    a := r
  }
}

module Pattern = {
  type constant =
    | TBool(bool)
    | TString(string)
    | TInt(int)
    | TFloat(float)

  let toStringConst = x =>
    switch x {
    | TBool(true) => "true"
    | TBool(false) => "false"
    | TString(s) => `"${s}"`
    | TInt(i) => Belt.Int.toString(i)
    | TFloat(f) => Belt.Float.toString(f)
    }

  type construct = TList | TNullable

  type rec t =
    | TConst(Debug.t, constant)
    | TConstruct(Debug.t, construct, option<t>)
    | TTuple(Debug.t, array<t>)
    | TRecord(Debug.t, Belt.Map.String.t<t>, ref<Belt.Map.String.t<Typescheme.t>>)
    | TDict(Debug.t, Belt.Map.String.t<t>, ref<Belt.Set.String.t>)
    | TVar(Debug.t, string) // any binding
    | TOptionalVar(Debug.t, string) // any binding, may not be set
    | TAny(Debug.t) // ignored wildcard _

  type mode = Construct | Destruct

  @raises(Exit)
  let rec make = (umode, mode, ~f, pat, ty) =>
    switch pat {
    | UPat.UInt(dbg, i) =>
      unify(ty, Ty.int(), umode, dbg)
      TConst(dbg, TInt(i))
    | UString(dbg, s) =>
      unify(ty, Ty.string(), umode, dbg)
      TConst(dbg, TString(s))
    | UFloat(dbg, x) =>
      unify(ty, Ty.float(), umode, dbg)
      TConst(dbg, TFloat(x))
    | UBool(dbg, b) =>
      unify(ty, Ty.boolean(), umode, dbg)
      TConst(dbg, TBool(b))
    | UNullable(dbg, pat) =>
      let tyvar = switch ty.contents {
      | Nullable(ty) => ty
      | Unknown => Ty.unknown()
      | _ => raise(Exit(Debug.typeMismatch(dbg, ty, Ty.nullable(Ty.unknown()), Ty.toString)))
      }
      let pat = switch pat {
      | None => None
      | Some(pat) => Some(TTuple(dbg, [make(~f, umode, mode, pat, tyvar)]))
      }
      unify(ty, Ty.nullable(tyvar), umode, dbg)
      TConstruct(dbg, TNullable, pat)
    | UList(dbg, a, tail) =>
      let tyvar = switch ty.contents {
      | List(ty) => ty
      | Unknown => Ty.unknown()
      | _ => raise(Exit(Debug.typeMismatch(dbg, ty, Ty.list(Ty.unknown()), Ty.toString)))
      }
      let tl = switch tail {
      | None => TConstruct(dbg, TList, None)
      | Some(UBinding(_) as tail) => make(umode, mode, tail, ty, ~f)
      | Some(_) => raise(Exit(Debug.tailBindingClash(dbg)))
      }
      unify(ty, Ty.list(tyvar), umode, dbg)
      make_list(umode, mode, a, 0, tyvar, ~tl, ~f)
    | UTuple(dbg, a) =>
      let new_tyvars = ref(Array.mapU(a, (. _) => Ty.unknown()))
      let tyvars = switch ty.contents {
      | Tuple(tys) => tys
      | Unknown => new_tyvars
      | _ => raise(Exit(Debug.typeMismatch(dbg, ty, ref(Ty.Tuple(new_tyvars)), Ty.toString)))
      }
      unify(ty, ref(Ty.Tuple(tyvars)), umode, dbg)
      TTuple(dbg, Array.zipByU(a, tyvars.contents, (. pat, ty) => make(umode, mode, pat, ty, ~f)))
    | URecord(dbg, m) =>
      let new_tyvars = MapString.mapU(m, (. _) => Ty.unknown())->ref
      let tyvars = switch ty.contents {
      | Record(tys) => tys
      | Unknown => new_tyvars
      | _ => raise(Exit(Debug.typeMismatch(dbg, ty, ref(Ty.Record(new_tyvars)), Ty.toString)))
      }
      unify(ty, ref(Ty.Record(new_tyvars)), umode, dbg)
      let r = switch mode {
      | Construct => make_record(umode, m, tyvars.contents, ~f, dbg)
      | Destruct => make_record_destructure(umode, m, tyvars.contents, ~f, dbg)
      }
      TRecord(dbg, r, tyvars)
    | UDict(dbg, m) =>
      let new_kys = ref(SetString.empty)
      let (tyvar, kys) = switch ty.contents {
      | Dict(ty, kys) => (ty, kys)
      | Unknown => (Ty.unknown(), new_kys)
      | _ => raise(Exit(Debug.typeMismatch(dbg, ty, Ty.dict(Ty.unknown()), Ty.toString)))
      }
      unify(ty, ref(Ty.Dict(tyvar, new_kys)), umode, dbg)
      let d = MapString.mapU(m, (. pat) => make(umode, mode, pat, tyvar, ~f))
      TDict(dbg, d, kys)
    | UBinding(dbg, "_") =>
      switch mode {
      | Construct => raise(Exit(Debug.underscoreInConstruct(dbg)))
      | Destruct => TAny(dbg)
      }
    | UBinding(dbg, b) =>
      f(. b, ty, dbg)
      TVar(dbg, b)
    }

  @raises(Exit)
  and make_list = (umode, mode, ~tl, ~f, a, i, ty) =>
    switch a[i] {
    | None => tl
    | Some(p) =>
      let dbg = UPat.debug(p)
      let hd = make(umode, mode, ~f, p, ty)
      let tl = make_list(umode, mode, ~tl, ~f, a, succ(i), ty)
      TConstruct(dbg, TList, Some(TTuple(dbg, [hd, tl])))
    }

  @raises(Exit)
  and make_record = (mode, m, tyvars, ~f, dbg) => {
    MapString.mergeU(m, tyvars, (. k, pat, ty) =>
      switch (pat, ty) {
      | (Some(_), None) | (None, None) => None
      | (Some(pat), Some(ty)) => Some(make(mode, Construct, pat, ty, ~f))
      | (None, Some(ty)) => raise(Exit(Debug.missingRecordField(dbg, k, ty, Ty.toString)))
      }
    )
  }

  @raises(Exit)
  and make_record_destructure = (umode, m, tyvars, ~f, dbg) => {
    MapString.mergeU(m, tyvars, (. _, pat, ty) =>
      switch (pat, ty) {
      | (Some(pat), None) => Some(make(umode, Destruct, pat, Ty.unknown(), ~f))
      | (Some(pat), Some(ty)) => Some(make(umode, Destruct, pat, ty, ~f))
      | (None, Some(_)) => Some(TAny(dbg))
      | (None, None) => None
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
    | TConst(_, x) => toStringConst(x)
    | TTuple(_, t) => "(" ++ Array.joinWith(t, ", ", toString) ++ ")"
    | TRecord(_, r, _) =>
      let a = MapString.toArray(r)
      "{" ++ Array.joinWith(a, ", ", ((k, v)) => keyValuesToString(k, toString(v))) ++ "}"
    | TDict(_, r, _) =>
      let a = MapString.toArray(r)
      "<" ++ Array.joinWith(a, ", ", ((k, v)) => keyValuesToString(k, toString(v))) ++ ">"
    | TVar(_, v) | TOptionalVar(_, v) => v
    | TConstruct(_, TNullable, None) => "null"
    | TConstruct(_, TNullable, Some(x)) => toString(x)
    | TConstruct(_, TList, None) => "[]"
    | TConstruct(_, TList, Some(l)) =>
      let rec aux = (s, ~sep, l) =>
        switch l {
        | TTuple(_, [hd, TConstruct(_, _, Some(tl))]) =>
          aux(s ++ sep ++ toString(hd), ~sep=", ", tl)
        | TTuple(_, [hd, TConstruct(_, _, None)]) => `${s}${sep}${toString(hd)}]`
        | TTuple(_, [hd, tl]) => `${s}${sep}${toString(hd)},...${toString(tl)}]`
        | l => `${s}${sep}...${toString(l)}]`
        }
      aux("[", ~sep="", l)
    | TAny(_) => "_"
    }

  let debug = t =>
    switch t {
    | TConst(d, _)
    | TConstruct(d, _, _)
    | TTuple(d, _)
    | TRecord(d, _, _)
    | TDict(d, _, _)
    | TVar(d, _)
    | TOptionalVar(d, _)
    | TAny(d) => d
    }
}

type rec node =
  | TText(string, Parser.trim)
  // The first echo item that isn't null will be returned.
  | TEcho(Debug.t, array<Parser.echo>, Parser.echo)
  | TMatch(Debug.t, NonEmpty.t<Pattern.t>, NonEmpty.t<case>)
  | TMapList(Debug.t, Pattern.t, NonEmpty.t<case>)
  | TMapDict(Debug.t, Pattern.t, NonEmpty.t<case>)
  | TComponent(Debug.t, string, MapString.t<Pattern.t>, MapString.t<child>)

and nodes = array<node>

and case = {pats: NonEmpty.t<NonEmpty.t<Pattern.t>>, nodes: nodes}

and child = TChildName(string) | TChildBlock(nodes)

type t = {
  nodes: nodes,
  prop_types: Ty.props,
  child_types: Ty.Child.props,
}

@raises(Exit)
let unify_child = (a, b, debug) =>
  if Ty.Child.equal(a, b) {
    ()
  } else {
    raise(Exit(Debug.childTypeMismatch(debug, a, b, Ty.Child.toString)))
  }

type root = [#Root | #Component]

module Context = {
  type t = {
    global: ref<MapString.t<Ty.t>>,
    scope: MapString.t<Ty.t>,
    children: ref<MapString.t<Ty.Child.t>>,
    root: root,
  }

  let make = root => {
    global: ref(MapString.empty),
    scope: MapString.empty,
    children: ref(MapString.empty),
    root: root,
  }

  @raises(Exit)
  let update = ({scope, global, _}, k, v, debug) =>
    switch MapString.get(scope, k) {
    | None =>
      global :=
        MapString.updateU(global.contents, k, (. v') =>
          switch v' {
          | None => Some(v)
          | Some(v') as r =>
            unify(v', v, Narrow_left, debug)
            r
          }
        )
    | Some(v') => unify(v', v, Narrow_left, debug)
    }

  @raises(Exit)
  let updateChild = ({root, children, _}, k, v, debug) =>
    switch root {
    | #Root => raise(Exit(Debug.childNotAllowedInRoot(debug)))
    | #Component =>
      children :=
        MapString.updateU(children.contents, k, (. v') =>
          switch v' {
          | None => Some(v)
          | Some(v') as r =>
            unify_child(v', v, debug)
            r
          }
        )
    }

  @raises(Exit)
  let addScope = (ctx, q) => {
    let newscope = Queue.reduceU(q, MapString.empty, (. newscope, (k, v, debug)) =>
      if MapString.has(newscope, k) {
        raise(Exit(Debug.nameBoundMultipleTimes(debug, k)))
      } else {
        MapString.set(newscope, k, v)
      }
    )
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

@raise(Exit)
let updateContext = (ctx, . k, v, debug) =>
  switch MapString.get(ctx.Context.scope, k) {
  | None =>
    switch MapString.get(ctx.global.contents, k) {
    | None => ctx.global := MapString.set(ctx.global.contents, k, v)
    | Some(v') => unify(v, v', Expand_left_to_right, debug)
    }
  | Some(v') => unify(v, v', Expand_left_to_right, debug)
  }

@raises(Exit)
let unifyMatchCases = (bindingArray, tys, ctx) => {
  if NonEmpty.size(bindingArray) != NonEmpty.size(tys) {
    let debug = NonEmpty.hd(bindingArray)->UPat.debug
    raise(Exit(Debug.patternNumberMismatch(debug)))
  } else {
    NonEmpty.zipExn(bindingArray, tys)->NonEmpty.map((. (pat, ty)) =>
      Pattern.make(Narrow_left, Construct, pat, ty, ~f=updateContext(ctx))
    )
  }
}

@raises(Exit)
let unifyEchoes = (nullables, default, ctx) => {
  @raises(Exit)
  let rec aux = i => {
    switch nullables[i] {
    | None =>
      switch default {
      | Parser.EBinding(debug, binding, _) => Context.update(ctx, binding, Ty.echo(), debug)
      | EChild(debug, child) => Context.updateChild(ctx, child, Ty.Child.child(), debug)
      | EString(_, _, _) | EInt(_, _, _) | EFloat(_, _, _) => ()
      }
    | Some(Parser.EBinding(debug, binding, _)) =>
      Context.update(ctx, binding, Ty.nullable(Ty.echo()), debug)
      aux(succ(i))
    | Some(EString(debug, _, _) | EInt(debug, _, _) | EFloat(debug, _, _)) =>
      raise(Exit(Debug.nonNullableEchoLiteral(debug)))
    | Some(EChild(debug, child)) =>
      Context.updateChild(ctx, child, Ty.Child.nullable(), debug)
      aux(succ(i))
    }
  }
  aux(0)
}

let getTypes = x =>
  switch x {
  | Source.Acutis(_, {prop_types, child_types, _}) => (prop_types, child_types)
  | Function(_, props, children, _) => (props, children)
  }

@raises(Exit)
let rec makeCases = (cases, ctx, g) => {
  let tys = NonEmpty.hd(cases).Parser.patterns->NonEmpty.hd->NonEmpty.map((. _) => Ty.unknown())
  let cases = NonEmpty.map(cases, (. {Parser.patterns: patterns, nodes}) => {
    let bindings = Queue.make()
    let f = (. k, v, debug) => Queue.add(bindings, (k, v, debug))
    let pats = NonEmpty.map(patterns, (. ps) =>
      NonEmpty.zipByExn(ps, tys, (. p, ty) => Pattern.make(Expand_both, Destruct, p, ty, ~f))
    )
    let ctx = Context.addScope(ctx, bindings)
    (pats, nodes, ctx)
  })->NonEmpty.map((. (pats, nodes, ctx)) => {
    pats: pats,
    nodes: makeNodes(nodes, ctx, g),
  })
  (tys, cases)
}

@raises(Exit)
and unifyMap = (~ty, ~key, cases, pattern, ctx, g, debug) => {
  // Add a default wildcard for patterns without indices
  let cases = NonEmpty.map(cases, (. case) => {
    ...case,
    Parser.patterns: NonEmpty.map(case.Parser.patterns, (. pattern) =>
      switch NonEmpty.toArray(pattern) {
      | [hd] => NonEmpty.two(hd, UBinding(debug, "_"))
      | _ => pattern
      }
    ),
  })
  let (tys, cases) = makeCases(cases, ctx, g)
  let hd_ty = switch NonEmpty.toArray(tys) {
  | [hd, tl] =>
    unify(key(.), tl, Narrow_left, debug)
    ty(. hd)
  | _ => raise(Exit(Debug.mapPatternSizeMismatch(debug)))
  }
  let pattern = Pattern.make(Narrow_left, Construct, pattern, hd_ty, ~f=updateContext(ctx))
  (pattern, cases)
}

@raises(Exit)
and makeNodes = (nodes, ctx, g) =>
  Array.mapU(nodes, (. node) =>
    switch node {
    | Parser.UText(s, t) => TText(s, t)
    | UEcho(debug, nullables, default) =>
      unifyEchoes(nullables, default, ctx)->ignore
      TEcho(debug, nullables, default)
    | UComponent(debug, cname, props, children) =>
      let (propTypes, propTypesChildren) = getTypes(Utils.Dagmap.get(g, cname, debug))
      let propTypes = Ty.copy_record(propTypes) // The original should not mutate.
      let props = Pattern.make_record(Narrow_left, props, ~f=updateContext(ctx), propTypes, debug)
      let children = MapString.mergeU(children, propTypesChildren, (. k, c, ty) =>
        switch (c, ty) {
        | (None, None) | (None, Some({contents: NullableChild})) => None
        | (None, Some({contents: Child})) => raise(Exit(Debug.missingChild(debug, ~comp=cname, k)))
        | (Some(_), None) => raise(Exit(Debug.extraChild(debug, ~comp=cname, k)))
        | (Some(UChildName(debug, c)), Some({contents: ty})) =>
          Context.updateChild(ctx, c, ref(ty), debug)
          Some(TChildName(c))
        | (Some(UChildBlock(_, nodes)), Some(_)) => Some(TChildBlock(makeNodes(nodes, ctx, g)))
        }
      )
      TComponent(debug, cname, props, children)
    | UMatch(debug, bindingArray, cases) =>
      let (tys, cases) = makeCases(cases, ctx, g)
      let patterns = unifyMatchCases(bindingArray, tys, ctx)
      TMatch(debug, patterns, cases)
    | UMapList(debug, pattern, cases) =>
      let ty = (. t) => Ty.list(t)
      let key = (. ()) => Ty.int()
      let (pattern, cases) = unifyMap(~ty, ~key, cases, pattern, ctx, g, debug)
      TMapList(debug, pattern, cases)
    | UMapDict(debug, pattern, cases) =>
      let ty = (. t) => Ty.dict(t)
      let key = (. ()) => Ty.string()
      let (pattern, cases) = unifyMap(~ty, ~key, cases, pattern, ctx, g, debug)
      TMapDict(debug, pattern, cases)
    }
  )

@raises(Exit)
and make = (ast, g, root) => {
  let ctx = Context.make(root)
  let nodes = makeNodes(ast, ctx, g)
  {
    nodes: nodes,
    prop_types: ctx.global.contents,
    child_types: ctx.children.contents,
  }
}

let makeSrc = (. g, x) =>
  switch x {
  | Source.Acutis(name, ast) => Source.src(~name, make(ast, g, #Component))
  | Function(name, p, c, f) => Source.fnU(~name, p, c, f)
  }

let makeComponents = a => a->Utils.Dagmap.make(~f=makeSrc)->Utils.Dagmap.linkAll

let make = (ast, components) => make(ast, Utils.Dagmap.prelinked(components), #Root)
