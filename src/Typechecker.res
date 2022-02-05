/**
  Copyright (c) 2021 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Array = Belt.Array
module MapString = Belt.Map.String
module Queue = Belt.MutableQueue
module SetInt = Belt.Set.Int
module SetString = Belt.Set.String
module Ty = Typescheme
module UPat = Parser.Pattern

exception Exit = Debug.Exit

/*
  When we unify a type during destructuring, we expand any structural types
  (records, enums).

  When we unify a type during constructing literal values, record types are
  narrowed (we use a subset of both) and enum behavior depends on its row
  property.

  When we unify a type during constructing with a variable, then we expand
  records and open enums in the variable's type.
*/

type mode =
  | Destructure_expand // Record a and b both take on each other's fields.
  | Construct_literal // Record a is narrowed to a subset of a and b.
  | Construct_var // Record a expands into b only.

let check_enum_subset = (subset, a, b, tref1, tref2, debug) =>
  if !subset(b, a) {
    raise(Exit(Debug.typeMismatch(debug, tref1, tref2, Ty.toString)))
  }

let union_enum = (a, b, tref1, tref2, debug) =>
  switch (a, b) {
  | (Ty.Enum.Enum_String(a), Ty.Enum.Enum_String(b)) => Ty.Enum.Enum_String(SetString.union(a, b))
  | (Enum_Int(a), Enum_Int(b)) => Enum_Int(SetInt.union(a, b))
  | _ => raise(Exit(Debug.typeMismatch(debug, tref1, tref2, Ty.toString)))
  }

@raises(Exit)
let rec unify = (tref1, tref2, mode, debug) =>
  switch (tref1.contents, tref2.contents) {
  | (Ty.Int, Ty.Int) | (Float, Float) | (String, String) => ()
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
    let s1 = Array.size(t1)
    let s2 = Array.size(t2)
    if s1 == s2 {
      Array.zipByU(t1, t2, (. a, b) => unify(a, b, mode, debug))->ignore
    } else {
      raise(Exit(Debug.tupleSizeMismatch(debug, s1, s2)))
    }
  | (Record(a), Record(b)) =>
    switch mode {
    | Destructure_expand =>
      let r = MapString.mergeU(a.contents, b.contents, (. _, v1, v2) => {
        switch (v1, v2) {
        | (Some(v1) as x, Some(v2)) =>
          unify(v1, v2, mode, debug)
          x
        | (Some(_) as x, None) | (None, Some(_) as x) => x
        | (None, None) => None
        }
      })
      a := r
    | Construct_var =>
      let r = MapString.mergeU(a.contents, b.contents, (. _, v1, v2) => {
        switch (v1, v2) {
        | (Some(v1), Some(v2) as x) =>
          unify(v1, v2, mode, debug)
          x
        | (Some(_) as x, None) | (None, Some(_) as x) => x
        | (None, None) => None
        }
      })
      b := r
    | Construct_literal =>
      let r = MapString.mergeU(a.contents, b.contents, (. _, v1, v2) =>
        switch (v1, v2) {
        | (Some(v1) as x, Some(v2)) =>
          unify(v1, v2, mode, debug)
          x
        | (Some(_), None) | (None, Some(_)) | (None, None) => None
        }
      )
      if MapString.isEmpty(r) {
        raise(Exit(Debug.cantNarrowType(debug, tref1, tref2, Ty.toString)))
      } else {
        a := r
      }
    }
  | (Enum(a), Enum(b)) =>
    switch mode {
    | Destructure_expand =>
      let cases = union_enum(a.cases, b.cases, tref1, tref2, debug)
      let row = switch (a.row, b.row) {
      | (Closed, Closed) => Ty.Enum.Closed
      | (Open, _) | (_, Open) => Open
      }
      a.cases = cases
      a.row = row
    | Construct_literal =>
      switch (a.row, b.row) {
      | (Open, Closed | Open) =>
        let cases = union_enum(a.cases, b.cases, tref1, tref2, debug)
        a.cases = cases
      | (Closed, Closed | Open) =>
        switch (a.cases, b.cases) {
        | (Enum_String(a), Enum_String(b)) =>
          check_enum_subset(SetString.subset, a, b, tref1, tref2, debug)
        | (Enum_Int(a), Enum_Int(b)) => check_enum_subset(SetInt.subset, a, b, tref1, tref2, debug)
        | _ => raise(Exit(Debug.typeMismatch(debug, tref1, tref2, Ty.toString)))
        }
      }
    | Construct_var =>
      switch (a.row, b.row) {
      | (Closed, Closed) =>
        switch (a.cases, b.cases) {
        | (Enum_String(a), Enum_String(b)) =>
          check_enum_subset(SetString.subset, a, b, tref1, tref2, debug)
        | (Enum_Int(a), Enum_Int(b)) => check_enum_subset(SetInt.subset, a, b, tref1, tref2, debug)
        | _ => raise(Exit(Debug.typeMismatch(debug, tref1, tref2, Ty.toString)))
        }
      | (Open, Closed) =>
        let cases = union_enum(a.cases, b.cases, tref1, tref2, debug)
        a.cases = cases
      | (Open, Open) =>
        let cases = union_enum(a.cases, b.cases, tref1, tref2, debug)
        a.cases = cases
        b.cases = cases
      | (Closed, Open) => raise(Exit(Debug.typeMismatch(debug, tref1, tref2, Ty.toString)))
      }
    }
  | (Boolean(a), Boolean(b)) =>
    switch mode {
    | Destructure_expand => a := Ty.Boolean.union(a.contents, b.contents)
    | Construct_literal | Construct_var =>
      check_enum_subset(Ty.Boolean.subset, a.contents, b.contents, tref1, tref2, debug)
    }
  | _ => raise(Exit(Debug.typeMismatch(debug, tref1, tref2, Ty.toString)))
  }

let rec open_all_rows = ty =>
  switch ty.contents {
  | Ty.Enum(ty) => ty.row = Open
  | Boolean(ty) => ty := Ty.Boolean.False_or_true
  | Nullable(ty) | List(ty) | Dict(ty, _) => open_all_rows(ty)
  | Tuple(a) => Array.forEach(a, open_all_rows)
  | Record(d) => MapString.forEachU(d.contents, (. _, ty) => open_all_rows(ty))
  | Unknown | Int | Float | String | Echo => ()
  }

module Pattern = {
  type constant =
    | TBool(bool)
    | TString(string)
    | TInt(int)
    | TFloat(float)

  let toStringConst = (~enum, x) =>
    switch x {
    | TBool(true) => "true"
    | TBool(false) => "false"
    | TString(s) =>
      if enum {
        `@"${s}"`
      } else {
        `"${s}"`
      }
    | TInt(i) =>
      if enum {
        "@" ++ Belt.Int.toString(i)
      } else {
        Belt.Int.toString(i)
      }
    | TFloat(f) => Belt.Float.toString(f)
    }

  type construct = TList | TNullable

  type rec t =
    | TConst(Debug.t, constant, option<Ty.Enum.t>)
    | TConstruct(Debug.t, construct, option<t>)
    | TTuple(Debug.t, array<t>)
    | TRecord(Debug.t, Belt.Map.String.t<t>, ref<Belt.Map.String.t<Typescheme.t>>)
    | TDict(Debug.t, Belt.Map.String.t<t>, ref<Belt.Set.String.t>)
    | TVar(Debug.t, string) // any binding
    | TOptionalVar(Debug.t, string) // any binding, may not be set
    | TAny(Debug.t) // ignored wildcard _

  @raises(Exit)
  let rec make = (pat, ty, ~f, mode) =>
    switch pat {
    | UPat.UInt(dbg, i) =>
      unify(ty, Ty.int(), mode, dbg)
      TConst(dbg, TInt(i), None)
    | UString(dbg, s) =>
      unify(ty, Ty.string(), mode, dbg)
      TConst(dbg, TString(s), None)
    | UFloat(dbg, x) =>
      unify(ty, Ty.float(), mode, dbg)
      TConst(dbg, TFloat(x), None)
    | UBool(dbg, b) =>
      let new_ty = switch mode {
      | Destructure_expand =>
        switch b {
        | true => Ty.true_()
        | false => Ty.false_()
        }
      | Construct_var | Construct_literal => Ty.boolean()
      }
      unify(ty, new_ty, mode, dbg)
      TConst(dbg, TBool(b), None)
    | UStringEnum(dbg, s) =>
      let new_enum = switch mode {
      | Destructure_expand => Ty.Enum.make(#String(s), Closed)
      | Construct_var | Construct_literal => Ty.Enum.make(#String(s), Open)
      }
      let new_ty = ref(Ty.Enum(new_enum))
      let enum = switch ty.contents {
      | Enum(enum) => enum
      | Unknown => new_enum
      | _ => raise(Exit(Debug.typeMismatch(dbg, ty, new_ty, Ty.toString)))
      }
      unify(ty, new_ty, mode, dbg)
      TConst(dbg, TString(s), Some(enum))
    | UIntEnum(dbg, i) =>
      let new_enum = switch mode {
      | Destructure_expand => Ty.Enum.make(#Int(i), Closed)
      | Construct_var | Construct_literal => Ty.Enum.make(#Int(i), Open)
      }
      let new_ty = ref(Ty.Enum(new_enum))
      let enum = switch ty.contents {
      | Enum(enum) => enum
      | Unknown => new_enum
      | _ => raise(Exit(Debug.typeMismatch(dbg, ty, new_ty, Ty.toString)))
      }
      unify(ty, new_ty, mode, dbg)
      TConst(dbg, TInt(i), Some(enum))
    | UNullable(dbg, pat) =>
      let tyvar = switch ty.contents {
      | Nullable(ty) => ty
      | Unknown => Ty.unknown()
      | _ => raise(Exit(Debug.typeMismatch(dbg, ty, Ty.nullable(Ty.unknown()), Ty.toString)))
      }
      let pat = switch pat {
      | None => None
      | Some(pat) => Some(TTuple(dbg, [make(~f, pat, tyvar, mode)]))
      }
      unify(ty, Ty.nullable(tyvar), mode, dbg)
      TConstruct(dbg, TNullable, pat)
    | UList(dbg, a, tail) =>
      let tyvar = switch ty.contents {
      | List(ty) => ty
      | Unknown => Ty.unknown()
      | _ => raise(Exit(Debug.typeMismatch(dbg, ty, Ty.list(Ty.unknown()), Ty.toString)))
      }
      unify(ty, Ty.list(tyvar), mode, dbg)
      let tl = switch tail {
      | None => TConstruct(dbg, TList, None)
      | Some(tail) => make(tail, ty, mode, ~f)
      }
      make_list(a, 0, tyvar, ~tl, ~f, mode)
    | UTuple(dbg, a) =>
      let new_tyvars = Array.mapU(a, (. _) => Ty.unknown())
      let tyvars = switch ty.contents {
      | Tuple(tys) => tys
      | Unknown => new_tyvars
      | _ => raise(Exit(Debug.typeMismatch(dbg, ty, Ty.tuple(new_tyvars), Ty.toString)))
      }
      unify(ty, Ty.tuple(tyvars), mode, dbg)
      TTuple(dbg, Array.zipByU(a, tyvars, (. pat, ty) => make(pat, ty, mode, ~f)))
    | URecord(dbg, m) =>
      let new_tyvars = MapString.mapU(m, (. _) => Ty.unknown())->ref
      let tyvars = switch ty.contents {
      | Record(tys) => tys
      | Unknown => new_tyvars
      | _ => raise(Exit(Debug.typeMismatch(dbg, ty, Ty.record2(new_tyvars), Ty.toString)))
      }
      unify(ty, Ty.record2(new_tyvars), mode, dbg)
      let r = switch mode {
      | Destructure_expand => make_record_destructure(mode, m, tyvars.contents, ~f, dbg)
      | Construct_var | Construct_literal => make_record(m, tyvars.contents, ~f, dbg, mode)
      }
      TRecord(dbg, r, tyvars)
    | UDict(dbg, m) =>
      let new_kys = ref(SetString.empty)
      let (tyvar, kys) = switch ty.contents {
      | Dict(ty, kys) => (ty, kys)
      | Unknown => (Ty.unknown(), new_kys)
      | _ =>
        raise(Exit(Debug.typeMismatch(dbg, ty, Ty.dict_keys(Ty.unknown(), new_kys), Ty.toString)))
      }
      unify(ty, Ty.dict_keys(tyvar, new_kys), mode, dbg)
      let d = MapString.mapU(m, (. pat) => make(pat, tyvar, mode, ~f))
      TDict(dbg, d, kys)
    | UBinding(dbg, "_") =>
      switch mode {
      | Destructure_expand =>
        open_all_rows(ty)
        TAny(dbg)
      | Construct_literal | Construct_var => raise(Exit(Debug.underscoreInConstruct(dbg)))
      }
    | UBinding(dbg, b) =>
      switch mode {
      | Destructure_expand => open_all_rows(ty)
      | Construct_literal | Construct_var => ()
      }
      f(. b, ty, dbg)
      TVar(dbg, b)
    }

  @raises(Exit)
  and make_list = (~tl, ~f, a, i, ty, mode) =>
    switch a[i] {
    | None => tl
    | Some(p) =>
      let dbg = UPat.debug(p)
      let hd = make(~f, p, ty, mode)
      let tl = make_list(~tl, ~f, a, succ(i), ty, mode)
      TConstruct(dbg, TList, Some(TTuple(dbg, [hd, tl])))
    }

  @raises(Exit)
  and make_record = (m, tyvars, ~f, dbg, mode) => {
    MapString.mergeU(m, tyvars, (. k, pat, ty) =>
      switch (pat, ty) {
      | (Some(_), None) | (None, None) => None
      | (Some(pat), Some(ty)) => Some(make(pat, ty, ~f, mode))
      | (None, Some(ty)) => raise(Exit(Debug.missingRecordField(dbg, k, ty, Ty.toString)))
      }
    )
  }

  @raises(Exit)
  and make_record_destructure = (mode, m, tyvars, ~f, dbg) => {
    MapString.mergeU(m, tyvars, (. _, pat, ty) =>
      switch (pat, ty) {
      | (Some(pat), None) => Some(make(pat, Ty.unknown(), mode, ~f))
      | (Some(pat), Some(ty)) => Some(make(pat, ty, mode, ~f))
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
    | TConst(_, x, None) => toStringConst(x, ~enum=false)
    | TConst(_, x, Some(_)) => toStringConst(x, ~enum=true)
    | TTuple(_, t) => "(" ++ Array.joinWith(t, ", ", toString) ++ ")"
    | TRecord(_, r, _) =>
      let a = MapString.toArray(r)
      "{" ++ Array.joinWith(a, ", ", ((k, v)) => keyValuesToString(k, toString(v))) ++ "}"
    | TDict(_, r, _) =>
      let a = MapString.toArray(r)
      "<" ++ Array.joinWith(a, ", ", ((k, v)) => keyValuesToString(k, toString(v))) ++ ">"
    | TVar(_, v) | TOptionalVar(_, v) => v
    | TConstruct(_, TNullable, None) => "null"
    | TConstruct(_, TNullable, Some(x)) => "!" ++ toString(x)
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
    | TConst(d, _, _)
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
  let update = ({scope, global, _}, . k, v, debug) =>
    switch MapString.get(scope, k) {
    | None =>
      switch MapString.get(global.contents, k) {
      | None => global := MapString.set(global.contents, k, v)
      | Some(v') => unify(v, v', Construct_var, debug)
      }
    | Some(v') => unify(v', v, Construct_var, debug)
    }

  @raises(Exit)
  let updateChild = ({root, children, _}, (k, v), debug) =>
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
  let addScope = (ctx, bindings_all) => {
    let bindings_all = Queue.mapU(bindings_all, (. q) =>
      Queue.reduceU(q, MapString.empty, (. newscope, (k, v, dbg)) =>
        if MapString.has(newscope, k) {
          raise(Exit(Debug.nameBoundMultipleTimes(dbg, k)))
        } else {
          MapString.set(newscope, k, (v, dbg))
        }
      )
    )
    switch Queue.pop(bindings_all) {
    | None => ctx
    | Some(hd) =>
      let newscope = Queue.reduceU(bindings_all, hd, (. acc, m) =>
        MapString.mergeU(acc, m, (. k, a, b) =>
          switch (a, b) {
          | (Some((a, _)) as a', Some((b, dbg))) =>
            unify(a, b, Construct_literal, dbg)
            a'
          | (Some((_, dbg)), None) | (None, Some((_, dbg))) =>
            raise(Exit(Debug.variableMissingInPattern(dbg, k)))
          | (None, None) => None
          }
        )
      )
      // Merge the new bindings with the outer scope & shadow duplicate names.
      let scope = MapString.mergeU(ctx.scope, newscope, (. _, a, b) =>
        switch (a, b) {
        | (None, None) => None
        | (Some(x), None) | (_, Some((x, _))) => Some(x)
        }
      )
      {...ctx, scope: scope}
    }
  }
}

@raises(Exit)
let unifyMatchCases = (bindingArray, tys, ctx) => {
  if NonEmpty.size(bindingArray) != NonEmpty.size(tys) {
    let debug = NonEmpty.hd(bindingArray)->UPat.debug
    raise(Exit(Debug.patternNumberMismatch(debug)))
  } else {
    NonEmpty.zip(bindingArray, tys)->NonEmpty.map((. (pat, ty)) =>
      Pattern.make(pat, ty, Construct_literal, ~f=Context.update(ctx))
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
      | Parser.EBinding(debug, binding, _) => Context.update(ctx)(. binding, Ty.echo(), debug)
      | EChild(debug, child) => Context.updateChild(ctx, Ty.Child.child(child), debug)
      | EString(_, _, _) | EInt(_, _, _) | EFloat(_, _, _) => ()
      }
    | Some(Parser.EBinding(debug, binding, _)) =>
      Context.update(ctx)(. binding, Ty.nullable(Ty.echo()), debug)
      aux(succ(i))
    | Some(EString(debug, _, _) | EInt(debug, _, _) | EFloat(debug, _, _)) =>
      raise(Exit(Debug.nonNullableEchoLiteral(debug)))
    | Some(EChild(debug, child)) =>
      Context.updateChild(ctx, Ty.Child.nullable(child), debug)
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

let addDefaultWildcardToCases = cases =>
  NonEmpty.map(cases, (. case) => {
    ...case,
    Parser.patterns: NonEmpty.map(case.Parser.patterns, (. pattern) =>
      switch NonEmpty.toArray(pattern) {
      | [hd] => NonEmpty.two(hd, UBinding(UPat.debug(hd), "_"))
      | _ => pattern
      }
    ),
  })

@raises(Exit)
let unifyMap = (~ty, ~key, (tys, cases), pattern, ctx, debug) => {
  let hd_ty = switch NonEmpty.toArray(tys) {
  | [hd, tl] =>
    unify(key(.), tl, Construct_literal, debug)
    ty(. hd)
  | _ => raise(Exit(Debug.mapPatternSizeMismatch(debug)))
  }
  let pattern = Pattern.make(pattern, hd_ty, Construct_literal, ~f=Context.update(ctx))
  (pattern, cases)
}

@raises(Exit)
let rec makeCases = (cases, ctx, g) => {
  let tys = NonEmpty.hd(cases).Parser.patterns->NonEmpty.hd->NonEmpty.map((. _) => Ty.unknown())
  let size = NonEmpty.size(tys)
  let cases = NonEmpty.map(cases, (. {Parser.patterns: patterns, nodes}) => {
    let bindings_all = Queue.make()
    let pats = NonEmpty.map(patterns, (. ps) => {
      let bindings = Queue.make()
      Queue.add(bindings_all, bindings)
      let f = (. k, v, debug) => Queue.add(bindings, (k, v, debug))
      if NonEmpty.size(ps) != size {
        let debug = NonEmpty.hd(ps)->UPat.debug
        raise(Exit(Debug.patternNumberMismatch(debug)))
      } else {
        NonEmpty.zipBy(ps, tys, (. p, ty) => Pattern.make(p, ty, Destructure_expand, ~f))
      }
    })
    let ctx = Context.addScope(ctx, bindings_all)
    (pats, nodes, ctx)
  })->NonEmpty.map((. (pats, nodes, ctx)) => {
    pats: pats,
    nodes: makeNodes(nodes, ctx, g),
  })
  (tys, cases)
}

@raises(Exit)
and makeNodes = (nodes, ctx, g) =>
  Array.mapU(nodes, (. node) =>
    switch node {
    | Parser.UText(s, t) => TText(s, t)
    | UEcho(debug, nullables, default) =>
      unifyEchoes(nullables, default, ctx)->ignore
      TEcho(debug, nullables, default)
    | UComponent(debug, comp, props, children) =>
      let (propTypes, propTypesChildren) = getTypes(Utils.Dagmap.get(g, comp, debug))
      let propTypes = Ty.copy_record(propTypes) // The original should not mutate.
      let props = Pattern.make_record(
        props,
        propTypes,
        debug,
        ~f=Context.update(ctx),
        Construct_literal,
      )
      let children = MapString.mergeU(children, propTypesChildren, (. k, c, ty) =>
        switch (c, ty) {
        | (None, None) => None
        | (None, Some(ty)) =>
          if Ty.Child.is_nullable(ty) {
            None
          } else {
            raise(Exit(Debug.missingChild(debug, ~comp, k)))
          }
        | (Some(_), None) => raise(Exit(Debug.extraChild(debug, ~comp, k)))
        | (Some(UChildName(debug, c)), Some(ty)) =>
          Context.updateChild(ctx, (c, ty), debug)
          Some(TChildName(c))
        | (Some(UChildBlock(_, nodes)), Some(_)) => Some(TChildBlock(makeNodes(nodes, ctx, g)))
        }
      )
      TComponent(debug, comp, props, children)
    | UMatch(debug, bindingArray, cases) =>
      let (tys, cases) = makeCases(cases, ctx, g)
      let patterns = unifyMatchCases(bindingArray, tys, ctx)
      TMatch(debug, patterns, cases)
    | UMapList(debug, pattern, cases) =>
      let ty = (. t) => Ty.list(t)
      let key = (. ()) => Ty.int()
      let cases = addDefaultWildcardToCases(cases)->makeCases(ctx, g)
      let (pattern, cases) = unifyMap(~ty, ~key, cases, pattern, ctx, debug)
      TMapList(debug, pattern, cases)
    | UMapDict(debug, pattern, cases) =>
      let ty = (. t) => Ty.dict(t)
      let key = (. ()) => Ty.string()
      let cases = addDefaultWildcardToCases(cases)->makeCases(ctx, g)
      let (pattern, cases) = unifyMap(~ty, ~key, cases, pattern, ctx, debug)
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
