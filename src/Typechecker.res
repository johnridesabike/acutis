/**
  Copyright (c) 2021 John Jackson.

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
module Array = Belt.Array
module MapInt = Belt.Map.Int
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

@raises(Exit)
let unify_enum_cases = (a, b, aty, bty, _, debug) =>
  switch (a, b) {
  | (Ty.Variant.String(a), Ty.Variant.String(b)) => Ty.Variant.String(SetString.union(a, b))
  | (Int(a), Int(b)) => Int(SetInt.union(a, b))
  | _ => raise(Exit(Debug.typeMismatch(debug, aty, bty, Ty.toString)))
  }

@raises(Exit)
let subset_enum_cases = (a, b, aty, bty, _, debug) => {
  let success = switch (a, b) {
  | (Ty.Variant.String(a), Ty.Variant.String(b)) => SetString.subset(b, a)
  | (Int(a), Int(b)) => SetInt.subset(b, a)
  | _ => false
  }
  if !success {
    raise(Exit(Debug.typeMismatch(debug, aty, bty, Ty.toString)))
  }
}

@raises(Exit)
let unify_variant = (a, b, aty, bty, mode, debug, ~unify_cases, ~subset_cases) =>
  switch mode {
  | Destructure_expand =>
    switch (a.Ty.Variant.row, b.Ty.Variant.row) {
    | (Closed, Closed) => a.row = Closed
    | (Open, _) | (_, Open) => a.row = Open
    }
    a.cases = unify_cases(a.cases, b.cases, aty, bty, mode, debug)
  | Construct_literal =>
    switch (a.row, b.row) {
    | (Open, Closed | Open) => a.cases = unify_cases(a.cases, b.cases, aty, bty, mode, debug)
    | (Closed, Closed | Open) => subset_cases(a.cases, b.cases, aty, bty, mode, debug)
    }
  | Construct_var =>
    switch (a.row, b.row) {
    | (Closed, Closed) => subset_cases(a.cases, b.cases, aty, bty, mode, debug)
    | (Open, Closed) => a.cases = unify_cases(a.cases, b.cases, aty, bty, mode, debug)
    | (Open, Open) =>
      let cases = unify_cases(a.cases, b.cases, aty, bty, mode, debug)
      a.cases = cases
      b.cases = cases
    | (Closed, Open) => raise(Exit(Debug.typeMismatch(debug, aty, bty, Ty.toString)))
    }
  }

@raises(Exit)
let rec unify = (aty, bty, mode, debug) => {
  switch (aty.contents, bty.contents) {
  | (Ty.Int, Ty.Int) | (Float, Float) | (String, String) => ()
  | (Unknown, t) => aty := t
  | (t, Unknown) => bty := t
  | (Echo, (Int | Float | String | Echo | Enum(_)) as t) => aty := t
  | ((Int | Float | String | Enum(_)) as t, Echo) => bty := t
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
  | (Record(a), Record(b)) => unify_record(a, b, aty, bty, mode, debug)
  | (Enum(a), Enum(b)) =>
    unify_variant(
      ~unify_cases=unify_enum_cases,
      ~subset_cases=subset_enum_cases,
      a,
      b,
      aty,
      bty,
      mode,
      debug,
    )
  | (Union(ka, a), Union(kb, b)) if ka == kb =>
    unify_variant(
      ~unify_cases=unify_union_cases,
      ~subset_cases=subset_union_cases,
      a,
      b,
      aty,
      bty,
      mode,
      debug,
    )
  | _ => raise(Exit(Debug.typeMismatch(debug, aty, bty, Ty.toString)))
  }
}

@raises(Exit)
and unify_union_cases = (a, b, aty, bty, mode, debug) => {
  let f = (. _, a, b) =>
    switch (a, b) {
    | (Some(_) as x, None) | (None, Some(_) as x) => x
    | (Some(a) as x, Some(b)) =>
      unify_record(a, b, aty, bty, mode, debug)
      x
    | (None, None) => None
    }
  switch (a, b) {
  | (Ty.Variant.String(a), Ty.Variant.String(b)) => Ty.Variant.String(MapString.mergeU(a, b, f))
  | (Int(a), Int(b)) => Int(MapInt.mergeU(a, b, f))
  | _ => raise(Exit(Debug.typeMismatch(debug, aty, bty, Ty.toString)))
  }
}

@raises(Exit)
and subset_union_cases = (a, b, aty, bty, mode, debug) => {
  let f = (. _, a, b) =>
    switch (a, b) {
    | (Some(_) as x, None) => x
    | (Some(a) as x, Some(b)) =>
      unify_record(a, b, aty, bty, mode, debug)
      x
    | (None, Some(_)) => raise(Exit(Debug.typeMismatch(debug, aty, bty, Ty.toString)))
    | (None, None) => None
    }
  switch (a, b) {
  | (Ty.Variant.String(a), Ty.Variant.String(b)) => MapString.mergeU(a, b, f)->ignore
  | (Int(a), Int(b)) => MapInt.mergeU(a, b, f)->ignore
  | _ => raise(Exit(Debug.typeMismatch(debug, aty, bty, Ty.toString)))
  }
}

@raises(Exit)
and unify_record = (a, b, aty, bty, mode, debug) =>
  switch mode {
  | Destructure_expand =>
    a :=
      MapString.mergeU(a.contents, b.contents, (. _, v1, v2) => {
        switch (v1, v2) {
        | (Some(v1) as x, Some(v2)) =>
          unify(v1, v2, mode, debug)
          x
        | (Some(_) as x, None) | (None, Some(_) as x) => x
        | (None, None) => None
        }
      })
  | Construct_var =>
    b :=
      MapString.mergeU(a.contents, b.contents, (. _, v1, v2) => {
        switch (v1, v2) {
        | (Some(v1), Some(v2) as x) =>
          unify(v1, v2, mode, debug)
          x
        | (Some(_) as x, None) | (None, Some(_) as x) => x
        | (None, None) => None
        }
      })
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
      raise(Exit(Debug.cantNarrowType(debug, aty, bty, Ty.toString)))
    } else {
      a := r
    }
  }

let open_rows_bool_union_aux = (. ty) =>
  switch ty {
  | None => Some(ref(MapString.empty))
  | Some(_) as m => m
  }

let open_rows = ty =>
  switch ty.contents {
  | Ty.Enum(ty) =>
    switch ty.extra {
    | Extra_none => ty.row = Open
    | Extra_boolean => ty.cases = Ty.Enum.false_and_true_cases
    }
  | Union(_, ty) =>
    switch (ty.extra, ty.cases) {
    | (Extra_boolean, Int(cases)) =>
      ty.cases = Int(
        cases
        ->MapInt.updateU(0, open_rows_bool_union_aux)
        ->MapInt.updateU(1, open_rows_bool_union_aux),
      )
    | _ => ty.row = Open
    }
  | _ => ()
  }

module Pattern = {
  type constant =
    | TString(string)
    | TInt(int)
    | TFloat(float)

  type construct = TList | TNullable

  type rec t =
    | TConst(Debug.t, constant, option<Ty.Enum.t>)
    | TConstruct(Debug.t, construct, option<t>)
    | TTuple(Debug.t, array<t>)
    | TRecord(
        Debug.t,
        option<(string, constant, Typescheme.Union.t<Typescheme.t>)>,
        MapString.t<t>,
        ref<MapString.t<Typescheme.t>>,
      )
    | TDict(Debug.t, MapString.t<t>, ref<SetString.t>)
    | TVar(Debug.t, string) // any binding
    | TOptionalVar(Debug.t, string) // any binding, may not be set
    | TAny(Debug.t) // ignored wildcard _

  @raises(Exit)
  let make_enum_aux = (tag, extra, row, tyvars, debug) =>
    switch tag {
    | TInt(i) => {
        Ty.Variant.cases: Int(MapInt.set(MapInt.empty, i, tyvars)),
        row: row,
        extra: extra,
      }
    | TString(s) => {
        Ty.Variant.cases: String(MapString.set(MapString.empty, s, tyvars)),
        row: row,
        extra: extra,
      }
    | _ => raise(Exit(Debug.badUnionTag(debug))) // error goes here
    }

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
      let new_enum = switch mode {
      | Destructure_expand =>
        switch b {
        | 0 => Ty.Enum.false_only()
        | _ => Ty.Enum.true_only()
        }
      | Construct_var | Construct_literal => Ty.Enum.false_and_true()
      }
      let new_ty = ref(Ty.Enum(new_enum))
      let enum = switch ty.contents {
      | Enum(enum) => enum
      | Unknown => new_enum
      | _ => raise(Exit(Debug.typeMismatch(dbg, ty, new_ty, Ty.toString)))
      }
      unify(ty, new_ty, mode, dbg)
      TConst(dbg, TInt(b), Some(enum))
    | UStringEnum(dbg, s) =>
      let new_enum = switch mode {
      | Destructure_expand => Ty.Enum.string_singleton(s, Closed)
      | Construct_var | Construct_literal => Ty.Enum.string_singleton(s, Open)
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
      | Destructure_expand => Ty.Enum.int_singleton(i, Closed)
      | Construct_var | Construct_literal => Ty.Enum.int_singleton(i, Open)
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
    | URecord(dbg, None, m) =>
      let new_tyvars = MapString.mapU(m, (. _) => Ty.unknown())->ref
      let tyvars = switch ty.contents {
      | Record(tys) => tys
      | Unknown => new_tyvars
      | _ => raise(Exit(Debug.typeMismatch(dbg, ty, Ty.internal_record(new_tyvars), Ty.toString)))
      }
      unify(ty, Ty.internal_record(new_tyvars), mode, dbg)
      let r = switch mode {
      | Destructure_expand => make_record_destructure(mode, m, tyvars.contents, ~f, dbg)
      | Construct_var | Construct_literal => make_record(m, tyvars.contents, ~f, dbg, mode)
      }
      TRecord(dbg, None, r, tyvars)
    | URecord(dbg, Some((k, v)), m) =>
      let new_tyvars = MapString.mapU(m, (. _) => Ty.unknown())->ref
      let new_tag_ty = Ty.unknown()
      let row = switch mode {
      | Destructure_expand => Ty.Variant.Closed
      | Construct_var | Construct_literal => Open
      }
      let (tag, tag_extra) = switch make(v, new_tag_ty, ~f, mode) {
      | TConst(_, tag, None) => (tag, Ty.Variant.Extra_none)
      | TConst(_, tag, Some({extra, _})) => (tag, extra) // for booleans
      | _ => raise(Exit(Debug.badUnionTag(dbg)))
      }
      let tyvars = switch ty.contents {
      | Union(_, enum) =>
        let tyvars = switch (tag, enum) {
        | (TInt(i), {cases: Int(cases), _}) => MapInt.get(cases, i)
        | (TString(s), {cases: String(cases), _}) => MapString.get(cases, s)
        | _ => None // Let the unification function handle the type error.
        }
        switch tyvars {
        | Some(vars) => vars
        | None => new_tyvars
        }
      | Unknown => new_tyvars
      | _ =>
        let ty' = ref(Ty.Union(k, make_enum_aux(tag, tag_extra, row, new_tyvars, dbg)))
        raise(Exit(Debug.typeMismatch(dbg, ty, ty', Ty.toString)))
      }
      let new_enum = make_enum_aux(tag, tag_extra, row, new_tyvars, dbg)
      unify(ty, ref(Ty.Union(k, new_enum)), mode, dbg)
      let r = switch mode {
      | Destructure_expand => make_record_destructure(mode, m, tyvars.contents, ~f, dbg)
      | Construct_var | Construct_literal => make_record(m, tyvars.contents, ~f, dbg, mode)
      }
      TRecord(dbg, Some((k, tag, new_enum)), r, tyvars)
    | UDict(dbg, m) =>
      let new_kys = ref(SetString.empty)
      let (tyvar, kys) = switch ty.contents {
      | Dict(ty, kys) => (ty, kys)
      | Unknown => (Ty.unknown(), new_kys)
      | _ =>
        raise(Exit(Debug.typeMismatch(dbg, ty, Ty.internal_dict_keys(Ty.unknown(), new_kys), Ty.toString)))
      }
      unify(ty, Ty.internal_dict_keys(tyvar, new_kys), mode, dbg)
      let d = MapString.mapU(m, (. pat) => make(pat, tyvar, mode, ~f))
      TDict(dbg, d, kys)
    | UBinding(dbg, "_") =>
      switch mode {
      | Destructure_expand =>
        open_rows(ty)
        TAny(dbg)
      | Construct_literal | Construct_var => raise(Exit(Debug.underscoreInConstruct(dbg)))
      }
    | UBinding(dbg, b) =>
      switch mode {
      | Destructure_expand => open_rows(ty)
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

  let toStringConst = (x, e) =>
    switch (x, e) {
    | (TInt(i), Some({Ty.Variant.extra: Extra_boolean, _})) =>
      switch i {
      | 0 => "false"
      | _ => "true"
      }
    | (TInt(i), Some(_)) => "@" ++ Belt.Int.toString(i)
    | (TInt(i), None) => Belt.Int.toString(i)
    | (TString(s), Some(_)) => `@"${s}"`
    | (TString(s), None) => `"${s}"`
    | (TFloat(f), _) => Belt.Float.toString(f)
    }

  let keyValuesToString = (k, v) =>
    if v == k {
      v
    } else {
      k ++ ": " ++ v
    }

  let rec toString = x =>
    switch x {
    | TConst(_, x, e) => toStringConst(x, e)
    | TTuple(_, t) => "(" ++ Array.joinWith(t, ", ", toString) ++ ")"
    | TRecord(_, uniontag, r, _) =>
      let s =
        MapString.toArray(r)->Array.joinWith(", ", ((k, v)) => keyValuesToString(k, toString(v)))
      switch uniontag {
      | Some(tag) =>
        let tag = switch tag {
        | (k, TInt(0), {extra: Extra_boolean, _}) => `@"${k}": false`
        | (k, _, {extra: Extra_boolean, _}) => `@"${k}": true`
        | (k, v, _) => `@"${k}": ${toStringConst(v, None)}`
        }
        let sep = switch s {
        | "" => ""
        | _ => ", "
        }
        "{" ++ tag ++ sep ++ s ++ "}"
      | None => "{" ++ s ++ "}"
      }
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
    | TRecord(d, _, _, _)
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
      | EString(_, _, _) => ()
      }
    | Some(Parser.EBinding(debug, binding, _)) =>
      Context.update(ctx)(. binding, Ty.nullable(Ty.echo()), debug)
      aux(succ(i))
    | Some(EString(debug, _, _)) => raise(Exit(Debug.nonNullableEchoLiteral(debug)))
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
