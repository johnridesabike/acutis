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

exception Exit = Debug.Exit
exception Exit2(option<string>, option<string>)

module Pattern = {
  type constant =
    | TBool(bool)
    | TString(string)
    | TInt(int)
    | TFloat(float)

  let compareConst = (a, b) =>
    switch (a, b) {
    | (TString(a), TString(b)) => compare(a, b)
    | (TFloat(a), TFloat(b)) => compare(a, b)
    | (TInt(a), TInt(b)) => compare(a, b)
    | (TBool(a), TBool(b)) => compare(a, b)
    | _ => assert false
    }

  let eqConst = (a, b) =>
    switch (a, b) {
    | (TString(a), TString(b)) => a == b
    | (TFloat(a), TFloat(b)) => a == b
    | (TInt(a), TInt(b)) => a == b
    | (TBool(a), TBool(b)) => a == b
    | _ => assert false
    }

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
    | TConst(Debug.loc, constant)
    | TConstruct(Debug.loc, construct, option<t>)
    | TTuple(Debug.loc, array<t>)
    | TRecord(Debug.loc, array<(string, t)>)
    | TDict(Debug.loc, array<(string, t)>)
    | TVar(Debug.loc, string) // any binding
    | TOptionalVar(Debug.loc, string) // any binding, may not be set
    | TAny(Debug.loc) // ignored wildcard _

  let rec makeList = (a, ty, ~tail) => {
    let r = ref(tail)
    for i in Array.size(a) - 1 downto 0 {
      let p = Array.getUnsafe(a, i)
      let loc = UPat.toLocation(p)
      r := TConstruct(loc, TList, Some(TTuple(loc, [make(p, ty), r.contents])))
    }
    r.contents
  }

  and make = (p: UPat.t, ty) =>
    switch (p, ty) {
    | (UNull(l), Typescheme.Nullable(_)) => TConstruct(l, TNullable, None)
    | (USome(l, p), Nullable({contents})) =>
      TConstruct(l, TNullable, Some(TTuple(l, [make(p, contents)])))
    | (UFalse(l), _) => TConst(l, TBool(false))
    | (UTrue(l), _) => TConst(l, TBool(true))
    | (UString(l, s), _) => TConst(l, TString(s))
    | (UInt(l, i), _) => TConst(l, TInt(i))
    | (UFloat(l, f), _) => TConst(l, TFloat(f))
    | (UTuple(l, t), Tuple({contents})) =>
      TTuple(l, Array.zipByU(t, contents, (. p, ty) => make(p, ty.contents)))
    | (UList(l, a), List({contents})) => makeList(a, contents, ~tail=TConstruct(l, TList, None))
    | (UListWithTailBinding(_, l, tail), List({contents})) =>
      makeList(l, contents, ~tail=make(tail, contents))
    | (UDict(l, d), Dict(tys, {contents: ks})) =>
      let ks = ks->SetString.toArray->Array.mapU((. k) => (k, tys))->MapString.fromArray
      let d =
        d
        ->MapString.fromArray
        ->MapString.mergeU(ks, (. _, p, ty) =>
          switch (p, ty) {
          | (None, None) | (Some(_), None) => None
          | (Some(p), Some({contents})) => Some(make(p, contents))
          | (None, Some(_)) => Some(TAny(l))
          }
        )
      TDict(l, MapString.toArray(d))
    | (URecord(l, o), Record({contents})) =>
      let r = make_record(o, contents, ~loc=l)
      TRecord(l, MapString.toArray(r))
    | (UBinding(l, "_"), _) => TAny(l)
    | (UBinding(l, b), Nullable(_)) => TOptionalVar(l, b)
    | (UBinding(l, b), _) => TVar(l, b)
    | _ => raise(Exit2(Js.Json.stringifyAny(p), Js.Json.stringifyAny(Typescheme.debug(ref(ty)))))
    }

  and make_record = (x, ty, ~loc) => {
    let r = MapString.fromArray(x)
    MapString.mergeU(r, ty, (. _, p, ty) =>
      switch (p, ty) {
      | (None, None) | (Some(_), None) => None
      | (Some(p), Some({contents})) => Some(make(p, contents))
      | (None, Some(_)) => Some(TAny(loc))
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
    | TRecord(_, r) =>
      "{" ++ Array.joinWith(r, ", ", ((k, v)) => keyValuesToString(k, toString(v))) ++ "}"
    | TDict(_, r) =>
      "<" ++ Array.joinWith(r, ", ", ((k, v)) => keyValuesToString(k, toString(v))) ++ ">"
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
}

type rec node =
  | TText(string, Parser.trim)
  // The first echo item that isn't null will be returned.
  | TEcho({loc: Debug.loc, nullables: array<Parser.echo>, default: Parser.echo})
  | TMatch(Debug.loc, NonEmpty.t<Pattern.t>, NonEmpty.t<case>)
  | TMapList(Debug.loc, Pattern.t, NonEmpty.t<case>)
  | TMapDict(Debug.loc, Pattern.t, NonEmpty.t<case>)
  | TComponent({
      loc: Debug.loc,
      props: array<(string, Pattern.t)>,
      children: array<(string, child)>,
      val: string,
    })

and nodes = array<node>

and case = {pats: NonEmpty.t<NonEmpty.t<Pattern.t>>, nodes: nodes}

and child = TChildName(string) | TChildBlock(nodes)

type t = {
  nodes: nodes,
  prop_types: Typescheme.props,
  child_types: Typescheme.Child.props,
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
  | _ => raise(Exit(Debug.typeMismatch(tref1, tref2, ~loc, ~name, Typescheme.toString)))
  }

@raises(Exit)
and unifyTuple = (t1, t2, mode, ~loc, ~name) => {
  if Array.size(t1.contents) == Array.size(t2.contents) {
    Array.zip(t1.contents, t2.contents)->Array.forEachU((. (a, b)) =>
      unify(a, b, mode, ~loc, ~name)
    )
  } else {
    raise(Exit(Debug.tupleSizeMismatch(Array.size(t1.contents), Array.size(t2.contents), ~name)))
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
    raise(Exit(Debug.cantNarrowType(t1, t2, Typescheme.record_toString)))
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
      raise(Exit(Debug.missingProp(k, v, ~name, ~comp, ~loc, Typescheme.toString)))
    | (Some(_), None) | (None, None) => None
    }
  )
  t1 := r
  t2 := r
}

@raises(Exit)
let unify_child = (a, b, ~loc) =>
  if Typescheme.Child.equal(a, b) {
    ()
  } else {
    raise(Exit(Debug.childTypeMismatch(a, b, ~loc, Typescheme.Child.toString)))
  }

type root = [#Root | #Component]

module Context = {
  type t = {
    global: ref<MapString.t<Typescheme.t>>,
    scope: MapString.t<Typescheme.t>,
    children: ref<MapString.t<Typescheme.Child.t>>,
    root: root,
  }

  let make = root => {
    global: ref(MapString.empty),
    scope: MapString.empty,
    children: ref(MapString.empty),
    root: root,
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
  let updateChild = (ctx, k, v, ~loc) =>
    switch ctx.root {
    | #Root => raise(Exit(Debug.childNotAllowedInRoot(loc)))
    | #Component =>
      ctx.children :=
        MapString.updateU(ctx.children.contents, k, (. v') =>
          switch v' {
          | None => Some(v)
          | Some(v') as r =>
            unify_child(v', v, ~loc)
            r
          }
        )
    }

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
  let rec fromPattern = (~default, pattern, q, ~name) => {
    module T = Typescheme
    switch pattern {
    | UPat.UNull(_) => T.nullable(T.unknown())
    | USome(_, x) => T.nullable(fromPattern(x, q, ~default=T.unknown(), ~name))
    | UFalse(_) | UTrue(_) => T.boolean()
    | UString(_) => T.string()
    | UInt(_) => T.int()
    | UFloat(_) => T.float()
    | UTuple(_, t) =>
      let types = Array.mapU(t, (. x) => fromPattern(x, q, ~default, ~name))
      T.tuple(types)
    | UList(loc, a) =>
      let t = T.unknown()
      Array.forEachU(a, (. x) =>
        unify(t, fromPattern(x, q, ~default=t, ~name), Expand, ~loc, ~name)
      )
      T.list(t)
    | UListWithTailBinding(loc, a, UBinding(_, b)) =>
      let t = T.unknown()
      Array.forEachU(a, (. x) =>
        unify(t, fromPattern(x, ~default=t, q, ~name), Expand, ~loc, ~name)
      )
      let t = T.list(t)
      Queue.add(q, (b, t))
      t
    | UListWithTailBinding(_, _, _) => assert false // error goes here
    | UDict(loc, d) =>
      let t = T.unknown()
      Array.forEachU(d, (. (_, x)) =>
        unify(t, fromPattern(x, q, ~default=t, ~name), Expand, ~loc, ~name)
      )
      let ks = d->Array.mapU((. (k, _)) => k)->SetString.fromArray
      ref(T.Dict(t, ref(ks)))
    | URecord(_, o) =>
      let types = o->Array.mapU((. (k, x)) => {
        let types = fromPattern(x, q, ~default=T.unknown(), ~name)
        (k, types)
      })
      T.record(types)
    | UBinding(_, "_") => default
    | UBinding(_, b) =>
      Queue.add(q, (b, default))
      default
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
  let rec fromPattern = (~default, ~name, pattern: UPat.t, ctx) => {
    module T = Typescheme
    switch pattern {
    | UNull(_) => T.nullable(T.unknown())
    | USome(_, x) => T.nullable(fromPattern(~default=T.unknown(), x, ctx, ~name))
    | UFalse(_) | UTrue(_) => T.boolean()
    | UString(_) => T.string()
    | UInt(_) => T.int()
    | UFloat(_) => T.float()
    | UTuple(_, t) =>
      let types = Array.mapU(t, (. x) => fromPattern(~default=T.unknown(), x, ctx, ~name))
      T.tuple(types)
    | UList(loc, a) =>
      let t = T.unknown()
      Array.forEachU(a, (. x) =>
        unify(t, fromPattern(~default=t, x, ctx, ~name), Narrow, ~loc, ~name)
      )
      T.list(t)
    | UListWithTailBinding(loc, a, UBinding(bloc, b)) =>
      let t = T.unknown()
      Array.forEachU(a, (. x) =>
        unify(t, fromPattern(~default=t, x, ctx, ~name), Narrow, ~loc, ~name)
      )
      let t = T.list(t)
      updateContext(ctx, b, t, ~loc=bloc, ~name)
    | UListWithTailBinding(_, _, _) => assert false // error goes here
    | UDict(loc, d) =>
      let t = T.unknown()
      Array.forEachU(d, (. (_, x)) =>
        unify(t, fromPattern(~default=t, x, ctx, ~name), Narrow, ~loc, ~name)
      )
      let ks = d->Array.mapU((. (k, _)) => k)->SetString.fromArray
      ref(T.Dict(t, ref(ks)))
    | URecord(_, o) => T.record2(fromPattern_record(o, ctx, ~name))
    | UBinding(_, "_") => default
    | UBinding(loc, b) => updateContext(ctx, b, default, ~loc, ~name)
    }
  }

  and fromPattern_record = (x, ctx, ~name) => {
    let types = Array.mapU(x, (. (k, x)) => (
      k,
      fromPattern(~default=Typescheme.unknown(), x, ctx, ~name),
    ))
    MapString.fromArray(types)
  }

  @raises(Exit)
  let unifyMatchCases = (bindingArray, cases, ctx, ~name) =>
    if NonEmpty.size(bindingArray) != NonEmpty.size(cases) {
      let loc = NonEmpty.hd(bindingArray)->UPat.toLocation
      raise(Exit(Debug.patternNumberMismatch(~loc, ~name)))
    } else {
      NonEmpty.zipExn(bindingArray, cases)->NonEmpty.map((. (pat, ty)) => {
        let t = fromPattern(pat, ctx, ~default=Typescheme.unknown(), ~name)
        unify(ty, t, Expand, ~loc=UPat.toLocation(pat), ~name)
        Pattern.make(pat, t.contents)
      })
    }

  @raises(Exit)
  let unifyMapListCases = (pat, tys, ctx, ~loc, ~name) => {
    let ty = switch NonEmpty.toArray(tys) {
    | [hd] => Typescheme.list(hd)
    | [hd, tl] =>
      unify(Typescheme.int(), tl, Expand, ~loc, ~name)
      Typescheme.list(hd)
    | _ => raise(Exit(Debug.mapPatternSizeMismatch(~loc, ~name)))
    }
    let t = fromPattern(~default=Typescheme.unknown(), pat, ctx, ~name)
    unify(ty, t, Expand, ~loc, ~name)
    Pattern.make(pat, ty.contents)
  }

  @raises(Exit)
  let unifyMapDictCases = (pat, tys, ctx, ~loc, ~name) => {
    let ty = switch NonEmpty.toArray(tys) {
    | [hd] => Typescheme.dict(hd)
    | [hd, tl] =>
      unify(Typescheme.string(), tl, Expand, ~loc, ~name)
      Typescheme.dict(hd)
    | _ => raise(Exit(Debug.mapPatternSizeMismatch(~loc, ~name)))
    }
    let t = fromPattern(~default=Typescheme.unknown(), pat, ctx, ~name)
    unify(ty, t, Expand, ~loc, ~name)
    Pattern.make(pat, ty.contents)
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
      | Parser.EBinding(loc, binding, _) => Context.update(ctx, binding, echo(), ~loc, ~name)
      | EChild(loc, child) => Context.updateChild(ctx, child, Child.child(), ~loc)
      | EString(_, _, _) | EInt(_, _, _) | EFloat(_, _, _) => ()
      }
    | Some(Parser.EBinding(loc, binding, _)) =>
      Context.update(ctx, binding, nullable(echo()), ~loc, ~name)
      aux(succ(i))
    | Some(EString(_, _, _) | EInt(_, _, _) | EFloat(_, _, _)) =>
      raise(Exit(Debug.nonNullableEchoLiteral()))
    | Some(EChild(loc, child)) =>
      Context.updateChild(ctx, child, Child.nullable(), ~loc)
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
  | Source.Acutis(_, {prop_types, child_types, _}) => (prop_types, child_types)
  | Function(_, props, children, _) => (props, children)
  }

@raises(Exit)
let rec makeCases = (cases, ctx, ~loc, ~name, g) => {
  let (casetypes, cases) =
    cases
    ->NonEmpty.map((. {Parser.patterns: pats, nodes}) => {
      let bindings = Queue.make()
      let casetypes =
        pats
        ->NonEmpty.map((. pattern) =>
          NonEmpty.map(pattern, (. p) => (
            UPat.toLocation(p),
            Local.fromPattern(p, bindings, ~default=Typescheme.unknown(), ~name),
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
    pats: NonEmpty.map(pats, (. pats) =>
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
    | Parser.UText(s, t) => TText(s, t)
    | UEcho({loc, nullables, default}) =>
      unifyEchoes(nullables, default, ctx, ~name)->ignore
      TEcho({loc: loc, nullables: nullables, default: default})
    | UComponent({loc, props, children, name: cname}) =>
      let (propTypes, propTypesChildren) = getTypes(Utils.Dagmap.getExn(g, ~name, ~key=cname, ~loc))
      let t = Global.fromPattern_record(props, ctx, ~name)
      // The original proptypes should not mutate.
      unifyRecord_exact(ref(t), ref(Typescheme.copy_record(propTypes)), ~loc, ~comp=cname, ~name)
      let children =
        children
        ->MapString.fromArray
        ->MapString.mergeU(propTypesChildren, (. _, c, ty) =>
          switch (c, ty) {
          | (None, None) | (None, Some({contents: NullableChild})) => None
          | (None, Some({contents: Child})) => assert false // error message goes here
          | (Some(_), None) => assert false // error message goes here
          | (Some(UChildName(loc, c)), Some({contents: ty})) =>
            Context.updateChild(ctx, c, ref(ty), ~loc)
            Some(TChildName(c))
          | (Some(UChildBlock(_, nodes)), Some(_)) =>
            Some(TChildBlock(makeNodes(nodes, ctx, ~name=cname, g)))
          }
        )
        ->MapString.toArray
      let props = Pattern.make_record(props, t, ~loc)->MapString.toArray
      TComponent({loc: loc, props: props, children: children, val: cname})
    | UMatch(loc, bindingArray, cases) =>
      // Add a default wildcard for patterns without indices
      let (caseTypes, cases) = makeCases(cases, ctx, ~loc, ~name, g)
      let patterns = Global.unifyMatchCases(bindingArray, caseTypes, ctx, ~name)
      TMatch(loc, patterns, cases)
    | UMapList(loc, pattern, cases) =>
      // Add a default wildcard for patterns without indices
      let cases = NonEmpty.map(cases, (. case) => {
        ...case,
        patterns: NonEmpty.map(case.patterns, (. pattern) =>
          switch NonEmpty.toArray(pattern) {
          | [hd] => NonEmpty.two(hd, UBinding(loc, "_"))
          | _ => pattern
          }
        ),
      })
      let (casetypes, cases) = makeCases(cases, ctx, ~loc, ~name, g)
      let pattern = Global.unifyMapListCases(pattern, casetypes, ctx, ~loc, ~name)
      TMapList(loc, pattern, cases)
    | UMapDict(loc, pattern, cases) =>
      // Add a default wildcard for patterns without indices
      let cases = NonEmpty.map(cases, (. case) => {
        ...case,
        patterns: NonEmpty.map(case.patterns, (. pattern) =>
          switch NonEmpty.toArray(pattern) {
          | [hd] => NonEmpty.two(hd, UBinding(loc, "_"))
          | _ => pattern
          }
        ),
      })
      let (casetypes, cases) = makeCases(cases, ctx, ~loc, ~name, g)
      let pattern = Global.unifyMapDictCases(pattern, casetypes, ctx, ~loc, ~name)
      TMapDict(loc, pattern, cases)
    }
  )

and make = (name, ast, g, root) => {
  let ctx = Context.make(root)
  let nodes = makeNodes(ast, ctx, ~name, g)
  let ast = {
    nodes: nodes,
    prop_types: ctx.global.contents,
    child_types: ctx.children.contents,
  }
  ast
}

let makeSrc = (. g, x) =>
  switch x {
  | Source.Acutis(name, ast) => Source.src(~name, make(name, ast, g, #Component))
  | Function(name, p, c, f) => Source.fnU(~name, p, c, f)
  }

let makeComponents = a => a->Utils.Dagmap.make(~f=makeSrc)->Utils.Dagmap.linkAll

let make = (name, ast, components) => make(name, ast, Utils.Dagmap.prelinked(components), #Root)
