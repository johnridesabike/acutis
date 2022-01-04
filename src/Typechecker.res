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
    | TRecord(Debug.t, array<(string, t)>)
    | TDict(Debug.t, array<(string, t)>)
    | TVar(Debug.t, string) // any binding
    | TOptionalVar(Debug.t, string) // any binding, may not be set
    | TAny(Debug.t) // ignored wildcard _

  let rec makeList = (a, ty, ~tail) => {
    let r = ref(tail)
    for i in Array.size(a) - 1 downto 0 {
      let p = Array.getUnsafe(a, i)
      let debug = UPat.debug(p)
      r := TConstruct(debug, TList, Some(TTuple(debug, [make(p, ty), r.contents])))
    }
    r.contents
  }

  and make = (p: UPat.t, ty) =>
    switch (p, ty) {
    | (UNull(dbg), Ty.Nullable(_)) => TConstruct(dbg, TNullable, None)
    | (USome(dbg, p), Nullable({contents})) =>
      TConstruct(dbg, TNullable, Some(TTuple(dbg, [make(p, contents)])))
    | (UFalse(dbg), _) => TConst(dbg, TBool(false))
    | (UTrue(dbg), _) => TConst(dbg, TBool(true))
    | (UString(dbg, s), _) => TConst(dbg, TString(s))
    | (UInt(dbg, i), _) => TConst(dbg, TInt(i))
    | (UFloat(dbg, f), _) => TConst(dbg, TFloat(f))
    | (UTuple(dbg, t), Tuple({contents})) =>
      TTuple(dbg, Array.zipByU(t, contents, (. p, ty) => make(p, ty.contents)))
    | (UList(dbg, a), List({contents})) => makeList(a, contents, ~tail=TConstruct(dbg, TList, None))
    | (UListWithTailBinding(_, l, tail), List({contents})) =>
      makeList(l, contents, ~tail=make(tail, contents))
    | (UDict(dbg, d), Dict(tys, {contents: ks})) =>
      let ks = ks->SetString.toArray->Array.mapU((. k) => (k, tys))->MapString.fromArray
      let d =
        d
        ->MapString.fromArray
        ->MapString.mergeU(ks, (. _, p, ty) =>
          switch (p, ty) {
          | (None, None) | (Some(_), None) => None
          | (Some(p), Some({contents})) => Some(make(p, contents))
          | (None, Some(_)) => Some(TAny(dbg))
          }
        )
      TDict(dbg, MapString.toArray(d))
    | (URecord(dbg, o), Record({contents})) =>
      let r = make_record(o, contents, dbg)
      TRecord(dbg, MapString.toArray(r))
    | (UBinding(dbg, "_"), _) => TAny(dbg)
    | (UBinding(dbg, b), Nullable(_)) => TOptionalVar(dbg, b)
    | (UBinding(dbg, b), _) => TVar(dbg, b)
    | _ => assert false
    }

  and make_record = (x, ty, debug) => {
    let r = MapString.fromArray(x)
    MapString.mergeU(r, ty, (. _, p, ty) =>
      switch (p, ty) {
      | (None, None) | (Some(_), None) => None
      | (Some(p), Some({contents})) => Some(make(p, contents))
      | (None, Some(_)) => Some(TAny(debug))
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

  let debug = t =>
    switch t {
    | TConst(l, _)
    | TConstruct(l, _, _)
    | TTuple(l, _)
    | TRecord(l, _)
    | TDict(l, _)
    | TVar(l, _)
    | TOptionalVar(l, _)
    | TAny(l) => l
    }
}

type rec node =
  | TText(string, Parser.trim)
  // The first echo item that isn't null will be returned.
  | TEcho(Debug.t, array<Parser.echo>, Parser.echo)
  | TMatch(Debug.t, NonEmpty.t<Pattern.t>, NonEmpty.t<case>)
  | TMapList(Debug.t, Pattern.t, NonEmpty.t<case>)
  | TMapDict(Debug.t, Pattern.t, NonEmpty.t<case>)
  | TComponent(Debug.t, string, array<(string, Pattern.t)>, array<(string, child)>)

and nodes = array<node>

and case = {pats: NonEmpty.t<NonEmpty.t<Pattern.t>>, nodes: nodes}

and child = TChildName(string) | TChildBlock(nodes)

type t = {
  nodes: nodes,
  prop_types: Ty.props,
  child_types: Ty.Child.props,
}

// If a type is incomplete, then it can be unified more liberally. (Unused).
// type complete = Complete | Incomplete

type mode = Expand | Narrow

@raises(Exit)
let rec unify = (tref1, tref2, mode, debug) =>
  switch (tref1.contents, tref2.contents) {
  | (Ty.Boolean, Ty.Boolean)
  | (Int, Int)
  | (Float, Float)
  | (String, String) => ()
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
  | (Tuple(t1), Tuple(t2)) => unifyTuple(t1, t2, mode, debug)
  | (Record(t1), Record(t2)) =>
    switch mode {
    | Expand => unifyRecord_expand(t1, t2, debug)
    | Narrow => unifyRecord_narrow(t1, t2, debug)
    }
  | _ => raise(Exit(Debug.typeMismatch(debug, tref1, tref2, Ty.toString)))
  }

@raises(Exit)
and unifyTuple = (t1, t2, mode, debug) => {
  if Array.size(t1.contents) == Array.size(t2.contents) {
    Array.zip(t1.contents, t2.contents)->Array.forEachU((. (a, b)) => unify(a, b, mode, debug))
  } else {
    raise(Exit(Debug.tupleSizeMismatch(debug, Array.size(t1.contents), Array.size(t2.contents))))
  }
}

@raises(Exit)
and unifyRecord_expand = (t1, t2, debug) => {
  let r = MapString.mergeU(t1.contents, t2.contents, (. _, v1, v2) => {
    switch (v1, v2) {
    | (Some(v1) as r, Some(v2)) =>
      unify(v1, v2, Expand, debug)
      r
    | (Some(_) as r, None) | (None, Some(_) as r) => r
    | (None, None) => None
    }
  })
  t1 := r
  t2 := r
}

@raises(Exit)
and unifyRecord_narrow = (t1, t2, debug) => {
  let r = MapString.mergeU(t1.contents, t2.contents, (. _, v1, v2) =>
    switch (v1, v2) {
    | (Some(v1) as r, Some(v2)) =>
      unify(v1, v2, Expand, debug)
      r
    | (Some(_), None) | (None, Some(_)) => None
    | (None, None) => None
    }
  )
  if MapString.isEmpty(r) {
    raise(Exit(Debug.cantNarrowType(debug, t1, t2, Ty.record_toString)))
  } else {
    t1 := r
    t2 := r
  }
}

@raises(Exit)
let unifyRecord_exact = (t1, t2, debug, ~comp) => {
  let r = MapString.mergeU(t1.contents, t2.contents, (. k, v1, v2) =>
    switch (v1, v2) {
    | (Some(v1) as r, Some(v2)) =>
      unify(v1, v2, Expand, debug)
      r
    | (None, Some(v)) => raise(Exit(Debug.missingProp(debug, k, v, ~comp, Ty.toString)))
    | (Some(_), None) | (None, None) => None
    }
  )
  t1 := r
  t2 := r
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
            unify(v', v, Expand, debug)
            r
          }
        )
    | Some(v') => unify(v', v, Expand, debug)
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
  let addScope = (ctx, q, debug) => {
    // Merge all of the bindings into a single map & typecheck them.
    let newscope = Queue.reduceU(q, MapString.empty, (. newscope, (k, v)) => {
      MapString.updateU(newscope, k, (. v') =>
        switch v' {
        | None => Some(v)
        | Some(v') as r =>
          unify(v', v, Expand, debug)
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

@raises(Exit)
let rec fromPattern = (~default, ~f, mode, pattern) => {
  switch pattern {
  | UPat.UNull(_) => Ty.nullable(Ty.unknown())
  | USome(_, x) => Ty.nullable(fromPattern(mode, x, ~f, ~default=Ty.unknown()))
  | UFalse(_) | UTrue(_) => Ty.boolean()
  | UString(_) => Ty.string()
  | UInt(_) => Ty.int()
  | UFloat(_) => Ty.float()
  | UTuple(_, t) =>
    let types = Array.mapU(t, (. x) => fromPattern(mode, x, ~f, ~default))
    Ty.tuple(types)
  | UList(debug, a) =>
    let t = Ty.unknown()
    Array.forEachU(a, (. x) => unify(t, fromPattern(mode, x, ~f, ~default=t), mode, debug))
    Ty.list(t)
  | UListWithTailBinding(debug, a, UBinding(_, b)) =>
    let t = Ty.unknown()
    Array.forEachU(a, (. x) => unify(t, fromPattern(mode, x, ~default=t, ~f), mode, debug))
    let t = Ty.list(t)
    f(. b, t, debug)
  | UListWithTailBinding(debug, _, _) => raise(Exit(Debug.tailBindingClash(debug)))
  | UDict(debug, d) =>
    let t = Ty.unknown()
    Array.forEachU(d, (. (_, x)) => unify(t, fromPattern(mode, x, ~f, ~default=t), mode, debug))
    let ks = d->Array.mapU((. (k, _)) => k)->SetString.fromArray
    ref(Ty.Dict(t, ref(ks)))
  | URecord(_, o) =>
    let types = o->Array.mapU((. (k, x)) => {
      let types = fromPattern(mode, x, ~f, ~default=Ty.unknown())
      (k, types)
    })
    Ty.record(types)
  | UBinding(_, "_") => default
  | UBinding(debug, b) => f(. b, default, debug)
  }
}

@raise(Exit)
let updateContext = (ctx, . k, v, debug) =>
  switch MapString.get(ctx.Context.scope, k) {
  | None =>
    switch MapString.get(ctx.global.contents, k) {
    | None =>
      ctx.global := MapString.set(ctx.global.contents, k, v)
      v
    | Some(v') =>
      unify(v, v', Expand, debug)
      v'
    }
  | Some(v') =>
    unify(v, v', Expand, debug)
    v'
  }

@raises(Exit)
let fromProps = (x, ctx) => {
  Array.mapU(x, (. (k, x)) => {
    let types = fromPattern(Narrow, x, ~default=Ty.unknown(), ~f=updateContext(ctx))
    (k, types)
  })->MapString.fromArray
}

@raises(Exit)
let unifyMatchCases = (bindingArray, cases, ctx) => {
  if NonEmpty.size(bindingArray) != NonEmpty.size(cases) {
    let debug = NonEmpty.hd(bindingArray)->UPat.debug
    raise(Exit(Debug.patternNumberMismatch(debug)))
  } else {
    NonEmpty.zipExn(bindingArray, cases)->NonEmpty.map((. (pat, ty)) => {
      let t = fromPattern(Narrow, pat, ~default=Ty.unknown(), ~f=updateContext(ctx))
      unify(ty, t, Expand, UPat.debug(pat))
      Pattern.make(pat, t.contents)
    })
  }
}

module type MapTy = {
  let make: (. Ty.t) => Ty.t
  let key: (. unit) => Ty.t
}

module ListTy = {
  let make = (. t) => Ty.list(t)
  let key = (. ()) => Ty.int()
}

module DictTy = {
  let make = (. t) => Ty.dict(t)
  let key = (. ()) => Ty.string()
}

@raises(Exit)
let unifyMap = (module(M: MapTy), pat, tys, ctx, debug) => {
  let ty = switch NonEmpty.toArray(tys) {
  | [hd] => M.make(. hd)
  | [hd, tl] =>
    unify(M.key(.), tl, Expand, debug)
    M.make(. hd)
  | _ => raise(Exit(Debug.mapPatternSizeMismatch(debug)))
  }
  let t = fromPattern(Narrow, pat, ~default=Ty.unknown(), ~f=updateContext(ctx))
  unify(ty, t, Expand, debug)
  Pattern.make(pat, ty.contents)
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

@raises(Exit)
let unifyNestedNonEmpty = (cases: NonEmpty.t<NonEmpty.t<(_, _)>>) => {
  let (_, r) = NonEmpty.reduceHd(cases, (. casea, caseb) => {
    NonEmpty.zipByExn(casea, caseb, (. (_, casea), (debug, caseb)) => {
      unify(casea, caseb, Expand, debug)
      (debug, casea)
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
let rec makeCases = (cases, ctx, g, debug) => {
  let (casetypes, cases) =
    cases
    ->NonEmpty.map((. {Parser.patterns: pats, nodes}) => {
      let bindings = Queue.make()
      let f = (. k, v, _) => {
        Queue.add(bindings, (k, v))
        v
      }
      let casetypes =
        pats
        ->NonEmpty.map((. pattern) =>
          NonEmpty.map(pattern, (. p) => (
            UPat.debug(p),
            fromPattern(Expand, p, ~default=Ty.unknown(), ~f),
          ))
        )
        ->NonEmpty.reduceHd((. pat1, pat2) =>
          NonEmpty.zipByExn(pat1, pat2, (. (d, a), (_, b)) => {
            unify(a, b, Expand, debug)
            (d, a)
          })
        )
      let ctx = Context.addScope(ctx, bindings, debug)
      (casetypes, (pats, nodes, ctx))
    })
    ->NonEmpty.unzip
  // We need to make the typed patterns AFTER the entire expression is
  // type-checked so records and dictionary fields expand correctly.
  let casetypes = unifyNestedNonEmpty(casetypes)
  let cases = NonEmpty.map(cases, (. (pats, nodes, ctx)) => {
    pats: NonEmpty.map(pats, (. pats) =>
      NonEmpty.zipByExn(pats, casetypes, (. p, ty) => Pattern.make(p, ty.contents))
    ),
    nodes: makeNodes(nodes, ctx, g),
  })
  (casetypes, cases)
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
      let t = fromProps(props, ctx)
      // The original proptypes should not mutate.
      unifyRecord_exact(ref(t), ref(Ty.copy_record(propTypes)), debug, ~comp=cname)
      let children =
        children
        ->MapString.fromArray
        ->MapString.mergeU(propTypesChildren, (. k, c, ty) =>
          switch (c, ty) {
          | (None, None) | (None, Some({contents: NullableChild})) => None
          | (None, Some({contents: Child})) =>
            raise(Exit(Debug.missingChild(debug, ~comp=cname, k)))
          | (Some(_), None) => raise(Exit(Debug.extraChild(debug, ~comp=cname, k)))
          | (Some(UChildName(debug, c)), Some({contents: ty})) =>
            Context.updateChild(ctx, c, ref(ty), debug)
            Some(TChildName(c))
          | (Some(UChildBlock(_, nodes)), Some(_)) => Some(TChildBlock(makeNodes(nodes, ctx, g)))
          }
        )
        ->MapString.toArray
      let props = Pattern.make_record(props, t, debug)->MapString.toArray
      TComponent(debug, cname, props, children)
    | UMatch(debug, bindingArray, cases) =>
      // Add a default wildcard for patterns without indices
      let (caseTypes, cases) = makeCases(cases, ctx, g, debug)
      let patterns = unifyMatchCases(bindingArray, caseTypes, ctx)
      TMatch(debug, patterns, cases)
    | UMapList(debug, pattern, cases) =>
      // Add a default wildcard for patterns without indices
      let cases = NonEmpty.map(cases, (. case) => {
        ...case,
        patterns: NonEmpty.map(case.patterns, (. pattern) =>
          switch NonEmpty.toArray(pattern) {
          | [hd] => NonEmpty.two(hd, UBinding(debug, "_"))
          | _ => pattern
          }
        ),
      })
      let (casetypes, cases) = makeCases(cases, ctx, g, debug)
      let pattern = unifyMap(module(ListTy), pattern, casetypes, ctx, debug)
      TMapList(debug, pattern, cases)
    | UMapDict(debug, pattern, cases) =>
      // Add a default wildcard for patterns without indices
      let cases = NonEmpty.map(cases, (. case) => {
        ...case,
        patterns: NonEmpty.map(case.patterns, (. pattern) =>
          switch NonEmpty.toArray(pattern) {
          | [hd] => NonEmpty.two(hd, UBinding(debug, "_"))
          | _ => pattern
          }
        ),
      })
      let (casetypes, cases) = makeCases(cases, ctx, g, debug)
      let pattern = unifyMap(module(DictTy), pattern, casetypes, ctx, debug)
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
