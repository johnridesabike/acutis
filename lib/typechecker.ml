(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module F = Format
module Ty = Typescheme
module V = Ty.Variant

exception Clash

(**
  When we unify a type during destructuring, we expand any structural types
  (records, enums).

  When we unify a type during constructing literal values, record types are
  narrowed (we use a subset of both) and enum behavior depends on its row
  property.

  When we unify a type during constructing with a variable, then we expand
  records and open enums in the variable's type.
*)

let rec open_rows_bool_union_aux = function
  | None -> Some (ref Map.String.empty)
  | Some ty as x ->
      Map.String.iter (fun _ v -> open_rows v) !ty;
      x

and open_rows ty =
  match !ty with
  | Ty.Enum ty -> (
      match ty.extra with
      | `Extra_none -> ty.row <- `Open
      | `Extra_bool -> ty.cases <- Ty.Enum.false_and_true_cases)
  | Union (_, ty) -> (
      match (ty.extra, ty.cases) with
      | `Extra_bool, Int cases ->
          let f = open_rows_bool_union_aux in
          ty.cases <- Int (Map.Int.update 0 f cases |> Map.Int.update 1 f)
      | _, Int cases ->
          Map.Int.iter
            (fun _ v -> Map.String.iter (fun _ v -> open_rows v) !v)
            cases;
          ty.row <- `Open
      | _, String cases ->
          Map.String.iter
            (fun _ v -> Map.String.iter (fun _ v -> open_rows v) !v)
            cases;
          ty.row <- `Open)
  | Tuple a -> List.iter open_rows a
  | Record ty -> Map.String.iter (fun _ v -> open_rows v) !ty
  | Nullable ty | List ty | Dict (ty, _) -> open_rows ty
  | Unknown row -> row := `Open
  | Int | Float | String | Echo -> ()

type mode =
  | Destructure_expand (* Record a and b both take on each other's fields. *)
  | Construct_literal (* Record a is narrowed to a subset of a and b. *)
  | Construct_var (* Record a expands into b only. *)

let unify_enum_cases _ a b =
  match (a, b) with
  | V.String a, V.String b -> V.String (Set.String.union a b)
  | Int a, Int b -> Int (Set.Int.union a b)
  | _ -> raise_notrace Clash

let subset_enum_cases _ a b =
  let success =
    match (a, b) with
    | V.String a, V.String b -> Set.String.subset b a
    | Int a, Int b -> Set.Int.subset b a
    | _ -> false
  in
  if not success then raise_notrace Clash

let unify_variant ~unify_cases ~subset_cases mode a b =
  match mode with
  | Destructure_expand ->
      (match (a.V.row, b.V.row) with
      | `Open, _ | _, `Open -> a.row <- `Open
      | _ -> ());
      a.cases <- unify_cases mode a.cases b.cases
  | Construct_literal -> (
      match (a.row, b.row) with
      | `Open, (`Closed | `Open) -> a.cases <- unify_cases mode a.cases b.cases
      | `Closed, (`Closed | `Open) -> subset_cases mode a.cases b.cases)
  | Construct_var -> (
      match (a.row, b.row) with
      | `Closed, `Closed -> subset_cases mode a.cases b.cases
      | `Open, `Closed -> a.cases <- unify_cases mode a.cases b.cases
      | `Open, `Open ->
          let cases = unify_cases mode a.cases b.cases in
          a.cases <- cases;
          b.cases <- cases
      | `Closed, `Open -> raise_notrace Clash)

let rec unify mode aty bty =
  match (!aty, !bty) with
  | Ty.Int, Ty.Int | Float, Float | String, String | Echo, Echo -> ()
  | Echo, ((Int | Float | String | Enum _) as t) -> aty := t
  | ((Int | Float | String | Enum _) as t), Echo -> bty := t
  | Nullable t1, Nullable t2 -> unify mode t1 t2
  | List t1, List t2 -> unify mode t1 t2
  | Dict (t1, ks1), Dict (t2, ks2) ->
      let ks' = Set.String.union !ks1 !ks2 in
      ks1 := ks';
      ks2 := ks';
      unify mode t1 t2
  | Tuple t1, Tuple t2 -> (
      try List.iter2 (unify mode) t1 t2
      with Invalid_argument _ -> raise_notrace Clash)
  | Record a, Record b -> unify_record mode a b
  | Enum a, Enum b ->
      unify_variant ~unify_cases:unify_enum_cases
        ~subset_cases:subset_enum_cases mode a b
  | Union (ka, a), Union (kb, b) when String.equal ka kb ->
      unify_variant ~unify_cases:unify_union_cases
        ~subset_cases:subset_union_cases mode a b
  | Unknown { contents = `Open }, t ->
      (match mode with Destructure_expand -> open_rows bty | _ -> ());
      aty := t
  | Unknown _, t -> aty := t
  | t, Unknown _ -> bty := t
  | _ -> raise_notrace Clash

and unify_record mode a b =
  match mode with
  | Destructure_expand ->
      let f _ a b =
        match (a, b) with
        | (Some a as a'), Some b ->
            unify mode a b;
            a'
        | None, Some b ->
            open_rows b;
            Some b
        | x, None -> x
      in
      a := Map.String.merge f !a !b
  | Construct_var ->
      let f _ a b =
        unify mode a b;
        Some b
      in
      b := Map.String.union f !a !b
  | Construct_literal ->
      let f _ a b =
        match (a, b) with
        | (Some a as x), Some b ->
            unify mode a b;
            x
        | Some _, None -> raise_notrace Clash
        | None, _ -> None
      in
      a := Map.String.merge f !a !b

and unify_union_cases mode a b =
  let f _ a b =
    unify_record mode a b;
    Some a
  in
  match (a, b) with
  | V.String a, V.String b -> V.String (Map.String.union f a b)
  | Int a, Int b -> Int (Map.Int.union f a b)
  | _ -> raise_notrace Clash

and subset_union_cases mode a b =
  let f _ a b =
    match (a, b) with
    | (Some _ as x), None -> x
    | (Some a as x), Some b ->
        unify_record mode a b;
        x
    | None, Some _ -> raise_notrace Clash
    | None, None -> None
  in
  match (a, b) with
  | V.String a, V.String b -> Map.String.merge f a b |> ignore
  | Int a, Int b -> Map.Int.merge f a b |> ignore
  | _ -> raise_notrace Clash

let unify loc mode a b =
  try unify mode a b with Clash -> Error.type_mismatch loc a b

module Pattern = struct
  type constant = [ `Int of int | `String of string | `Float of float ]
  [@@deriving eq]

  type construct = TList | TNullable [@@deriving eq, show]

  type t =
    | TConst of constant * Ty.Enum.t option
    | TConstruct of construct * t option
    | TTuple of t list
    | TRecord of
        (string * constant * Ty.t Ty.Union.t) option
        * t Map.String.t
        * Ty.t Map.String.t ref
    | TDict of t Map.String.t * Set.String.t ref
    | TVar of string
    | TAny
  [@@deriving eq]

  let unknown _ = Ty.unknown ()

  let rec make ~f mode ty = function
    | Ast.Pattern.Int (loc, i) ->
        unify loc mode ty (Ty.int ());
        TConst (`Int i, None)
    | String (loc, s) ->
        unify loc mode ty (Ty.string ());
        TConst (`String s, None)
    | Float (loc, f) ->
        unify loc mode ty (Ty.float ());
        TConst (`Float f, None)
    | Bool (loc, b) ->
        let new_enum =
          match mode with
          | Destructure_expand -> (
              match b with
              | 0 -> Ty.Enum.false_only ()
              | _ -> Ty.Enum.true_only ())
          | Construct_var | Construct_literal -> Ty.Enum.false_and_true ()
        in
        let new_ty = ref (Ty.Enum new_enum) in
        let enum = match !ty with Enum e -> e | _ -> new_enum in
        unify loc mode ty new_ty;
        TConst (`Int b, Some enum)
    | Enum_string (loc, s) ->
        let new_enum =
          match mode with
          | Destructure_expand -> Ty.Enum.string_singleton s `Closed
          | Construct_var | Construct_literal ->
              Ty.Enum.string_singleton s `Open
        in
        let new_ty = ref (Ty.Enum new_enum) in
        let enum = match !ty with Enum e -> e | _ -> new_enum in
        unify loc mode ty new_ty;
        TConst (`String s, Some enum)
    | Enum_int (loc, i) ->
        let new_enum =
          match mode with
          | Destructure_expand -> Ty.Enum.int_singleton i `Closed
          | Construct_var | Construct_literal -> Ty.Enum.int_singleton i `Open
        in
        let new_ty = ref (Ty.Enum new_enum) in
        let enum = match !ty with Enum e -> e | _ -> new_enum in
        unify loc mode ty new_ty;
        TConst (`Int i, Some enum)
    | Nullable (loc, pat) ->
        let tyvar = match !ty with Nullable ty -> ty | _ -> Ty.unknown () in
        let pat =
          match pat with
          | None -> None
          | Some pat -> Some (TTuple [ make ~f mode tyvar pat ])
        in
        unify loc mode ty (Ty.nullable tyvar);
        TConstruct (TNullable, pat)
    | List (loc, l, tl) ->
        let tyvar = match !ty with List ty -> ty | _ -> Ty.unknown () in
        unify loc mode ty (Ty.list tyvar);
        let tl =
          match tl with
          | None -> TConstruct (TList, None)
          | Some tl -> make ~f mode ty tl
        in
        make_list ~tl ~f mode tyvar l
    | Tuple (loc, l) ->
        let new_tyvars = List.map unknown l in
        let tyvars = match !ty with Tuple tys -> tys | _ -> new_tyvars in
        unify loc mode ty (Ty.tuple tyvars);
        TTuple (List.map2 (make ~f mode) tyvars l)
    | Record (loc, Untagged m) ->
        let m = Ast.Dict.to_map m in
        let new_tyvars = Map.String.map unknown m |> ref in
        let tyvars = match !ty with Record tys -> tys | _ -> new_tyvars in
        unify loc mode ty (Ty.internal_record new_tyvars);
        let r = make_record ~f loc mode !tyvars m in
        TRecord (None, r, tyvars)
    | Record (loc, Tagged (k, v, m)) ->
        let m = Ast.Dict.to_map m in
        let new_tyvars = Map.String.map unknown m |> ref in
        let new_tag_ty = Ty.unknown () in
        let row =
          match mode with
          | Destructure_expand -> `Closed
          | Construct_literal | Construct_var -> `Open
        in
        let tag, tag_extra =
          match make ~f mode new_tag_ty v with
          | TConst (((`Int _ | `String _) as tag), None) -> (tag, `Extra_none)
          | TConst (((`Int _ | `String _) as tag), Some { extra; _ }) ->
              (tag, extra) (* booleans *)
          | _ -> Error.bad_union_tag loc new_tag_ty
        in
        let tyvars =
          match !ty with
          | Union (_, enum) -> (
              let tyvars =
                match (tag, enum) with
                | `Int i, { cases = Int cases; _ } -> Map.Int.find_opt i cases
                | `String s, { cases = String cases; _ } ->
                    Map.String.find_opt s cases
                | _ -> None
              in
              match tyvars with Some tv -> tv | None -> new_tyvars)
          | _ -> new_tyvars
        in
        let new_enum =
          match tag with
          | `Int i -> Ty.Union.int_singleton i tyvars row tag_extra
          | `String s -> Ty.Union.string_singleton s tyvars row
        in
        unify loc mode ty (ref (Ty.Union (k, new_enum)));
        let r = make_record ~f loc mode !tyvars m in
        TRecord (Some (k, tag, new_enum), r, tyvars)
    | Dict (loc, m) ->
        let new_kys = ref Set.String.empty in
        let tyvar, kys =
          match !ty with
          | Dict (ty, ks) -> (ty, ks)
          | _ -> (Ty.unknown (), new_kys)
        in
        unify loc mode ty (Ty.internal_dict_keys tyvar new_kys);
        let d = Ast.Dict.to_map m |> Map.String.map (make ~f mode tyvar) in
        TDict (d, kys)
    | Var (loc, "_") -> (
        match mode with
        | Destructure_expand ->
            open_rows ty;
            TAny
        | Construct_literal | Construct_var -> Error.underscore_in_construct loc
        )
    | Var (loc, b) ->
        (match mode with
        | Destructure_expand -> open_rows ty
        | Construct_literal | Construct_var -> ());
        f loc b ty;
        TVar b

  and make_list ~f ~tl mode ty = function
    | [] -> tl
    | p :: l ->
        let hd = make ~f mode ty p in
        let tl = make_list ~f ~tl mode ty l in
        TConstruct (TList, Some (TTuple [ hd; tl ]))

  and make_record ~f loc mode tyvars m =
    match mode with
    | Destructure_expand ->
        let f _ pat ty =
          match (pat, ty) with
          | Some pat, None -> Some (make ~f mode (Ty.unknown ()) pat)
          | Some pat, Some ty -> Some (make ~f mode ty pat)
          | None, Some _ -> Some TAny
          | None, None -> None
        in
        Map.String.merge f m tyvars
    | Construct_var | Construct_literal ->
        let f k pat ty =
          match (pat, ty) with
          | Some pat, Some ty -> Some (make ~f mode ty pat)
          | None, Some ty -> Error.missing_field loc k ty (* for components *)
          | _ -> None
        in
        Map.String.merge f m tyvars

  let to_list l =
    let rec aux acc = function
      | TTuple [ hd; TConstruct (_, Some tl) ] -> aux (hd :: acc) tl
      | TTuple [ hd; TConstruct (_, None) ] -> (List.rev (hd :: acc), None)
      | TTuple [ hd; tl ] -> (List.rev (hd :: acc), Some tl)
      | l -> (List.rev acc, Some l)
    in
    aux [] l

  let pp_constant ppf c =
    match c with
    | `Int i -> F.pp_print_int ppf i
    | `String s -> F.fprintf ppf "%S" s
    | `Float f -> F.pp_print_float ppf f

  let pp_constant_enum ppf x c =
    match (x, c) with
    | { V.extra = `Extra_bool; _ }, `Int 0 -> F.pp_print_string ppf "false"
    | { V.extra = `Extra_bool; _ }, `Int _ -> F.pp_print_string ppf "true"
    | _, c -> F.fprintf ppf "%@%a" pp_constant c

  let pp_constant_union ppf x c =
    match (x, c) with
    | { V.extra = `Extra_bool; _ }, `Int 0 -> F.pp_print_string ppf "false"
    | { V.extra = `Extra_bool; _ }, `Int _ -> F.pp_print_string ppf "true"
    | _, c -> pp_constant ppf c

  let rec pp ppf = function
    | TConst (c, None) -> pp_constant ppf c
    | TConst (c, Some x) -> pp_constant_enum ppf x c
    | TTuple t ->
        F.fprintf ppf "(@[%a@])" (F.pp_print_list ~pp_sep:Pp.sep_comma pp) t
    | TRecord (Some (k, tag, var), r, _) ->
        F.fprintf ppf "{@[%@%a: %t, %a@]}" Pp.field k
          (fun ppf -> pp_constant_union ppf var tag)
          pp_bindings r
    | TRecord (None, r, _) -> F.fprintf ppf "{@[%a@]}" pp_bindings r
    | TDict (m, _) -> F.fprintf ppf "<%a>" pp_bindings m
    | TVar v -> F.pp_print_string ppf v
    | TConstruct (TNullable, None) -> F.pp_print_string ppf "null"
    | TConstruct (TNullable, Some x) -> F.fprintf ppf "!%a" pp x
    | TConstruct (TList, None) -> F.pp_print_string ppf "[]"
    | TConstruct (TList, Some l) ->
        let l, rest = to_list l in
        F.fprintf ppf "[@[%a%a@]]"
          (F.pp_print_list ~pp_sep:Pp.sep_comma pp)
          l
          (F.pp_print_option pp_rest)
          rest
    | TAny -> F.pp_print_string ppf "_"

  and pp_key_values ppf (k, v) =
    match v with
    | TVar v when v = k -> Pp.field ppf k
    | v -> F.fprintf ppf "%a: %a" Pp.field k pp v

  and pp_bindings ppf m =
    F.pp_print_seq ~pp_sep:Pp.sep_comma pp_key_values ppf (Map.String.to_seq m)

  and pp_rest ppf t = F.fprintf ppf ",@ ...%a" pp t
end

type echo =
  | Ech_var of string * Ast.escape
  | Ech_component of string
  | Ech_string of string

type node =
  | TText of string * Ast.trim * Ast.trim
  | TEcho of echo list * echo
  | TMatch of Loc.t * Pattern.t Nonempty.t * Ty.t Nonempty.t * case Nonempty.t
  | TMap_list of Loc.t * Pattern.t * Ty.t Nonempty.t * case Nonempty.t
  | TMap_dict of Loc.t * Pattern.t * Ty.t Nonempty.t * case Nonempty.t
  | TComponent of string * Pattern.t Map.String.t * child Map.String.t

and case = { pats : (Loc.t * Pattern.t Nonempty.t) Nonempty.t; nodes : nodes }
and child = TChild_name of string | TChild_block of nodes
and nodes = node list

type t = {
  nodes : nodes;
  prop_types : Ty.t Map.String.t;
  child_types : Ty.Child.t Map.String.t;
}

let unify_child loc a b =
  if Ty.Child.equal a b then () else Error.child_type_mismatch loc a b

module Context = struct
  type root = [ `Root | `Component ]

  type t = {
    global : Ty.t Map.String.t ref;
    scope : Ty.t Map.String.t;
    children : Ty.Child.t Map.String.t ref;
    root : root;
  }

  let make root =
    {
      global = ref Map.String.empty;
      scope = Map.String.empty;
      children = ref Map.String.empty;
      root;
    }

  let get k scope global =
    match Map.String.find_opt k scope with
    | None -> Map.String.find_opt k !global
    | Some _ as x -> x

  let update { scope; global; _ } loc k v =
    match get k scope global with
    | None -> global := Map.String.add k v !global
    | Some v' -> unify loc Construct_var v v'

  let update_child loc { root; children; _ } (k, v) =
    match root with
    | `Root -> Error.child_in_root loc
    | `Component ->
        let f = function
          | None -> Some v
          | Some v' as r ->
              unify_child loc v v';
              r
        in
        children := Map.String.update k f !children

  let add_scope bindings ctx =
    let bindings' = Queue.create () in
    Queue.iter
      (fun q ->
        Queue.add
          (Queue.fold
             (fun newscope (loc, k, v) ->
               if Map.String.mem k newscope then Error.name_bound_too_many loc k
               else Map.String.add k (loc, v) newscope)
             Map.String.empty q)
          bindings')
      bindings;
    match Queue.take_opt bindings' with
    | None -> ctx
    | Some hd ->
        let newscope =
          Queue.fold
            (fun acc m ->
              Map.String.merge
                (fun k a b ->
                  match (a, b) with
                  | (Some (loc, a) as a'), Some (_, b) ->
                      unify loc Construct_literal a b;
                      a'
                  | Some (loc, _), None | None, Some (loc, _) ->
                      Error.var_missing loc k
                  | None, None -> None)
                acc m)
            hd bindings'
        in
        let scope =
          Map.String.merge
            (fun _ a b ->
              match (a, b) with
              | None, None -> None
              | Some x, None | _, Some (_, x) -> Some x)
            ctx.scope newscope
        in
        { ctx with scope }
end

(** @raises [Invalid_argument] if the list sizes are mismatched. *)
let unify_match_cases pats tys ctx =
  Nonempty.map2
    (Pattern.make ~f:(Context.update ctx) Construct_literal)
    tys pats

let rec make_echoes ctx = function
  | [] -> []
  | Ast.Ech_var (loc, b, esc) :: tl ->
      Context.update ctx loc b (Ty.nullable (Ty.echo ()));
      Ech_var (b, esc) :: make_echoes ctx tl
  | Ech_component (loc, c) :: tl ->
      Context.update_child loc ctx (Ty.Child.nullable c);
      Ech_component c :: make_echoes ctx tl
  | Ech_string (loc, _) :: _ -> Error.echo_nullable_literal loc

let make_default_echo ctx = function
  | Ast.Ech_var (loc, b, esc) ->
      Context.update ctx loc b (Ty.echo ());
      Ech_var (b, esc)
  | Ech_component (loc, c) ->
      Context.update_child loc ctx (Ty.Child.child c);
      Ech_component c
  | Ech_string (_, s) -> Ech_string s

type ('a, 'b) source =
  | Src of string * 'a
  | Fun of string * Ty.t Map.String.t * Ty.Child.t Map.String.t * 'b

let get_types = function
  | Src (_, { prop_types; child_types; _ }) -> (prop_types, child_types)
  | Fun (_, props, children, _) -> (props, children)

let add_default_wildcard cases =
  let f = function
    | loc, Nonempty.[ h ] ->
        (loc, Nonempty.[ h; Ast.Pattern.Var (Loc.dummy, "_") ])
    | pat -> pat
  in
  Nonempty.map
    (fun case -> { case with Ast.pats = Nonempty.map f case.Ast.pats })
    cases

let unify_map ~ty ~key loc (tys, cases) pat ctx =
  let hd_ty =
    match tys with
    | Nonempty.[ hd; tl ] ->
        unify loc Construct_literal (key ()) tl;
        ty hd
    | _ -> Error.map_pat_num_mismatch loc
  in
  let p = Pattern.make ~f:(Context.update ctx) Construct_literal hd_ty pat in
  (p, tys, cases)

let rec make_cases ctx g cases =
  let tys =
    let _, pats = (Nonempty.hd cases).Ast.pats |> Nonempty.hd in
    Nonempty.map Pattern.unknown pats
  in
  (* Type-check all of the cases BEFORE running [make_nodes]. *)
  let cases =
    Nonempty.map
      (fun Ast.{ pats; nodes } ->
        let bindings_all = Queue.create () in
        let pats =
          Nonempty.map
            (fun (loc, pat) ->
              let bindings = Queue.create () in
              Queue.add bindings bindings_all;
              let f l k v = Queue.add (l, k, v) bindings in
              try
                (loc, Nonempty.map2 (Pattern.make ~f Destructure_expand) tys pat)
              with Invalid_argument _ -> Error.pat_num_mismatch loc)
            pats
        in
        let ctx = Context.add_scope bindings_all ctx in
        (pats, nodes, ctx))
      cases
    |> Nonempty.map (fun (pats, nodes, ctx) ->
           { pats; nodes = make_nodes ctx g nodes })
  in
  (tys, cases)

and make_nodes ctx g nodes =
  let f = function
    | Ast.Text (s, l, r) -> TText (s, l, r)
    | Echo (nullables, default) ->
        TEcho (make_echoes ctx nullables, make_default_echo ctx default)
    | Component (loc, comp, comp', props, children) ->
        if comp <> comp' then Error.component_name_mismatch loc comp comp';
        let prop_types, prop_types_child = get_types (Dagmap.get comp g) in
        (* The original should not mutate*)
        let prop_types = Ty.internal_copy_record prop_types in
        let missing_to_nullable _ ty prop =
          match (prop, ty) with
          | None, Some { contents = Ty.Nullable _ } ->
              Some (Ast.Pattern.Nullable (Loc.dummy, None))
          | prop, _ -> prop
        in
        let props =
          Ast.Dict.to_map props
          |> Map.String.merge missing_to_nullable prop_types
          |> Pattern.make_record ~f:(Context.update ctx) loc Construct_literal
               prop_types
        in
        let f k c ty =
          match (c, ty) with
          | None, Some ty ->
              if Ty.Child.is_nullable ty then None
              else Error.missing_child loc k
          | Some (Ast.Child_name (loc, c)), Some ty ->
              Context.update_child loc ctx (c, ty);
              Some (TChild_name c)
          | Some (Child_block nodes), Some _ ->
              Some (TChild_block (make_nodes ctx g nodes))
          | Some _, None -> Error.extra_child loc ~comp ~child:k
          | None, None -> None
        in
        let children =
          Map.String.merge f (Ast.Dict.to_map children) prop_types_child
        in
        TComponent (comp, props, children)
    | Match (loc, pats, cases) ->
        let tys, cases = make_cases ctx g cases in
        let patterns =
          try unify_match_cases pats tys ctx
          with Invalid_argument _ -> Error.pat_num_mismatch loc
        in
        TMatch (loc, patterns, tys, cases)
    | Map_list (loc, pattern, cases) ->
        let cases = add_default_wildcard cases |> make_cases ctx g in
        let pattern, ty, cases =
          unify_map ~ty:Ty.list ~key:Ty.int loc cases pattern ctx
        in
        TMap_list (loc, pattern, ty, cases)
    | Map_dict (loc, pattern, cases) ->
        let cases = add_default_wildcard cases |> make_cases ctx g in
        let pattern, ty, cases =
          unify_map ~ty:Ty.dict ~key:Ty.string loc cases pattern ctx
        in
        TMap_dict (loc, pattern, ty, cases)
  in
  List.map f nodes

let make root g ast =
  let ctx = Context.make root in
  let nodes = make_nodes ctx g ast in
  { nodes; prop_types = !(ctx.global); child_types = !(ctx.children) }

let make_src g = function
  | Src (name, ast) -> Src (name, make `Component g ast)
  | Fun (name, p, c, f) -> Fun (name, p, c, f)

let make_components m = Dagmap.make ~f:make_src m |> Dagmap.link_all

let make ~root components ast =
  make `Root (Dagmap.prelinked root components) ast
