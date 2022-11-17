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

(** When we unify a type during destructuring, we expand any structural types
    (records, enums).

    When we unify a type during constructing literal values, record types are
    narrowed (we use a subset of both) and enum behavior depends on its row
    property.

    When we unify a type during constructing with a variable, then we expand
    records and open enums in the variable's type. *)

let rec open_rows_bool_union_aux = function
  | None -> Some (ref Map.String.empty)
  | Some ty as x ->
      Map.String.iter (fun _ v -> open_rows v) !ty;
      x

and open_rows ty =
  match !ty with
  | Ty.Enum ty -> (
      match ty.extra with
      | Not_bool -> ty.row <- `Open
      | Bool -> ty.cases <- Ty.Enum.false_and_true_cases)
  | Union (_, ty) -> (
      match ty with
      | { cases = VInt cases; extra = Bool; _ } ->
          ty.cases <-
            VInt
              (Map.Int.update 0 open_rows_bool_union_aux cases
              |> Map.Int.update 1 open_rows_bool_union_aux)
      | { cases = VInt cases; _ } ->
          Map.Int.iter
            (fun _ v -> Map.String.iter (fun _ v -> open_rows v) !v)
            cases;
          ty.row <- `Open
      | { cases = VString cases; _ } ->
          Map.String.iter
            (fun _ v -> Map.String.iter (fun _ v -> open_rows v) !v)
            cases;
          ty.row <- `Open)
  | Tuple a -> List.iter open_rows a
  | Record ty -> Map.String.iter (fun _ v -> open_rows v) !ty
  | Nullable ty | List ty | Dict (ty, _) -> open_rows ty
  | Unknown row -> row := `Open
  | Int | Float | String -> ()

type mode =
  | Destructure_expand (* Record a and b both take on each other's fields. *)
  | Construct_literal (* Record a is narrowed to a subset of a and b. *)
  | Construct_var (* Record a expands into b only. *)

let unify_enum_cases _ a b =
  match (a, b) with
  | V.VString a, V.VString b -> V.VString (Set.String.union a b)
  | VInt a, VInt b -> VInt (Set.Int.union a b)
  | _ -> raise_notrace Clash

let subset_enum_cases _ a b =
  let success =
    match (a, b) with
    | V.VString a, V.VString b -> Set.String.subset b a
    | VInt a, VInt b -> Set.Int.subset b a
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
  | Ty.Int, Ty.Int | Float, Float | String, String -> ()
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
      a :=
        Map.String.merge
          (fun _ a b ->
            match (a, b) with
            | (Some a as a'), Some b ->
                unify mode a b;
                a'
            | None, Some b ->
                open_rows b;
                Some b
            | x, None -> x)
          !a !b
  | Construct_var ->
      b :=
        Map.String.union
          (fun _ a b ->
            unify mode a b;
            Some b)
          !a !b
  | Construct_literal ->
      a :=
        Map.String.merge
          (fun _ a b ->
            match (a, b) with
            | (Some a as x), Some b ->
                unify mode a b;
                x
            | Some _, None -> raise_notrace Clash
            | None, _ -> None)
          !a !b

and unify_union_cases mode a b =
  let f _ a b =
    unify_record mode a b;
    Some a
  in
  match (a, b) with
  | V.VString a, V.VString b -> V.VString (Map.String.union f a b)
  | VInt a, VInt b -> VInt (Map.Int.union f a b)
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
  | V.VString a, V.VString b -> Map.String.merge f a b |> ignore
  | VInt a, VInt b -> Map.Int.merge f a b |> ignore
  | _ -> raise_notrace Clash

let unify loc mode a b =
  try unify mode a b with Clash -> Error.type_mismatch loc a b

let map_string_subset f ~interface ~impl =
  Map.String.for_all
    (fun k impl ->
      match Map.String.find_opt k interface with
      | Some interface -> f ~interface ~impl
      | None -> false)
    impl

let map_int_subset f ~interface ~impl =
  Map.Int.for_all
    (fun k impl ->
      match Map.Int.find_opt k interface with
      | Some interface -> f ~interface ~impl
      | None -> false)
    impl

let rec check_interface ~interface ~impl =
  match (!interface, !impl) with
  | Ty.Int, Ty.Int | Float, Float | String, String | _, Unknown _ -> true
  | Nullable a, Nullable b | List a, List b | Dict (a, _), Dict (b, _) ->
      check_interface ~interface:a ~impl:b
  | Tuple a, Tuple b ->
      List.equal (fun a b -> check_interface ~interface:a ~impl:b) a b
  | Record a, Record b -> check_interface_record ~interface:a ~impl:b
  | Enum a, Enum b when Ty.Variant.equal_extra a.extra b.extra -> (
      match (a.row, b.row) with
      | `Closed, `Closed -> (
          match (a.cases, b.cases) with
          | VInt a, VInt b -> Set.Int.equal a b
          | VString a, VString b -> Set.String.equal a b
          | _ -> false)
      | `Open, `Open -> (
          match (a.cases, b.cases) with
          | VInt interface, VInt impl -> Set.Int.subset impl interface
          | VString interface, VString impl -> Set.String.subset impl interface
          | _ -> false)
      | _ -> false)
  | Union (ka, a), Union (kb, b)
    when String.equal ka kb && Ty.Variant.equal_extra a.extra b.extra -> (
      match (a.row, b.row) with
      | `Closed, `Closed -> (
          match (a.cases, b.cases) with
          | VInt a, VInt b ->
              Map.Int.equal
                (fun a b -> check_interface_record ~interface:a ~impl:b)
                a b
          | VString a, VString b ->
              Map.String.equal
                (fun a b -> check_interface_record ~interface:a ~impl:b)
                a b
          | _ -> false)
      | `Open, `Open -> (
          match (a.cases, b.cases) with
          | VInt interface, VInt impl ->
              map_int_subset check_interface_record ~interface ~impl
          | VString interface, VString impl ->
              map_string_subset check_interface_record ~interface ~impl
          | _ -> false)
      | _ -> false)
  | _ -> false

and check_interface_record ~interface ~impl =
  map_string_subset check_interface ~interface:!interface ~impl:!impl

(** Check for equality, but allow the interface to add additional
    fields to records and additional entries to open enums and unions. *)
let check_interface loc ~interface ~impl =
  Map.String.iter
    (fun k impl ->
      match Map.String.find_opt k interface with
      | Some (loc, interface) ->
          if not (check_interface ~interface ~impl) then
            Error.interface_type_mismatch loc k interface impl
      | None -> Error.interface_missing_prop loc k impl)
    impl

type echo = Ech_var of Ast.echo_format * string | Ech_string of string
type construct = TList | TNullable

type pat =
  | TConst of Data.Const.t * Ty.Enum.t option
  | TConstruct of construct * pat option
  | TTuple of pat list
  | TRecord of
      (string * Data.Const.t * Ty.t Ty.Union.t) option
      * pat Map.String.t
      * Ty.t Map.String.t ref
  | TDict of pat Map.String.t * Set.String.t ref
  | TVar of string
  | TBlock of Loc.t * nodes
  | TAny

and node =
  | TText of string * Ast.trim * Ast.trim
  | TEcho of (Ast.echo_format * string) list * echo * Ast.escape
  | TMatch of Loc.t * pat Nonempty.t * Ty.t Nonempty.t * case Nonempty.t
  | TMap_list of Loc.t * pat * Ty.t Nonempty.t * case Nonempty.t
  | TMap_dict of Loc.t * pat * Ty.t Nonempty.t * case Nonempty.t
  | TComponent of string * pat Map.String.t

and case = { pats : (Loc.t * pat Nonempty.t) Nonempty.t; nodes : nodes }
and nodes = node list

type t = { nodes : nodes; types : Ty.t Map.String.t }

type ('a, 'b) source =
  | Src of string * 'a
  | Fun of string * Ty.t Map.String.t * 'b

let get_types = function
  | Src (_, { types; _ }) -> types
  | Fun (_, props, _) -> props

module Context = struct
  type typed_tree = t
  type used = Unused | Used
  type binding = { loc : Loc.t; name : string; mutable used : used; ty : Ty.t }

  type 'a t = {
    global : Ty.t Map.String.t ref;
    scope : binding Map.String.t;
    all_bindings : binding Queue.t;
        (** This is for checking for unused variables. A queue preserves the
            order for friendlier debugging. *)
    interface : (Loc.t * Ty.t) Map.String.t ref option ref;
    interface_loc : Loc.t ref;
        (** When there's more than one interface, this is for the last one. *)
    graph : ((Ast.t, 'a) source, (typed_tree, 'a) source) Dagmap.t;
  }
  (** We have to wrap each mutable value in a [ref] instead of using mutable
      fields because a new context record is created for each scope. *)

  let make graph =
    {
      global = ref Map.String.empty;
      scope = Map.String.empty;
      all_bindings = Queue.create ();
      interface = ref None;
      interface_loc = ref Loc.dummy;
      graph;
    }

  let get k scope global =
    match Map.String.find_opt k scope with
    | None -> Map.String.find_opt k !global
    | Some x ->
        x.used <- Used;
        Some x.ty

  let update { scope; global; _ } loc k v =
    match get k scope global with
    | None -> global := Map.String.add k v !global
    | Some v' -> unify loc Construct_var v v'

  let add_scope var_matrix ctx =
    let var_map_rows =
      Queue.fold
        (fun var_map_rows var_row ->
          Queue.fold
            (fun var_map (loc, k, v) ->
              if Map.String.mem k var_map then Error.name_bound_too_many loc k
              else
                let binding = { loc; used = Unused; name = k; ty = v } in
                Queue.add binding ctx.all_bindings;
                Map.String.add k binding var_map)
            Map.String.empty var_row
          :: var_map_rows)
        [] var_matrix
      |> List.rev
    in
    match var_map_rows with
    | [] -> ctx
    | hd :: vars ->
        let scope =
          List.fold_left
            (fun acc m ->
              Map.String.merge
                (fun k a b ->
                  match (a, b) with
                  | (Some { loc; ty = a; _ } as a'), Some { ty = b; _ } ->
                      unify loc Construct_literal a b;
                      a'
                  | Some { loc; _ }, None | None, Some { loc; _ } ->
                      Error.var_missing loc k
                  | None, None -> None)
                acc m)
            hd vars
          |> Map.String.union (fun _k _oldvar newvar -> Some newvar) ctx.scope
        in
        { ctx with scope }
end

module Interface = struct
  let there_can_be_only_one loc k v = function
    | None -> Some v
    | Some _ -> Error.interface_duplicate loc k

  let update loc k v { Context.interface; _ } =
    match !interface with
    | None -> interface := Some (ref (Map.String.singleton k (loc, v)))
    | Some interface ->
        interface :=
          Map.String.update k (there_can_be_only_one loc k (loc, v)) !interface

  let named loc = function
    | "int" -> Ty.int ()
    | "string" -> Ty.string ()
    | "float" -> Ty.float ()
    | "_" -> Ty.unknown ()
    | s -> Error.interface_bad_name loc s

  let error_tag expected = function
    | Ast.Record.Tag_int (loc, _) ->
        Error.type_mismatch loc expected (Typescheme.int ())
    | Tag_bool (loc, 0) ->
        Error.type_mismatch loc expected (Typescheme.false_only ())
    | Tag_bool (loc, _) ->
        Error.type_mismatch loc expected (Typescheme.true_only ())
    | Tag_string (loc, _) ->
        Error.type_mismatch loc expected (Typescheme.string ())

  let tag_int = function
    | Ast.Record.Tag_int (_, i) -> i
    | t -> error_tag (Ty.int ()) t

  let tag_string = function
    | Ast.Record.Tag_string (_, s) -> s
    | t -> error_tag (Ty.string ()) t

  let tag_bool = function
    | Ast.Record.Tag_bool (_, i) -> i
    | t -> error_tag (Ty.boolean ()) t

  let rec make_ty = function
    | Ast.Interface.Named (loc, s) -> named loc s
    | Nullable t -> Ty.nullable (make_ty t)
    | List t -> Ty.list (make_ty t)
    | Dict t -> Ty.dict (make_ty t)
    | Tuple l -> Ty.tuple (List.map make_ty l)
    | Enum_int (l, r) -> Ty.enum_int r (Nonempty.to_list l)
    | Enum_bool l -> Ty.internal_bool (Nonempty.to_list l)
    | Enum_string (l, r) -> Ty.enum_string r (Nonempty.to_list l)
    | Record ([ (_, Untagged r) ], `Closed) ->
        Ast.Dict.to_map r |> Map.String.map make_ty |> ref |> Ty.internal_record
    | Record ((loc, Untagged _) :: _ :: _, _)
    | Record ((loc, Untagged _) :: _, `Open) ->
        Error.interface_untagged_union loc
    | Record ((_, Tagged (tagk, tagv, m)) :: tl, row) -> (
        let aux update parse_tag m =
          List.fold_left
            (fun acc -> function
              | loc, Ast.Record.Tagged (tagk', tagv, m) ->
                  if tagk <> tagk' then
                    Error.interface_unmatched_tags loc tagk tagk';
                  let k = parse_tag tagv in
                  update k
                    (function
                      | None ->
                          Some
                            (Ast.Dict.to_map m |> Map.String.map make_ty |> ref)
                      | Some _ ->
                          Error.interface_duplicate_tag loc Ast.Record.pp_tag
                            tagv)
                    acc
              | loc, Untagged _ -> Error.interface_untagged_union loc)
            m tl
        in
        let m = Ast.Dict.to_map m |> Map.String.map make_ty in
        match tagv with
        | Tag_int (_, i) ->
            let m = Map.Int.(singleton i (ref m) |> aux update tag_int) in
            ref (Ty.Union (tagk, { cases = VInt m; row; extra = Not_bool }))
        | Tag_bool (_, i) ->
            let m = Map.Int.(singleton i (ref m) |> aux update tag_bool) in
            ref (Ty.Union (tagk, { cases = VInt m; row; extra = Bool }))
        | Tag_string (_, s) ->
            let m = Map.String.(singleton s (ref m) |> aux update tag_string) in
            ref (Ty.Union (tagk, { cases = VString m; row; extra = Not_bool })))

  let make loc ctx l =
    ctx.Context.interface_loc := loc;
    List.iter
      (fun { Ast.Interface.loc; name; ty } -> update loc name (make_ty ty) ctx)
      l
end

let make_echo_type = function
  | Ast.Fmt_string -> Ty.string ()
  | Fmt_int _ -> Ty.int ()
  | Fmt_float _ | Fmt_float_g _ | Fmt_float_e _ -> Ty.float ()
  | Fmt_bool -> Ty.boolean ()

let[@tail_mod_cons] rec make_echoes ctx = function
  | [] -> []
  | Ast.Ech_var (loc, fmt, b) :: tl ->
      let ty = make_echo_type fmt in
      Context.update ctx loc b (Ty.nullable ty);
      (fmt, b) :: make_echoes ctx tl
  | Ech_string (loc, _) :: _ -> Error.echo_nullable_literal loc

let make_default_echo ctx = function
  | Ast.Ech_var (loc, fmt, b) ->
      let ty = make_echo_type fmt in
      Context.update ctx loc b ty;
      Ech_var (fmt, b)
  | Ech_string (_, s) -> Ech_string s

let add_default_wildcard cases =
  Nonempty.map
    (fun case ->
      {
        case with
        Ast.pats =
          Nonempty.map
            (function
              | loc, Nonempty.[ h ] ->
                  (loc, Nonempty.[ h; Ast.Var (Loc.dummy, "_") ])
              | pat -> pat)
            case.Ast.pats;
      })
    cases

let unknown _ = Ty.unknown ()

let rec make_pat ~f ctx mode ty = function
  | Ast.Int (loc, i) ->
      unify loc mode ty (Ty.int ());
      TConst (Int i, None)
  | String (loc, s) ->
      unify loc mode ty (Ty.string ());
      TConst (String s, None)
  | Block (loc, nodes) ->
      unify loc mode ty (Ty.string ());
      TBlock (loc, make_nodes ctx nodes)
  | Float (loc, f) ->
      unify loc mode ty (Ty.float ());
      TConst (Float f, None)
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
      TConst (Int b, Some enum)
  | Enum_string (loc, s) ->
      let new_enum =
        match mode with
        | Destructure_expand -> Ty.Enum.string_singleton s `Closed
        | Construct_var | Construct_literal -> Ty.Enum.string_singleton s `Open
      in
      let new_ty = ref (Ty.Enum new_enum) in
      let enum = match !ty with Enum e -> e | _ -> new_enum in
      unify loc mode ty new_ty;
      TConst (String s, Some enum)
  | Enum_int (loc, i) ->
      let new_enum =
        match mode with
        | Destructure_expand -> Ty.Enum.int_singleton i `Closed
        | Construct_var | Construct_literal -> Ty.Enum.int_singleton i `Open
      in
      let new_ty = ref (Ty.Enum new_enum) in
      let enum = match !ty with Enum e -> e | _ -> new_enum in
      unify loc mode ty new_ty;
      TConst (Int i, Some enum)
  | Nullable (loc, pat) ->
      let tyvar = match !ty with Nullable ty -> ty | _ -> Ty.unknown () in
      let pat =
        match pat with
        | None -> None
        | Some pat -> Some (TTuple [ make_pat ~f ctx mode tyvar pat ])
      in
      unify loc mode ty (Ty.nullable tyvar);
      TConstruct (TNullable, pat)
  | List (loc, l, tl) ->
      let tyvar = match !ty with List ty -> ty | _ -> Ty.unknown () in
      unify loc mode ty (Ty.list tyvar);
      let tl =
        match tl with
        | None -> TConstruct (TList, None)
        | Some tl -> make_pat ~f ctx mode ty tl
      in
      make_list ~tl ~f ctx mode tyvar l
  | Tuple (loc, l) ->
      let new_tyvars = List.map unknown l in
      let tyvars = match !ty with Tuple tys -> tys | _ -> new_tyvars in
      unify loc mode ty (Ty.tuple tyvars);
      TTuple (List.map2 (make_pat ~f ctx mode) tyvars l)
  | Record (loc, Untagged m) ->
      let m = Ast.Dict.to_map m in
      let new_tyvars = Map.String.map unknown m |> ref in
      let tyvars = match !ty with Record tys -> tys | _ -> new_tyvars in
      unify loc mode ty (Ty.internal_record new_tyvars);
      let r = make_record ~f loc ctx mode !tyvars m in
      TRecord (None, r, tyvars)
  | Record (loc, Tagged (k, v, m)) ->
      let m = Ast.Dict.to_map m in
      let new_tyvars = Map.String.map unknown m |> ref in
      let row =
        match mode with
        | Destructure_expand -> `Closed
        | Construct_literal | Construct_var -> `Open
      in
      (* Use a polyvar instead of Data.Const.t to disallow Float values. *)
      let tag, tag_extra =
        match v with
        | Tag_int (_, i) -> (`Int i, Ty.Variant.Not_bool)
        | Tag_bool (_, i) -> (`Int i, Bool)
        | Tag_string (_, s) -> (`String s, Not_bool)
      in
      let tyvars =
        match !ty with
        | Union (_, enum) -> (
            let tyvars =
              match (tag, enum) with
              | `Int i, { cases = VInt cases; _ } -> Map.Int.find_opt i cases
              | `String s, { cases = VString cases; _ } ->
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
      let r = make_record ~f loc ctx mode !tyvars m in
      let tag =
        match tag with `Int i -> Data.Const.Int i | `String s -> String s
      in
      TRecord (Some (k, tag, new_enum), r, tyvars)
  | Dict (loc, m) ->
      let new_kys = ref Set.String.empty in
      let tyvar, kys =
        match !ty with
        | Dict (ty, ks) -> (ty, ks)
        | _ -> (Ty.unknown (), new_kys)
      in
      unify loc mode ty (Ty.internal_dict_keys tyvar new_kys);
      let d =
        Ast.Dict.to_map m |> Map.String.map (make_pat ~f ctx mode tyvar)
      in
      TDict (d, kys)
  | Var (loc, "_") -> (
      match mode with
      | Destructure_expand ->
          open_rows ty;
          TAny
      | Construct_literal | Construct_var -> Error.underscore_in_construct loc)
  | Var (loc, b) ->
      (match mode with
      | Destructure_expand -> open_rows ty
      | Construct_literal | Construct_var -> ());
      f loc b ty;
      TVar b

and[@tail_mod_cons] make_list ~f ~tl ctx mode ty = function
  | [] -> tl
  | p :: l ->
      let hd = make_pat ~f ctx mode ty p in
      TConstruct (TList, Some (TTuple [ hd; make_list ~f ~tl ctx mode ty l ]))

and make_record ~f loc ctx mode tyvars m =
  match mode with
  | Destructure_expand ->
      Map.String.merge
        (fun _ pat ty ->
          match (pat, ty) with
          | Some pat, None -> Some (make_pat ~f ctx mode (Ty.unknown ()) pat)
          | Some pat, Some ty -> Some (make_pat ~f ctx mode ty pat)
          | None, Some _ -> Some TAny
          | None, None -> None)
        m tyvars
  | Construct_var | Construct_literal ->
      Map.String.merge
        (fun k pat ty ->
          match (pat, ty) with
          | Some pat, Some ty -> Some (make_pat ~f ctx mode ty pat)
          | None, Some ty -> Error.missing_field loc k ty (* for components *)
          | _ -> None)
        m tyvars

(** @raises [Invalid_argument] if the list sizes are mismatched. *)
and unify_match_cases pats tys ctx =
  Nonempty.map2
    (make_pat ~f:(Context.update ctx) ctx Construct_literal)
    tys pats

and unify_map ~ty ~key loc (tys, cases) pat ctx =
  let hd_ty =
    match tys with
    | Nonempty.[ hd; tl ] ->
        unify loc Construct_literal (key ()) tl;
        ty hd
    | _ -> Error.map_pat_num_mismatch loc
  in
  let p = make_pat ~f:(Context.update ctx) ctx Construct_literal hd_ty pat in
  (p, tys, cases)

and make_cases ctx cases =
  let tys =
    let _, pats = (Nonempty.hd cases).Ast.pats |> Nonempty.hd in
    Nonempty.map unknown pats
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
                (loc, Nonempty.map2 (make_pat ~f ctx Destructure_expand) tys pat)
              with Invalid_argument _ -> Error.pat_num_mismatch loc)
            pats
        in
        let ctx = Context.add_scope bindings_all ctx in
        (pats, nodes, ctx))
      cases
    |> Nonempty.map (fun (pats, nodes, ctx) ->
           { pats; nodes = make_nodes ctx nodes })
  in
  (tys, cases)

and make_nodes ctx nodes =
  List.filter_map
    (function
      | Ast.Text (s, l, r) -> Some (TText (s, l, r))
      | Echo (nullables, default, esc) ->
          Some
            (TEcho
               (make_echoes ctx nullables, make_default_echo ctx default, esc))
      | Component (loc, comp, comp', props) ->
          if comp <> comp' then Error.component_name_mismatch loc comp comp';
          let types = get_types (Dagmap.get comp ctx.graph) in
          (* The original should not mutate*)
          let types = Ty.internal_copy_record types in
          let missing_to_nullable _ ty prop =
            match (prop, ty) with
            | None, Some { contents = Ty.Nullable _ } ->
                Some (Ast.Nullable (Loc.dummy, None))
            | prop, _ -> prop
          in
          let props =
            Ast.Dict.to_map props
            |> Map.String.merge missing_to_nullable types
            |> make_record ~f:(Context.update ctx) loc ctx Construct_literal
                 types
          in
          Some (TComponent (comp, props))
      | Match (loc, pats, cases) ->
          let tys, cases = make_cases ctx cases in
          let patterns =
            try unify_match_cases pats tys ctx
            with Invalid_argument _ -> Error.pat_num_mismatch loc
          in
          Some (TMatch (loc, patterns, tys, cases))
      | Map_list (loc, pattern, cases) ->
          let cases = add_default_wildcard cases |> make_cases ctx in
          let pattern, ty, cases =
            unify_map ~ty:Ty.list ~key:Ty.int loc cases pattern ctx
          in
          Some (TMap_list (loc, pattern, ty, cases))
      | Map_dict (loc, pattern, cases) ->
          let cases = add_default_wildcard cases |> make_cases ctx in
          let pattern, ty, cases =
            unify_map ~ty:Ty.dict ~key:Ty.string loc cases pattern ctx
          in
          Some (TMap_dict (loc, pattern, ty, cases))
      | Interface (loc, i) ->
          Interface.make loc ctx i;
          None)
    nodes

let make g ast =
  let ctx = Context.make g in
  let nodes = make_nodes ctx ast in
  Queue.iter
    (function
      | Context.{ used = Used; _ } -> ()
      | { used = Unused; loc; name; _ } -> (
          match name.[0] with '_' -> () | _ -> Error.var_unused loc name))
    ctx.all_bindings;
  match !(ctx.interface) with
  | None -> { nodes; types = !(ctx.global) }
  | Some interface ->
      check_interface !(ctx.interface_loc) ~interface:!interface
        ~impl:!(ctx.global);
      { nodes; types = Map.String.map snd !interface }

let make_src g = function
  | Src (name, ast) -> Src (name, make g ast)
  | Fun (name, p, f) -> Fun (name, p, f)

let make_components m = Dagmap.make ~f:make_src m |> Dagmap.link_all
let make ~root components ast = make (Dagmap.prelinked root components) ast

let pp_pat =
  let to_list l =
    let rec aux acc = function
      | TTuple [ hd; TConstruct (_, Some tl) ] -> aux (hd :: acc) tl
      | TTuple [ hd; TConstruct (_, None) ] -> (List.rev (hd :: acc), None)
      | TTuple [ hd; tl ] -> (List.rev (hd :: acc), Some tl)
      | l -> (List.rev acc, Some l)
    in
    aux [] l
  in

  let pp_constant ppf c =
    match c with
    | Data.Const.Int i -> F.pp_print_int ppf i
    | String s -> F.fprintf ppf "%S" s
    | Float f -> F.pp_print_float ppf f
  in

  let pp_constant_enum ppf x c =
    match (x, c) with
    | { V.extra = Bool; _ }, Data.Const.Int 0 -> F.pp_print_string ppf "false"
    | { V.extra = Bool; _ }, Int _ -> F.pp_print_string ppf "true"
    | _, c -> F.fprintf ppf "%@%a" pp_constant c
  in

  let pp_constant_union ppf x c =
    match (x, c) with
    | { V.extra = Bool; _ }, Data.Const.Int 0 -> F.pp_print_string ppf "false"
    | { V.extra = Bool; _ }, Int _ -> F.pp_print_string ppf "true"
    | _, c -> pp_constant ppf c
  in

  let rec pp_pat ppf = function
    | TConst (c, None) -> pp_constant ppf c
    | TConst (c, Some x) -> pp_constant_enum ppf x c
    | TTuple t ->
        F.fprintf ppf "(@[%a@])" (F.pp_print_list ~pp_sep:Pp.sep_comma pp_pat) t
    | TRecord (Some (k, tag, var), r, _) ->
        F.fprintf ppf "{@[%@%a: %t, %a@]}" Pp.field k
          (fun ppf -> pp_constant_union ppf var tag)
          pp_bindings r
    | TRecord (None, r, _) -> F.fprintf ppf "{@[%a@]}" pp_bindings r
    | TDict (m, _) -> F.fprintf ppf "<%a>" pp_bindings m
    | TVar v -> F.pp_print_string ppf v
    | TConstruct (TNullable, None) -> F.pp_print_string ppf "null"
    | TConstruct (TNullable, Some x) -> F.fprintf ppf "!%a" pp_pat x
    | TConstruct (TList, None) -> F.pp_print_string ppf "[]"
    | TConstruct (TList, Some l) ->
        let l, rest = to_list l in
        F.fprintf ppf "[@[%a%a@]]"
          (F.pp_print_list ~pp_sep:Pp.sep_comma pp_pat)
          l
          (F.pp_print_option pp_rest)
          rest
    | TBlock _ -> F.pp_print_string ppf "#%}...{%/#"
    | TAny -> F.pp_print_string ppf "_"
  and pp_key_values ppf (k, v) =
    match v with
    | TVar v when v = k -> Pp.field ppf k
    | v -> F.fprintf ppf "%a: %a" Pp.field k pp_pat v
  and pp_bindings ppf m =
    F.pp_print_seq ~pp_sep:Pp.sep_comma pp_key_values ppf (Map.String.to_seq m)
  and pp_rest ppf t = F.fprintf ppf ",@ ...%a" pp_pat t in
  pp_pat
