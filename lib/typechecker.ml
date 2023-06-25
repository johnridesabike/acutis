(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module C = Data.Const
module Ty = Typescheme

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
      | { cases = Ty.Union.Int cases; extra = Bool; _ } ->
          ty.cases <-
            Ty.Union.Int
              (Map.Int.update 0 open_rows_bool_union_aux cases
              |> Map.Int.update 1 open_rows_bool_union_aux)
      | { cases = Ty.Union.Int cases; _ } ->
          Map.Int.iter
            (fun _ v -> Map.String.iter (fun _ v -> open_rows v) !v)
            cases;
          ty.row <- `Open
      | { cases = Ty.Union.String cases; _ } ->
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
  | Ty.Enum.String a, Ty.Enum.String b -> Ty.Enum.String (Set.String.union a b)
  | Ty.Enum.Int a, Ty.Enum.Int b -> Ty.Enum.Int (Set.Int.union a b)
  | _ -> raise_notrace Clash

let subset_enum_cases _ a b =
  let success =
    match (a, b) with
    | Ty.Enum.String a, Ty.Enum.String b -> Set.String.subset b a
    | Ty.Enum.Int a, Ty.Enum.Int b -> Set.Int.subset b a
    | _ -> false
  in
  if not success then raise_notrace Clash

let unify_variant ~unify_cases ~subset_cases mode a b =
  match mode with
  | Destructure_expand ->
      (match (a.Ty.row, b.Ty.row) with
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
            | (Some a as x), Some b ->
                unify mode a b;
                x
            | None, (Some b as x) ->
                open_rows b;
                x
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
  | Ty.Union.String a, Ty.Union.String b ->
      Ty.Union.String (Map.String.union f a b)
  | Ty.Union.Int a, Ty.Union.Int b -> Ty.Union.Int (Map.Int.union f a b)
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
  | Ty.Union.String a, Ty.Union.String b -> Map.String.merge f a b |> ignore
  | Ty.Union.Int a, Ty.Union.Int b -> Map.Int.merge f a b |> ignore
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
  | Enum a, Enum b when Ty.equal_sum_extra a.extra b.extra -> (
      match (a.row, b.row) with
      | `Closed, `Closed -> (
          match (a.cases, b.cases) with
          | Ty.Enum.Int a, Ty.Enum.Int b -> Set.Int.equal a b
          | Ty.Enum.String a, Ty.Enum.String b -> Set.String.equal a b
          | _ -> false)
      | `Open, `Open -> (
          match (a.cases, b.cases) with
          | Ty.Enum.Int interface, Ty.Enum.Int impl ->
              Set.Int.subset impl interface
          | Ty.Enum.String interface, Ty.Enum.String impl ->
              Set.String.subset impl interface
          | _ -> false)
      | _ -> false)
  | Union (ka, a), Union (kb, b)
    when String.equal ka kb && Ty.equal_sum_extra a.extra b.extra -> (
      match (a.row, b.row) with
      | `Closed, `Closed -> (
          match (a.cases, b.cases) with
          | Ty.Union.Int a, Ty.Union.Int b ->
              Map.Int.equal
                (fun a b -> check_interface_record ~interface:a ~impl:b)
                a b
          | Ty.Union.String a, Ty.Union.String b ->
              Map.String.equal
                (fun a b -> check_interface_record ~interface:a ~impl:b)
                a b
          | _ -> false)
      | `Open, `Open -> (
          match (a.cases, b.cases) with
          | Ty.Union.Int interface, Ty.Union.Int impl ->
              map_int_subset check_interface_record ~interface ~impl
          | Ty.Union.String interface, Ty.Union.String impl ->
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

type echo =
  | Echo_var of string
  | Echo_string of string
  | Echo_field of echo * string

type construct = TList | TNullable

type pat =
  | TConst of C.t * Ty.Enum.t option
  | TConstruct of construct * pat option
  | TTuple of pat list
  | TRecord of
      (string * C.t * Ty.t Ty.Union.t) option
      * pat Map.String.t
      * Ty.t Map.String.t ref
  | TDict of pat Map.String.t * Set.String.t ref
  | TVar of string
  | TBlock of nodes
  | TField of pat * string
  | TAny

and node =
  | TText of string * Ast.trim * Ast.trim
  | TEcho of (Ast.echo_format * echo) list * Ast.echo_format * echo * Ast.escape
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

let get_types = function Src (_, { types; _ }) | Fun (_, types, _) -> types

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
    (* Turn each row of variables into a map and check for duplicate keys. *)
    let var_matrix =
      Queue.fold
        (fun var_matrix var_row ->
          Queue.fold
            (fun var_map (loc, name, ty) ->
              Map.String.update name
                (function
                  | Some _ -> Error.name_bound_too_many loc name
                  | None -> Some { loc; used = Unused; name; ty })
                var_map)
            Map.String.empty var_row
          :: var_matrix)
        [] var_matrix
      |> List.rev
    in
    match var_matrix with
    | [] -> ctx
    | hd :: vars ->
        (* Merge the maps into one map. Unify the types and check that both maps
           have identical keys. *)
        let scope =
          List.fold_left
            (fun acc m ->
              Map.String.merge
                (fun name a b ->
                  match (a, b) with
                  | (Some { loc; ty = a; _ } as a'), Some { ty = b; _ } ->
                      unify loc Construct_literal a b;
                      a'
                  | Some { loc; _ }, None | None, Some { loc; _ } ->
                      Error.var_missing loc name
                  | None, None -> None)
                acc m)
            hd vars
          (* Add them to the scope, replacing (shadowing) the old variables. *)
          |> Map.String.merge
               (fun _k oldvar newvar ->
                 match newvar with
                 | Some newvar ->
                     Queue.add newvar ctx.all_bindings;
                     Some newvar
                 | None -> oldvar)
               ctx.scope
        in
        { ctx with scope }
end

let map_add_unique loc k v m =
  Map.String.update k
    (function None -> Some v | Some _ -> Error.dup_record_key loc k)
    m

let assoc_to_map =
  let rec aux m = function
    | [] -> m
    | (loc, k, v) :: tl -> aux (map_add_unique loc k v m) tl
  in
  fun l -> aux Map.String.empty l

type 'a record =
  | Untagged of 'a Map.String.t
  | Tagged of string * Ast.tag * 'a Map.String.t

let assoc_to_record =
  let rec aux m l =
    match (m, l) with
    | m, [] -> m
    | Tagged _, (loc, _, Ast.Tag _) :: _ -> Error.extra_record_tag loc
    | Tagged (tagk, tagv, m), (loc, k, Value v) :: tl ->
        if tagk = k then Error.dup_record_key loc k
        else aux (Tagged (tagk, tagv, map_add_unique loc k v m)) tl
    | Untagged m, (loc, k, Ast.Value v) :: tl ->
        aux (Untagged (map_add_unique loc k v m)) tl
    | Untagged m, (loc, k, Ast.Tag v) :: tl ->
        if Map.String.mem k m then Error.dup_record_key loc k
        else aux (Tagged (k, v, m)) tl
  in
  fun Nonempty.((_, k, v) :: tl) ->
    aux
      (match v with
      | Ast.Tag v -> Tagged (k, v, Map.String.empty)
      | Ast.Value v -> Untagged (Map.String.singleton k v))
      tl

module Interface = struct
  let there_can_be_only_one loc k v = function
    | None -> Some v
    | Some _ -> Error.interface_duplicate loc k

  let update loc k v interface =
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
    | Ast.Tag_int (loc, _) -> Error.type_mismatch loc expected (Ty.int ())
    | Ast.Tag_bool (loc, 0) ->
        Error.type_mismatch loc expected (Ty.false_only ())
    | Ast.Tag_bool (loc, _) ->
        Error.type_mismatch loc expected (Ty.true_only ())
    | Ast.Tag_string (loc, _) -> Error.type_mismatch loc expected (Ty.string ())

  let tag_int = function
    | Ast.Tag_int (_, i) -> i
    | t -> error_tag (Ty.int ()) t

  let tag_string = function
    | Ast.Tag_string (_, s) -> s
    | t -> error_tag (Ty.string ()) t

  let tag_bool = function
    | Ast.Tag_bool (_, i) -> i
    | t -> error_tag (Ty.boolean ()) t

  let rec make_ty = function
    | Ast.Ty_named (loc, s) -> named loc s
    | Ast.Ty_nullable t -> Ty.nullable (make_ty t)
    | Ast.Ty_list t -> Ty.list (make_ty t)
    | Ast.Ty_dict t -> Ty.dict (make_ty t)
    | Ast.Ty_tuple l -> Ty.tuple (List.map make_ty l)
    | Ast.Ty_enum_int (l, r) -> Ty.enum_int r (Nonempty.to_list l)
    | Ast.Ty_enum_bool l -> Ty.internal_bool (Nonempty.to_list l)
    | Ast.Ty_enum_string (l, r) -> Ty.enum_string r (Nonempty.to_list l)
    | Ast.Ty_record ((loc, hd) :: tl, row) -> (
        match assoc_to_record hd with
        | Untagged m -> (
            match (tl, row) with
            | [], `Closed ->
                Map.String.map make_ty m |> ref |> Ty.internal_record
            | _ :: _, _ | _, `Open -> Error.interface_untagged_union loc)
        | Tagged (tagk, tagv, m) -> (
            let aux update parse_tag m =
              List.fold_left
                (fun acc (loc, l) ->
                  match assoc_to_record l with
                  | Tagged (tagk', tagv, m) ->
                      if tagk <> tagk' then
                        Error.interface_unmatched_tags loc tagk tagk'
                      else
                        update (parse_tag tagv)
                          (function
                            | None -> Some (Map.String.map make_ty m |> ref)
                            | Some _ ->
                                Error.interface_duplicate_tag loc Ast.pp_tag
                                  tagv)
                          acc
                  | Untagged _ -> Error.interface_untagged_union loc)
                m tl
            in
            let m = Map.String.map make_ty m in
            match tagv with
            | Tag_int (_, i) ->
                let m = Map.Int.(singleton i (ref m) |> aux update tag_int) in
                ref (Ty.Union (tagk, { cases = Int m; row; extra = Not_bool }))
            | Tag_bool (_, i) ->
                let m = Map.Int.(singleton i (ref m) |> aux update tag_bool) in
                ref (Ty.Union (tagk, { cases = Int m; row; extra = Bool }))
            | Tag_string (_, s) ->
                let m =
                  Map.String.(singleton s (ref m) |> aux update tag_string)
                in
                ref
                  (Ty.Union (tagk, { cases = String m; row; extra = Not_bool }))
            ))

  let make loc ctx l =
    ctx.Context.interface_loc := loc;
    List.iter
      (fun { Ast.loc; name; ty } -> update loc name (make_ty ty) ctx.interface)
      l
end

let make_interface_standalone l =
  let interface = ref None in
  List.iter
    (fun { Ast.loc; name; ty } ->
      Interface.update loc name (Interface.make_ty ty) interface)
    l;
  match !interface with
  | None -> Map.String.empty
  | Some interface -> Map.String.map snd !interface

let make_echo_type = function
  | Ast.Fmt_string -> Ty.string ()
  | Fmt_int -> Ty.int ()
  | Fmt_float -> Ty.float ()
  | Fmt_bool -> Ty.boolean ()

let[@tail_mod_cons] rec make_echo ctx ty = function
  | Ast.Echo_var (loc, var) ->
      Context.update ctx loc var ty;
      Echo_var var
  | Echo_field (var, field) ->
      let ty = Map.String.singleton field ty |> ref |> Ty.internal_record in
      Echo_field (make_echo ctx ty var, field)
  | Echo_string (loc, s) ->
      unify loc Construct_literal ty (Ty.string ());
      Echo_string s

let[@tail_mod_cons] rec make_nullable_echoes ctx = function
  | [] -> []
  | (fmt, echo) :: tl ->
      let ty = make_echo_type fmt in
      let echo = make_echo ctx (Ty.nullable ty) echo in
      (fmt, echo) :: make_nullable_echoes ctx tl

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

type 'a var_action =
  | Add_vars of (Loc.t * string * Ty.t) Queue.t
      (** When we destructure a pattern, we add all new variables to a queue. *)
  | Update_vars of 'a Context.t
      (** When we construct a pattern, we update the context for each new
          variable. *)

(** When we type-check a pattern, we create a temporary type and unify it
    with the input type. Information retained in the resulting typed-pattern
    structure, for example dictionary keys or sum-type data, must come from the
    input type if possible, not the newly created one. The input type is the
    master copy which gets reused, and the temporary copy will become stale. *)

let rec make_pat var_action mode ty = function
  | Ast.Int (loc, i) ->
      unify loc mode ty (Ty.int ());
      TConst (C.int i, None)
  | String (loc, s) ->
      unify loc mode ty (Ty.string ());
      TConst (C.string s, None)
  | Block (loc, nodes) -> (
      unify loc mode ty (Ty.string ());
      match var_action with
      | Add_vars _ -> Error.bad_block loc
      | Update_vars ctx -> TBlock (make_nodes ctx nodes))
  | Field (loc, rec_pat, field) -> (
      let rec_ty = Map.String.singleton field ty |> ref |> Ty.internal_record in
      let rec_pat = make_pat var_action mode rec_ty rec_pat in
      match var_action with
      | Add_vars _ -> Error.bad_field loc
      | Update_vars _ -> TField (rec_pat, field))
  | Float (loc, f) ->
      unify loc mode ty (Ty.float ());
      TConst (C.float f, None)
  | Bool (loc, b) ->
      let temp_enum =
        match mode with
        | Destructure_expand -> (
            match b with
            | 0 -> Ty.Enum.false_only ()
            | _ -> Ty.Enum.true_only ())
        | Construct_var | Construct_literal -> Ty.Enum.false_and_true ()
      in
      let temp_ty = ref (Ty.Enum temp_enum) in
      let enum = match !ty with Enum e -> e | _ -> temp_enum in
      unify loc mode ty temp_ty;
      TConst (C.int b, Some enum)
  | Enum_string (loc, s) ->
      let temp_enum =
        match mode with
        | Destructure_expand -> Ty.Enum.string_singleton s `Closed
        | Construct_var | Construct_literal -> Ty.Enum.string_singleton s `Open
      in
      let temp_ty = ref (Ty.Enum temp_enum) in
      let enum = match !ty with Enum e -> e | _ -> temp_enum in
      unify loc mode ty temp_ty;
      TConst (C.string s, Some enum)
  | Enum_int (loc, i) ->
      let temp_enum =
        match mode with
        | Destructure_expand -> Ty.Enum.int_singleton i `Closed
        | Construct_var | Construct_literal -> Ty.Enum.int_singleton i `Open
      in
      let temp_ty = ref (Ty.Enum temp_enum) in
      let enum = match !ty with Enum e -> e | _ -> temp_enum in
      unify loc mode ty temp_ty;
      TConst (C.int i, Some enum)
  | Nullable (loc, pat) ->
      let tyvar = match !ty with Nullable ty -> ty | _ -> Ty.unknown () in
      let pat =
        match pat with
        | None -> None
        | Some pat -> Some (TTuple [ make_pat var_action mode tyvar pat ])
      in
      unify loc mode ty (Ty.nullable tyvar);
      TConstruct (TNullable, pat)
  | List (loc, l, tl) ->
      let tyvar = match !ty with List ty -> ty | _ -> Ty.unknown () in
      unify loc mode ty (Ty.list tyvar);
      let tl =
        match tl with
        | None -> TConstruct (TList, None)
        | Some tl -> make_pat var_action mode ty tl
      in
      make_list ~tl var_action mode tyvar l
  | Tuple (loc, l) ->
      let temp_tyvars = List.map unknown l in
      let tyvars = match !ty with Tuple tys -> tys | _ -> temp_tyvars in
      unify loc mode ty (Ty.tuple tyvars);
      TTuple (List.map2 (make_pat var_action mode) tyvars l)
  | Record (loc, r) -> (
      match assoc_to_record r with
      | Untagged m ->
          let temp_tyvars = Map.String.map unknown m |> ref in
          let tyvars = match !ty with Record tys -> tys | _ -> temp_tyvars in
          unify loc mode ty (Ty.internal_record temp_tyvars);
          let r = make_record var_action loc mode !tyvars m in
          TRecord (None, r, tyvars)
      | Tagged (k, v, m) ->
          let row =
            match mode with
            | Destructure_expand -> `Closed
            | Construct_literal | Construct_var -> `Open
          in
          let temp_tyvars = Map.String.map unknown m |> ref in
          let tag, temp_enum =
            match v with
            | Tag_int (_, i) ->
                (C.int i, Ty.Union.int_singleton i temp_tyvars row)
            | Tag_bool (_, i) -> (C.int i, Ty.Union.bool_singleton i temp_tyvars)
            | Tag_string (_, s) ->
                (C.string s, Ty.Union.string_singleton s temp_tyvars row)
          in
          let enum = match !ty with Union (_, e) -> e | _ -> temp_enum in
          let tyvars =
            try
              match (tag, enum.cases) with
              | Int i, Int m -> Map.Int.find i m
              | String s, String m -> Map.String.find s m
              | _ -> raise_notrace Not_found
            with Not_found -> temp_tyvars
          in
          unify loc mode ty (ref (Ty.Union (k, temp_enum)));
          let r = make_record var_action loc mode !tyvars m in
          TRecord (Some (k, tag, enum), r, tyvars))
  | Dict (loc, m) ->
      let m = assoc_to_map m in
      let temp_kys =
        Map.String.to_seq m |> Seq.map fst |> Set.String.of_seq |> ref
      in
      let tyvar, kys =
        match !ty with
        | Dict (ty, ks) -> (ty, ks)
        | _ -> (Ty.unknown (), temp_kys)
      in
      unify loc mode ty (Ty.internal_dict_keys tyvar temp_kys);
      let d = Map.String.map (make_pat var_action mode tyvar) m in
      TDict (d, kys)
  | Var (loc, "_") ->
      (match mode with
      | Destructure_expand -> open_rows ty
      | Construct_literal | Construct_var -> Error.underscore_in_construct loc);
      TAny
  | Var (loc, b) ->
      (match mode with
      | Destructure_expand -> open_rows ty
      | Construct_literal | Construct_var -> ());
      (match var_action with
      | Add_vars queue -> Queue.add (loc, b, ty) queue
      | Update_vars ctx -> Context.update ctx loc b ty);
      TVar b

and[@tail_mod_cons] make_list ~tl var_action mode ty = function
  | [] -> tl
  | p :: l ->
      let hd = make_pat var_action mode ty p in
      TConstruct
        (TList, Some (TTuple [ hd; make_list ~tl var_action mode ty l ]))

and make_record var_action loc mode tyvars m =
  match mode with
  | Destructure_expand ->
      Map.String.merge
        (fun _ pat ty ->
          match (pat, ty) with
          | Some pat, None ->
              Some (make_pat var_action mode (Ty.unknown ()) pat)
          | Some pat, Some ty -> Some (make_pat var_action mode ty pat)
          | None, Some _ -> Some TAny
          | None, None -> None)
        m tyvars
  | Construct_var | Construct_literal ->
      Map.String.merge
        (fun k pat ty ->
          match (pat, ty) with
          | Some pat, Some ty -> Some (make_pat var_action mode ty pat)
          | None, Some ty -> Error.missing_field loc k ty
          | _ -> None)
        m tyvars

and make_component_props loc name ctx tyvars m =
  Map.String.merge
    (fun k pat ty ->
      match (pat, ty) with
      | Some pat, Some ty ->
          Some (make_pat (Update_vars ctx) Construct_literal ty pat)
      | None, Some ty -> Error.missing_field loc k ty
      | Some _, None -> Error.component_extra_prop loc name k
      | None, None -> None)
    m tyvars

(** @raises [Invalid_argument] if the list sizes are mismatched. *)
and unify_match_cases pats tys ctx =
  Nonempty.map2 (make_pat (Update_vars ctx) Construct_literal) tys pats

and unify_map ~ty ~key loc (tys, cases) pat ctx =
  let hd_ty =
    match tys with
    | Nonempty.[ hd; tl ] ->
        unify loc Construct_literal (key ()) tl;
        ty hd
    | _ -> Error.map_pat_num_mismatch loc
  in
  let p = make_pat (Update_vars ctx) Construct_literal hd_ty pat in
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
        let var_matrix = Queue.create () in
        let pats =
          Nonempty.map
            (fun (loc, pats) ->
              let new_vars = Queue.create () in
              Queue.add new_vars var_matrix;
              try
                ( loc,
                  Nonempty.map2
                    (make_pat (Add_vars new_vars) Destructure_expand)
                    tys pats )
              with Invalid_argument _ -> Error.pat_num_mismatch loc)
            pats
        in
        let ctx = Context.add_scope var_matrix ctx in
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
      | Echo (nullables, fmt, default, esc) ->
          let nullables = make_nullable_echoes ctx nullables in
          let ty = make_echo_type fmt in
          let default = make_echo ctx ty default in
          Some (TEcho (nullables, fmt, default, esc))
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
            assoc_to_map props
            |> Map.String.merge missing_to_nullable types
            |> make_component_props loc comp ctx types
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
