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

type row = [ `Closed | `Open ]
type sum_extra = Not_bool | Bool

let sum_extra_equal a b =
  match (a, b) with
  | Bool, Bool | Not_bool, Not_bool -> true
  | (Bool | Not_bool), _ -> false

type 'a sum = { mutable cases : 'a; mutable row : row; extra : sum_extra }
type enum = Enum_int of Set.Int.t | Enum_string of Set.String.t

type ty_ =
  | Unknown of row ref
  | Int
  | Float
  | String
  | Nullable of ty
  | List of ty
  | Tuple of ty list
  | Record of ty Map.String.t ref
  | Dict of ty * Set.String.t ref
  | Enum of enum sum
  | Union of string * union sum

and union =
  | Union_int of ty Map.String.t ref Map.Int.t
  | Union_string of ty Map.String.t ref Map.String.t

and ty = ty_ ref

let ty_unknown _ = ref (Unknown (ref `Closed))
let ty_int () = ref Int
let ty_float () = ref Float
let ty_string () = ref String
let ty_nullable ty = ref (Nullable ty)
let ty_list t = ref (List t)
let ty_tuple l = ref (Tuple l)
let ty_record m = ref (Record m)
let ty_dict_keys t keys = ref (Dict (t, keys))
let ty_dict t = ty_dict_keys t (ref Set.String.empty)
let ty_enum e = ref (Enum e)
let ty_union k u = ref (Union (k, u))
let enum_string s row = { cases = Enum_string s; row; extra = Not_bool }
let enum_string_singleton s row = enum_string (Set.String.singleton s) row
let enum_int s row = { cases = Enum_int s; row; extra = Not_bool }
let enum_int_singleton i row = enum_int (Set.Int.singleton i) row
let enum_false_and_true_cases = Enum_int Set.Int.(singleton 0 |> add 1)
let enum_bool cases = { cases; row = `Closed; extra = Bool }
let enum_false_and_true () = enum_bool enum_false_and_true_cases
let enum_true_only () = enum_bool (Enum_int (Set.Int.singleton 1))
let enum_false_only () = enum_bool (Enum_int (Set.Int.singleton 0))
let enum_bool cases = enum_bool (Enum_int cases)
let union_int m row = { cases = Union_int m; row; extra = Not_bool }
let union_int_singleton i x row = union_int (Map.Int.singleton i x) row
let union_string m row = { cases = Union_string m; row; extra = Not_bool }
let union_string_singleton s x row = union_string (Map.String.singleton s x) row
let union_bool m = { cases = Union_int m; row = `Closed; extra = Bool }
let union_bool_singleton i x = union_bool (Map.Int.singleton i x)

let union_false_and_true ~f ~t =
  union_bool (Map.Int.singleton 0 f |> Map.Int.add 1 t)

let union_false_only x = union_bool (Map.Int.singleton 0 x)
let union_true_only x = union_bool (Map.Int.singleton 1 x)

let rec copy_ty ty =
  match !ty with
  | (Int | Float | String) as x -> ref x
  | Unknown r -> ref (Unknown (ref !r))
  | Enum { cases; row; extra } -> ref (Enum { cases; row; extra })
  | Nullable r -> ref (Nullable (copy_ty r))
  | List r -> ref (List (copy_ty r))
  | Dict (r, keys) -> ref (Dict (copy_ty r, ref !keys))
  | Tuple l -> ref (Tuple (List.map copy_ty l))
  | Record r -> ref (Record (ref (copy_record !r)))
  | Union (tag, { cases; row; extra }) ->
      let cases =
        match cases with
        | Union_string m ->
            Union_string (Map.String.map (fun r -> ref (copy_record !r)) m)
        | Union_int m ->
            Union_int (Map.Int.map (fun r -> ref (copy_record !r)) m)
      in
      ref (Union (tag, { cases; row; extra }))

and copy_record m = Map.String.map copy_ty m

let pp_sum_sep ppf () = F.fprintf ppf " |@ "

let pp_row ppf = function
  | `Closed -> ()
  | `Open -> F.fprintf ppf "%a..." pp_sum_sep ()

let pp_sum ppf pp_case cases row =
  F.fprintf ppf "@[<hv 0>%a%a@]"
    (F.pp_print_seq ~pp_sep:pp_sum_sep pp_case)
    cases pp_row row

let pp_enum_string ppf s = F.fprintf ppf "%@%S" s
let pp_enum_int ppf i = F.fprintf ppf "%@%i" i
let pp_tag_field ppf s = F.fprintf ppf "%@%a" Pp.field s

let pp_record =
  let field pp_k pp_v ppf (k, v) =
    F.fprintf ppf "@[<hv 2>%a:@ %a@]" pp_k k pp_v v
  in
  let record ?tag pp ppf m =
    (match tag with
    | None -> ()
    | Some (pp_tag, tag) ->
        field pp_tag_field pp_tag ppf tag;
        if not (Map.String.is_empty !m) then Pp.sep_comma ppf ());
    F.pp_print_seq ~pp_sep:Pp.sep_comma (field Pp.field pp) ppf
      (Map.String.to_seq !m)
  in
  fun ?tag pp ppf m -> Pp.surround ~left:'{' ~right:'}' (record ?tag pp) ppf m

let rec pp_ty ppf t =
  match !t with
  | Unknown _ -> F.pp_print_string ppf "_"
  | Int -> F.pp_print_string ppf "int"
  | Float -> F.pp_print_string ppf "float"
  | String -> F.pp_print_string ppf "string"
  | Nullable x -> F.fprintf ppf "?@[<hv 1>@,%a@]" pp_ty x
  | List t -> Pp.surround ~left:'[' ~right:']' pp_ty ppf t
  | Dict (t, _) -> Pp.surround ~left:'<' ~right:'>' pp_ty ppf t
  | Record r -> pp_record pp_ty ppf r
  | Tuple l ->
      Pp.surround ~left:'(' ~right:')'
        (F.pp_print_list ~pp_sep:Pp.sep_comma pp_ty)
        ppf l
  | Enum { cases = Enum_string cases; row; _ } ->
      pp_sum ppf pp_enum_string (Set.String.to_seq cases) row
  | Enum { cases = Enum_int cases; extra = Not_bool; row } ->
      pp_sum ppf pp_enum_int (Set.Int.to_seq cases) row
  | Enum { cases = Enum_int cases; extra = Bool; row } ->
      pp_sum ppf Pp.bool (Set.Int.to_seq cases) row
  | Union (key, union) -> (
      let aux pp_tag ppf (tag, fields) =
        pp_record ~tag:(pp_tag, (key, tag)) pp_ty ppf fields
      in
      match union with
      | { cases = Union_string cases; row; _ } ->
          pp_sum ppf (aux Pp.syntax_string) (Map.String.to_seq cases) row
      | { cases = Union_int cases; extra = Not_bool; row } ->
          pp_sum ppf (aux F.pp_print_int) (Map.Int.to_seq cases) row
      | { cases = Union_int cases; extra = Bool; row } ->
          pp_sum ppf (aux Pp.bool) (Map.Int.to_seq cases) row)

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
  | Enum ty -> (
      match ty.extra with
      | Not_bool -> ty.row <- `Open
      | Bool -> ty.cases <- enum_false_and_true_cases)
  | Union (_, ty) -> (
      match ty with
      | { cases = Union_int cases; extra = Bool; _ } ->
          ty.cases <-
            Union_int
              (Map.Int.update 0 open_rows_bool_union_aux cases
              |> Map.Int.update 1 open_rows_bool_union_aux)
      | { cases = Union_int cases; _ } ->
          Map.Int.iter
            (fun _ v -> Map.String.iter (fun _ v -> open_rows v) !v)
            cases;
          ty.row <- `Open
      | { cases = Union_string cases; _ } ->
          Map.String.iter
            (fun _ v -> Map.String.iter (fun _ v -> open_rows v) !v)
            cases;
          ty.row <- `Open)
  | Tuple a -> List.iter open_rows a
  | Record ty -> Map.String.iter (fun _ v -> open_rows v) !ty
  | Nullable ty | List ty | Dict (ty, _) -> open_rows ty
  | Unknown row -> row := `Open
  | Int | Float | String -> ()

type unify_mode =
  | Destruct_expand  (** Record a and b both take on each other's fields. *)
  | Construct_literal  (** Record a is narrowed to a subset of a and b. *)
  | Construct_var  (** Record a expands into b only. *)

let unify_enum_cases _ a b =
  match (a, b) with
  | Enum_string a, Enum_string b -> Enum_string (Set.String.union a b)
  | Enum_int a, Enum_int b -> Enum_int (Set.Int.union a b)
  | _ -> raise_notrace Clash

let subset_enum_cases _ a b =
  let success =
    match (a, b) with
    | Enum_string a, Enum_string b -> Set.String.subset b a
    | Enum_int a, Enum_int b -> Set.Int.subset b a
    | _ -> false
  in
  if not success then raise_notrace Clash

let unify_sum ~unify ~subset mode a b =
  if not (sum_extra_equal a.extra b.extra) then raise_notrace Clash;
  match mode with
  | Destruct_expand ->
      (match (a.row, b.row) with
      | `Open, _ | _, `Open -> a.row <- `Open
      | _ -> ());
      a.cases <- unify mode a.cases b.cases
  | Construct_literal -> (
      match (a.row, b.row) with
      | `Open, (`Closed | `Open) -> a.cases <- unify mode a.cases b.cases
      | `Closed, (`Closed | `Open) -> subset mode a.cases b.cases)
  | Construct_var -> (
      match (a.row, b.row) with
      | `Closed, `Closed -> subset mode a.cases b.cases
      | `Open, `Closed -> a.cases <- unify mode a.cases b.cases
      | `Open, `Open ->
          let cases = unify mode a.cases b.cases in
          a.cases <- cases;
          b.cases <- cases
      | `Closed, `Open -> raise_notrace Clash)

let rec unify mode aty bty =
  match (!aty, !bty) with
  | Int, Int | Float, Float | String, String -> ()
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
      unify_sum ~unify:unify_enum_cases ~subset:subset_enum_cases mode a b
  | Union (ka, a), Union (kb, b) when String.equal ka kb ->
      unify_sum ~unify:unify_union_cases ~subset:subset_union_cases mode a b
  | Unknown { contents = `Open }, t ->
      (match mode with Destruct_expand -> open_rows bty | _ -> ());
      aty := t
  | Unknown _, t -> aty := t
  | t, Unknown _ -> bty := t
  | _ -> raise_notrace Clash

and unify_record mode a b =
  match mode with
  | Destruct_expand ->
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
  | Union_string a, Union_string b -> Union_string (Map.String.union f a b)
  | Union_int a, Union_int b -> Union_int (Map.Int.union f a b)
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
  | Union_string a, Union_string b -> Map.String.merge f a b |> ignore
  | Union_int a, Union_int b -> Map.Int.merge f a b |> ignore
  | _ -> raise_notrace Clash

let unify loc mode a b =
  try unify mode a b with Clash -> Error.type_mismatch loc pp_ty a b

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
  | Int, Int | Float, Float | String, String | _, Unknown _ -> true
  | Nullable a, Nullable b | List a, List b | Dict (a, _), Dict (b, _) ->
      check_interface ~interface:a ~impl:b
  | Tuple a, Tuple b ->
      List.equal (fun a b -> check_interface ~interface:a ~impl:b) a b
  | Record a, Record b -> check_interface_record ~interface:a ~impl:b
  | Enum a, Enum b -> (
      sum_extra_equal a.extra b.extra
      &&
      match (a.row, b.row) with
      | `Closed, `Closed -> (
          match (a.cases, b.cases) with
          | Enum_int a, Enum_int b -> Set.Int.equal a b
          | Enum_string a, Enum_string b -> Set.String.equal a b
          | _ -> false)
      | `Open, `Open -> (
          match (a.cases, b.cases) with
          | Enum_int interface, Enum_int impl -> Set.Int.subset impl interface
          | Enum_string interface, Enum_string impl ->
              Set.String.subset impl interface
          | _ -> false)
      | _ -> false)
  | Union (ka, a), Union (kb, b) -> (
      sum_extra_equal a.extra b.extra
      && String.equal ka kb
      &&
      match (a.row, b.row) with
      | `Closed, `Closed -> (
          match (a.cases, b.cases) with
          | Union_int a, Union_int b ->
              Map.Int.equal
                (fun a b -> check_interface_record ~interface:a ~impl:b)
                a b
          | Union_string a, Union_string b ->
              Map.String.equal
                (fun a b -> check_interface_record ~interface:a ~impl:b)
                a b
          | _ -> false)
      | `Open, `Open -> (
          match (a.cases, b.cases) with
          | Union_int interface, Union_int impl ->
              map_int_subset check_interface_record ~interface ~impl
          | Union_string interface, Union_string impl ->
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
            Error.interface_type_mismatch loc k pp_ty interface impl
      | None -> Error.interface_missing_prop loc k pp_ty impl)
    impl

type echo = [ `Var of string | `String of string | `Field of echo * string ]
type scalar = [ `Int of int | `Float of float | `String of string ]
type tag = string * [ `Int of int | `String of string ] * union sum
type construct = private Construct_tag
type destruct = private Destruct_tag

type _ pat =
  | TScalar : scalar * enum sum option -> 'a pat
  | TNil : 'a pat
  | TCons : 'a pat -> 'a pat
  | TTuple : 'a pat list -> 'a pat
  | TRecord : tag option * 'a pat Map.String.t * ty Map.String.t ref -> 'a pat
  | TDict : 'a pat Map.String.t * Set.String.t ref -> 'a pat
  | TVar : string -> 'a pat
  | TBlock : nodes -> construct pat
  | TField : construct pat * string -> construct pat
  | TAny : destruct pat

and node =
  | TText of string * Ast.trim * Ast.trim
  | TEcho of (Ast.echo_format * echo) list * Ast.echo_format * echo * Ast.escape
  | TMatch of Loc.t * construct pat Nonempty.t * ty Nonempty.t * case Nonempty.t
  | TMap_list of Loc.t * construct pat * ty Nonempty.t * case Nonempty.t
  | TMap_dict of Loc.t * construct pat * ty Nonempty.t * case Nonempty.t
  | TComponent of string * construct pat Map.String.t

and case = {
  pats : (Loc.t * destruct pat Nonempty.t) Nonempty.t;
  nodes : nodes;
}

and nodes = node list

type t = { nodes : nodes; types : ty Map.String.t }

type ('a, 'b) source =
  | Src of string * 'a
  | Fun of string * ty Map.String.t * 'b

let get_types = function Src (_, { types; _ }) | Fun (_, types, _) -> types

module Context = struct
  type typed_tree = t
  type used = Unused | Used
  type binding = { loc : Loc.t; name : string; mutable used : used; ty : ty }

  type 'a t = {
    global : ty Map.String.t ref;
    scope : binding Map.String.t;
    all_bindings : binding Queue.t;
        (** This is for checking for unused variables. A queue preserves the
            order for friendlier debugging. *)
    interface : (Loc.t * ty) Map.String.t ref option ref;
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
    | "int" -> ty_int ()
    | "string" -> ty_string ()
    | "float" -> ty_float ()
    | "_" -> ty_unknown ()
    | s -> Error.interface_bad_name loc s

  let error_tag expected = function
    | Ast.Tag_int (loc, _) -> Error.type_mismatch loc pp_ty expected (ty_int ())
    | Ast.Tag_bool (loc, 0) ->
        Error.type_mismatch loc pp_ty expected (ty_enum (enum_false_only ()))
    | Ast.Tag_bool (loc, _) ->
        Error.type_mismatch loc pp_ty expected (ty_enum (enum_true_only ()))
    | Ast.Tag_string (loc, _) ->
        Error.type_mismatch loc pp_ty expected (ty_string ())

  let tag_int = function
    | Ast.Tag_int (_, i) -> i
    | t -> error_tag (ty_int ()) t

  let tag_string = function
    | Ast.Tag_string (_, s) -> s
    | t -> error_tag (ty_string ()) t

  let tag_bool = function
    | Ast.Tag_bool (_, i) -> i
    | t -> error_tag (ty_enum (enum_false_and_true ())) t

  let rec make_ty = function
    | Ast.Ty_named (loc, s) -> named loc s
    | Ast.Ty_nullable t -> ty_nullable (make_ty t)
    | Ast.Ty_list t -> ty_list (make_ty t)
    | Ast.Ty_dict t -> ty_dict (make_ty t)
    | Ast.Ty_tuple l -> ty_tuple (List.map make_ty l)
    | Ast.Ty_enum_int (l, (_, r)) ->
        ty_enum (enum_int (Nonempty.to_seq l |> Set.Int.of_seq) r)
    | Ast.Ty_enum_bool l ->
        ty_enum (enum_bool (Nonempty.to_seq l |> Set.Int.of_seq))
    | Ast.Ty_enum_string (l, (_, r)) ->
        ty_enum (enum_string (Nonempty.to_seq l |> Set.String.of_seq) r)
    | Ast.Ty_record ((loc, hd) :: tl, (row_l, row)) -> (
        match assoc_to_record hd with
        | Untagged m -> (
            match (tl, row) with
            | [], `Closed -> Map.String.map make_ty m |> ref |> ty_record
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
                ty_union tagk (union_int m row)
            | Tag_bool (_, i) -> (
                match row with
                | `Closed ->
                    ty_union tagk
                      (union_bool
                         Map.Int.(singleton i (ref m) |> aux update tag_bool))
                | `Open -> Error.interface_open_bool_union row_l)
            | Tag_string (_, s) ->
                let m =
                  Map.String.(singleton s (ref m) |> aux update tag_string)
                in
                ty_union tagk (union_string m row)))

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
  | Ast.Fmt_string -> ty_string ()
  | Fmt_int -> ty_int ()
  | Fmt_float -> ty_float ()
  | Fmt_bool -> ty_enum (enum_false_and_true ())

let[@tail_mod_cons] rec make_echo ctx ty = function
  | Ast.Echo_var (loc, var) ->
      Context.update ctx loc var ty;
      `Var var
  | Echo_field (var, field) ->
      let ty = Map.String.singleton field ty |> ref |> ty_record in
      `Field (make_echo ctx ty var, field)
  | Echo_string (loc, s) ->
      unify loc Construct_literal ty (ty_string ());
      `String s

let[@tail_mod_cons] rec make_nullable_echoes ctx = function
  | [] -> []
  | (fmt, echo) :: tl ->
      let ty = make_echo_type fmt in
      let echo = make_echo ctx (ty_nullable ty) echo in
      (fmt, echo) :: make_nullable_echoes ctx tl

let add_default_wildcard cases =
  Nonempty.map
    (fun case ->
      {
        case with
        Ast.pats =
          Nonempty.map
            (function
              | loc, Nonempty.[ h ] -> (loc, Nonempty.[ h; Ast.dummy_var ])
              | pat -> pat)
            case.Ast.pats;
      })
    cases

let make_row = function
  | Destruct_expand -> `Closed
  | Construct_literal | Construct_var -> `Open

(** This compliments the [mode] type with more information for [make_pat]. *)
type (_, _) var_action =
  | Destruct_add_vars :
      (Loc.t * string * ty) Queue.t
      -> (destruct, 'b) var_action
      (** When we destructure a pattern, we add all new variables to a queue. *)
  | Construct_update_vars : 'b Context.t -> (construct, 'b) var_action
      (** When we construct a pattern, we update the context for each new
          variable. *)

(** When we type-check a pattern, we create a temporary type and unify it
    with the input type. Information retained in the resulting typed-pattern
    structure, for example dictionary keys or sum-type data, must come from the
    input type if possible, not the newly created one. The input type is the
    master copy which gets reused, and the temporary copy will become stale. *)

let rec make_pat :
    type a. (a, 'b) var_action -> unify_mode -> ty -> Ast.pat -> a pat =
 fun var_action mode ty -> function
  | Int (loc, i) ->
      unify loc mode ty (ty_int ());
      TScalar (`Int i, None)
  | String (loc, s) ->
      unify loc mode ty (ty_string ());
      TScalar (`String s, None)
  | Block (loc, nodes) -> (
      unify loc mode ty (ty_string ());
      match var_action with
      | Destruct_add_vars _ -> Error.bad_block loc
      | Construct_update_vars ctx -> TBlock (make_nodes ctx nodes))
  | Field (loc, rec_pat, field) -> (
      let rec_ty = Map.String.singleton field ty |> ref |> ty_record in
      let rec_pat = make_pat var_action mode rec_ty rec_pat in
      match var_action with
      | Destruct_add_vars _ -> Error.bad_field loc
      | Construct_update_vars _ -> TField (rec_pat, field))
  | Float (loc, f) ->
      unify loc mode ty (ty_float ());
      TScalar (`Float f, None)
  | Bool (loc, b) ->
      let temp_enum =
        match var_action with
        | Destruct_add_vars _ -> (
            match b with 0 -> enum_false_only () | _ -> enum_true_only ())
        | Construct_update_vars _ -> enum_false_and_true ()
      in
      let temp_ty = ty_enum temp_enum in
      let enum = match !ty with Enum e -> e | _ -> temp_enum in
      unify loc mode ty temp_ty;
      TScalar (`Int b, Some enum)
  | Enum_string (loc, s) ->
      let temp_enum = enum_string_singleton s (make_row mode) in
      let temp_ty = ty_enum temp_enum in
      let enum = match !ty with Enum e -> e | _ -> temp_enum in
      unify loc mode ty temp_ty;
      TScalar (`String s, Some enum)
  | Enum_int (loc, i) ->
      let temp_enum = enum_int_singleton i (make_row mode) in
      let temp_ty = ty_enum temp_enum in
      let enum = match !ty with Enum e -> e | _ -> temp_enum in
      unify loc mode ty temp_ty;
      TScalar (`Int i, Some enum)
  | Nullable (loc, pat) ->
      let tyvar = match !ty with Nullable ty -> ty | _ -> ty_unknown () in
      let pat =
        match pat with
        | None -> TNil
        | Some pat -> TCons (TTuple [ make_pat var_action mode tyvar pat ])
      in
      unify loc mode ty (ty_nullable tyvar);
      pat
  | List (loc, l, tl) ->
      let tyvar = match !ty with List ty -> ty | _ -> ty_unknown () in
      unify loc mode ty (ty_list tyvar);
      let tl =
        match tl with None -> TNil | Some tl -> make_pat var_action mode ty tl
      in
      make_list ~tl var_action mode tyvar l
  | Tuple (loc, l) ->
      let temp_tyvars = List.map ty_unknown l in
      let tyvars = match !ty with Tuple tys -> tys | _ -> temp_tyvars in
      unify loc mode ty (ty_tuple tyvars);
      TTuple (List.map2 (make_pat var_action mode) tyvars l)
  | Record (loc, r) -> (
      match assoc_to_record r with
      | Untagged m ->
          let temp_tyvars = Map.String.map ty_unknown m |> ref in
          let tyvars = match !ty with Record tys -> tys | _ -> temp_tyvars in
          unify loc mode ty (ty_record temp_tyvars);
          let r = make_record var_action loc mode !tyvars m in
          TRecord (None, r, tyvars)
      | Tagged (k, v, m) ->
          let row = make_row mode in
          let temp_tyvars = Map.String.map ty_unknown m |> ref in
          let tag, temp_enum =
            match v with
            | Tag_int (_, i) -> (`Int i, union_int_singleton i temp_tyvars row)
            | Tag_bool (_, i) -> (`Int i, union_bool_singleton i temp_tyvars)
            | Tag_string (_, s) ->
                (`String s, union_string_singleton s temp_tyvars row)
          in
          let enum = match !ty with Union (_, e) -> e | _ -> temp_enum in
          let tyvars =
            try
              match (tag, enum.cases) with
              | `Int i, Union_int m -> Map.Int.find i m
              | `String s, Union_string m -> Map.String.find s m
              | _ -> raise_notrace Not_found
            with Not_found -> temp_tyvars
          in
          unify loc mode ty (ty_union k temp_enum);
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
        | _ -> (ty_unknown (), temp_kys)
      in
      unify loc mode ty (ty_dict_keys tyvar temp_kys);
      let d = Map.String.map (make_pat var_action mode tyvar) m in
      TDict (d, kys)
  | Var (loc, "_") -> (
      match var_action with
      | Destruct_add_vars _ ->
          open_rows ty;
          TAny
      | Construct_update_vars _ -> Error.underscore_in_construct loc)
  | Var (loc, b) ->
      (match var_action with
      | Destruct_add_vars queue ->
          open_rows ty;
          Queue.add (loc, b, ty) queue
      | Construct_update_vars ctx -> Context.update ctx loc b ty);
      TVar b

and[@tail_mod_cons] make_list :
    type a.
    tl:a pat -> (a, 'b) var_action -> unify_mode -> ty -> Ast.pat list -> a pat
    =
 fun ~tl var_action mode ty -> function
  | [] -> tl
  | p :: l ->
      let hd = make_pat var_action mode ty p in
      TCons (TTuple [ hd; make_list ~tl var_action mode ty l ])

and make_record :
    type a.
    (a, 'b) var_action ->
    Loc.t ->
    unify_mode ->
    ty Map.String.t ->
    Ast.pat Map.String.t ->
    a pat Map.String.t =
 fun var_action loc mode tyvars m ->
  match var_action with
  | Destruct_add_vars _ ->
      Map.String.merge
        (fun _ pat ty ->
          match (pat, ty) with
          | Some pat, None ->
              Some (make_pat var_action mode (ty_unknown ()) pat)
          | Some pat, Some ty -> Some (make_pat var_action mode ty pat)
          | None, Some _ -> Some TAny
          | None, None -> None)
        m tyvars
  | Construct_update_vars _ ->
      Map.String.merge
        (fun k pat ty ->
          match (pat, ty) with
          | Some pat, Some ty -> Some (make_pat var_action mode ty pat)
          | None, Some ty -> Error.missing_field loc k pp_ty ty
          | _ -> None)
        m tyvars

and make_component_props loc name ctx tyvars m =
  Map.String.merge
    (fun k pat ty ->
      match (pat, ty) with
      | Some pat, Some ty ->
          Some (make_pat (Construct_update_vars ctx) Construct_literal ty pat)
      | None, Some ty -> Error.missing_field loc k pp_ty ty
      | Some _, None -> Error.component_extra_prop loc name k
      | None, None -> None)
    m tyvars

(** @raises [Invalid_argument] if the list sizes are mismatched. *)
and unify_match_cases pats tys ctx =
  Nonempty.map2
    (make_pat (Construct_update_vars ctx) Construct_literal)
    tys pats

and unify_map ~ty ~key loc (tys, cases) pat ctx =
  let hd_ty =
    match tys with
    | Nonempty.[ hd; tl ] ->
        unify loc Construct_literal (key ()) tl;
        ty hd
    | _ -> Error.map_pat_num_mismatch loc
  in
  let p = make_pat (Construct_update_vars ctx) Construct_literal hd_ty pat in
  (p, tys, cases)

and make_cases ctx cases =
  let tys =
    let _, pats = (Nonempty.hd cases).Ast.pats |> Nonempty.hd in
    Nonempty.map ty_unknown pats
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
                    (make_pat (Destruct_add_vars new_vars) Destruct_expand)
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
          let types = copy_record types in
          let missing_to_nullable _ ty prop =
            match (prop, ty) with
            | None, Some { contents = Nullable _ } ->
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
            unify_map ~ty:ty_list ~key:ty_int loc cases pattern ctx
          in
          Some (TMap_list (loc, pattern, ty, cases))
      | Map_dict (loc, pattern, cases) ->
          let cases = add_default_wildcard cases |> make_cases ctx in
          let pattern, ty, cases =
            unify_map ~ty:ty_dict ~key:ty_string loc cases pattern ctx
          in
          Some (TMap_dict (loc, pattern, ty, cases))
      | Interface (loc, i) ->
          Interface.make loc ctx i;
          None
      | Comment _ -> None)
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
