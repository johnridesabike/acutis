(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

module Type = struct
  module F = Format

  type row = [ `Closed | `Open ]
  type int_bool = Not_bool | Bool
  type 'a sum = { mutable cases : 'a; mutable row : row }

  type ty =
    | Unknown of row ref
    | Int
    | Float
    | String
    | Nullable of t
    | List of t
    | Tuple of t list
    | Record of record
    | Dict of t * Set.String.t ref
    | Enum_int of Set.Int.t sum * int_bool
    | Enum_string of Set.String.t sum
    | Union_int of string * sum_union_int * int_bool
    | Union_string of string * sum_union_string

  and sum_union_int = record Map.Int.t sum
  and sum_union_string = record Map.String.t sum
  and record = t Map.String.t ref
  and t = ty ref

  let unknown _ = ref (Unknown (ref `Closed))
  let int () = ref Int
  let float () = ref Float
  let string () = ref String
  let nullable ty = ref (Nullable ty)
  let list t = ref (List t)
  let tuple l = ref (Tuple l)
  let record m = ref (Record m)
  let dict_keys t keys = ref (Dict (t, keys))
  let dict t = dict_keys t (ref Set.String.empty)
  let sum cases row = { cases; row }
  let sum_enum_string_singleton s row = sum (Set.String.singleton s) row
  let sum_enum_int_singleton i row = sum (Set.Int.singleton i) row
  let enum_string sum = ref (Enum_string sum)
  let enum_int sum = ref (Enum_int (sum, Not_bool))
  let false_and_true_cases = Set.Int.(singleton 0 |> add 1)
  let sum_bool cases = { cases; row = `Closed }
  let sum_enum_false_and_true () = sum_bool false_and_true_cases
  let sum_enum_bool_singleton i = sum_bool (Set.Int.singleton i)
  let enum_bool sum = ref (Enum_int (sum, Bool))
  let enum_false_and_true () = enum_bool (sum_enum_false_and_true ())
  let enum_false_only () = enum_bool (sum_enum_bool_singleton 0)
  let enum_true_only () = enum_bool (sum_enum_bool_singleton 1)
  let union_int key sum = ref (Union_int (key, sum, Not_bool))
  let sum_union_int_singleton i x row = sum (Map.Int.singleton i x) row
  let sum_union_bool_singleton i x = sum_bool (Map.Int.singleton i x)
  let sum_union_string_singleton i x row = sum (Map.String.singleton i x) row
  let union_string key sum = ref (Union_string (key, sum))
  let union_bool key sum = ref (Union_int (key, sum, Bool))

  let union_false_and_true key ~f ~t =
    union_bool key (sum_bool Map.Int.(singleton 0 f |> add 1 t))

  let union_false_only key x = union_bool key (sum_bool (Map.Int.singleton 0 x))
  let union_true_only key x = union_bool key (sum_bool (Map.Int.singleton 1 x))

  let rec copy ty =
    match !ty with
    | (Int | Float | String) as x -> ref x
    | Unknown r -> ref (Unknown (ref !r))
    | Nullable r -> ref (Nullable (copy r))
    | List r -> ref (List (copy r))
    | Dict (r, keys) -> ref (Dict (copy r, ref !keys))
    | Tuple l -> ref (Tuple (List.map copy l))
    | Record r -> ref (Record (ref (copy_record !r)))
    | Enum_int ({ cases; row }, int_bool) ->
        ref (Enum_int ({ cases; row }, int_bool))
    | Enum_string { cases; row } -> ref (Enum_string { cases; row })
    | Union_int (tag, { cases; row }, int_bool) ->
        ref
          (Union_int
             (tag, { cases = Map.Int.map copy_union_case cases; row }, int_bool))
    | Union_string (tag, { cases; row }) ->
        ref
          (Union_string
             (tag, { cases = Map.String.map copy_union_case cases; row }))

  and copy_union_case r = ref (copy_record !r)
  and copy_record m = Map.String.map copy m

  let pp_sum_sep ppf () = F.fprintf ppf " |@ "

  let pp_row ppf = function
    | `Closed -> ()
    | `Open -> pp_sum_sep ppf (); Pp.ellipsis ppf ()

  let pp_sum ppf pp_case cases row =
    F.fprintf ppf "@[<hv 0>%a%a@]"
      (F.pp_print_seq ~pp_sep:pp_sum_sep pp_case)
      cases pp_row row

  let pp_field pp_k pp_v ppf tup = Pp.equation ~sep:":" pp_k pp_v ppf tup

  let rec pp ppf t =
    match !t with
    | Unknown _ -> F.pp_print_string ppf "_"
    | Int -> F.pp_print_string ppf "int"
    | Float -> F.pp_print_string ppf "float"
    | String -> F.pp_print_string ppf "string"
    | Nullable x -> F.fprintf ppf "?@[<hv 1>@,%a@]" pp x
    | List t -> Pp.surround ~left:'[' ~right:']' pp ppf t
    | Dict (t, _) -> Pp.surround ~left:'<' ~right:'>' pp ppf t
    | Record r -> pp_record ppf r
    | Tuple l ->
        Pp.surround ~left:'(' ~right:')'
          (F.pp_print_list ~pp_sep:Pp.comma pp)
          ppf l
    | Enum_string { cases; row; _ } ->
        pp_sum ppf (Pp.at Pp.syntax_string) (Set.String.to_seq cases) row
    | Enum_int ({ cases; row }, Not_bool) ->
        pp_sum ppf (Pp.at F.pp_print_int) (Set.Int.to_seq cases) row
    | Enum_int ({ cases; row }, Bool) ->
        pp_sum ppf Pp.bool (Set.Int.to_seq cases) row
    | Union_int (key, { cases; row }, Bool) ->
        pp_sum ppf (pp_union Pp.bool key) (Map.Int.to_seq cases) row
    | Union_int (key, { cases; row }, Not_bool) ->
        pp_sum ppf (pp_union F.pp_print_int key) (Map.Int.to_seq cases) row
    | Union_string (key, { cases; row }) ->
        pp_sum ppf (pp_union Pp.syntax_string key) (Map.String.to_seq cases) row

  and pp_record ?tag ppf m =
    Pp.surround ~left:'{' ~right:'}'
      (fun ppf s ->
        (match tag with
        | None -> ()
        | Some (pp_tag, tag) ->
            pp_field (Pp.at Pp.field) pp_tag ppf tag;
            if not (Seq.is_empty s) then Pp.comma ppf ());
        F.pp_print_seq ~pp_sep:Pp.comma (pp_field Pp.field pp) ppf s)
      ppf (Map.String.to_seq !m)

  and pp_union :
        'a.
        (F.formatter -> 'a -> unit) ->
        string ->
        F.formatter ->
        'a * record ->
        unit =
   fun pp_tag key ppf (tag, fields) ->
    pp_record ~tag:(pp_tag, (key, tag)) ppf fields

  module Polymap = struct
    (** The type-checking functions must run the same logic across branches
        which contain either int or string maps. This provides one way for us to
        execute map functions polymorphically. *)

    type ('k, 'v, 'm) t =
      | Int : (int, 'v, 'v Map.Int.t) t
      | String : (string, 'v, 'v Map.String.t) t

    let equal : type k v m. (k, v, m) t -> (v -> v -> bool) -> m -> m -> bool =
      function
      | Int -> Map.Int.equal
      | String -> Map.String.equal

    let for_all : type k v m. (k, v, m) t -> (k -> v -> bool) -> m -> bool =
      function
      | Int -> Map.Int.for_all
      | String -> Map.String.for_all

    let find_opt : type k v m. (k, v, m) t -> k -> m -> v option = function
      | Int -> Map.Int.find_opt
      | String -> Map.String.find_opt

    let iter : type k v m. (k, v, m) t -> (k -> v -> unit) -> m -> unit =
      function
      | Int -> Map.Int.iter
      | String -> Map.String.iter

    let union :
        type k v m. (k, v, m) t -> (k -> v -> v -> v option) -> m -> m -> m =
      function
      | Int -> Map.Int.union
      | String -> Map.String.union

    let merge :
        type k v m.
        (k, v, m) t -> (k -> v option -> v option -> v option) -> m -> m -> m =
      function
      | Int -> Map.Int.merge
      | String -> Map.String.merge
  end

  exception Clash

  (** When we unify a type during destructuring, we expand any structural types
      (records, enums).

      When we unify a type during constructing literal values, record types are
      narrowed (we use a subset of both) and enum behavior depends on its row
      property.

      When we unify a type during constructing with a variable, then we expand
      records and open enums in the variable's type. *)

  type unify_mode =
    | Destruct_expand  (** Record a and b both take on each other's fields. *)
    | Construct_literal  (** Record a is narrowed to a subset of a and b. *)
    | Construct_var  (** Record a expands into b only. *)

  let rec open_rows_bool_union_aux = function
    | None -> Some (ref Map.String.empty)
    | Some ty as x ->
        Map.String.iter (fun _ v -> open_rows v) !ty;
        x

  and open_rows ty =
    match !ty with
    | Enum_int (sum, Not_bool) -> sum.row <- `Open
    | Enum_string sum -> sum.row <- `Open
    | Enum_int (sum, Bool) -> sum.cases <- false_and_true_cases
    | Union_int (_, sum, Bool) ->
        sum.cases <-
          Map.Int.update 0 open_rows_bool_union_aux sum.cases
          |> Map.Int.update 1 open_rows_bool_union_aux
    | Union_int (_, sum, Not_bool) -> open_rows_union Polymap.Int sum
    | Union_string (_, sum) -> open_rows_union Polymap.String sum
    | Tuple a -> List.iter open_rows a
    | Record ty -> Map.String.iter (fun _ v -> open_rows v) !ty
    | Nullable ty | List ty | Dict (ty, _) -> open_rows ty
    | Unknown row -> row := `Open
    | Int | Float | String -> ()

  and open_rows_union : 'k 'm. ('k, 'v, 'm) Polymap.t -> 'm sum -> unit =
   fun poly sum ->
    Polymap.iter poly
      (fun _ v -> Map.String.iter (fun _ v -> open_rows v) !v)
      sum.cases;
    sum.row <- `Open

  let unify_enum (type s) (module M : Set.S with type t = s) _ (a : s) (b : s) :
      s =
    M.union a b

  let subset_enum (type s) (module M : Set.S with type t = s) _ (a : s) (b : s)
      =
    if not (M.subset b a) then raise Clash

  let unify_sum ~unify ~subset mode a b =
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
        | `Closed, `Open -> raise Clash)

  let rec unify mode a b =
    match (!a, !b) with
    | Int, Int | Float, Float | String, String -> ()
    | Nullable a, Nullable b | List a, List b -> unify mode a b
    | Tuple a, Tuple b -> (
        try List.iter2 (unify mode) a b with Invalid_argument _ -> raise Clash)
    | Record a, Record b -> unify_record mode a b
    | Dict (a, keys1), Dict (b, keys2) ->
        let ks' = Set.String.union !keys1 !keys2 in
        keys1 := ks';
        keys2 := ks';
        unify mode a b
    | Enum_int (a, Bool), Enum_int (b, Bool)
    | Enum_int (a, Not_bool), Enum_int (b, Not_bool) ->
        unify_sum
          ~unify:(unify_enum (module Set.Int))
          ~subset:(subset_enum (module Set.Int))
          mode a b
    | Enum_string a, Enum_string b ->
        unify_sum
          ~unify:(unify_enum (module Set.String))
          ~subset:(subset_enum (module Set.String))
          mode a b
    | Union_int (ka, a, Bool), Union_int (kb, b, Bool)
    | Union_int (ka, a, Not_bool), Union_int (kb, b, Not_bool)
      when String.equal ka kb ->
        unify_sum ~unify:(unify_union Polymap.Int)
          ~subset:(subset_union Polymap.Int) mode a b
    | Union_string (ka, a), Union_string (kb, b) when String.equal ka kb ->
        unify_sum
          ~unify:(unify_union Polymap.String)
          ~subset:(subset_union Polymap.String)
          mode a b
    | Unknown { contents = `Open }, t ->
        (match mode with Destruct_expand -> open_rows b | _ -> ());
        a := t
    | Unknown _, t -> a := t
    | t, Unknown _ -> b := t
    | _ -> raise Clash

  and unify_record mode a b =
    match mode with
    | Destruct_expand ->
        a :=
          Map.String.merge
            (fun _ a b ->
              match (a, b) with
              | (Some a as x), Some b -> unify mode a b; x
              | None, (Some b as x) -> open_rows b; x
              | x, None -> x)
            !a !b
    | Construct_var ->
        b := Map.String.union (fun _ a b -> unify mode a b; Some b) !a !b
    | Construct_literal ->
        a :=
          Map.String.merge
            (fun _ a b ->
              match (a, b) with
              | (Some a as x), Some b -> unify mode a b; x
              | Some _, None -> raise Clash
              | None, _ -> None)
            !a !b

  and unify_union :
        'k 'm. ('k, 'v, 'm) Polymap.t -> unify_mode -> 'm -> 'm -> 'm =
   fun poly mode a b ->
    Polymap.union poly (fun _ a b -> unify_record mode a b; Some a) a b

  and subset_union :
        'k 'm. ('k, 'v, 'm) Polymap.t -> unify_mode -> 'm -> 'm -> unit =
   fun poly mode a b ->
    Polymap.merge poly
      (fun _ a b ->
        match (a, b) with
        | (Some _ as x), None -> x
        | (Some a as x), Some b -> unify_record mode a b; x
        | None, Some _ -> raise Clash
        | None, None -> None)
      a b
    |> ignore

  let unify loc mode a b =
    try unify mode a b with Clash -> Error.type_mismatch loc pp a b

  let check_interface_enum (type s) (module M : Set.S with type t = s)
      ~(intf : s sum) ~(impl : s sum) =
    match (intf.row, impl.row) with
    | `Closed, `Closed -> M.equal intf.cases impl.cases
    | `Open, `Open -> M.subset impl.cases intf.cases
    | _ -> false

  let rec check_interface ~intf ~impl =
    match (!intf, !impl) with
    | Int, Int | Float, Float | String, String | _, Unknown _ -> true
    | Nullable a, Nullable b | List a, List b | Dict (a, _), Dict (b, _) ->
        check_interface ~intf:a ~impl:b
    | Tuple intf, Tuple impl ->
        List.equal (fun intf impl -> check_interface ~intf ~impl) intf impl
    | Record intf, Record impl -> check_interface_record ~intf ~impl
    | Enum_int (intf, Bool), Enum_int (impl, Bool)
    | Enum_int (intf, Not_bool), Enum_int (impl, Not_bool) ->
        check_interface_enum (module Set.Int) ~intf ~impl
    | Enum_string intf, Enum_string impl ->
        check_interface_enum (module Set.String) ~intf ~impl
    | Union_int (ka, intf, Bool), Union_int (kb, impl, Bool)
    | Union_int (ka, intf, Not_bool), Union_int (kb, impl, Not_bool) ->
        String.equal ka kb && check_interface_union Polymap.Int ~intf ~impl
    | Union_string (ka, intf), Union_string (kb, impl) ->
        String.equal ka kb && check_interface_union Polymap.String ~intf ~impl
    | _ -> false

  and check_interface_union :
        'k 'm. ('k, 'v, 'm) Polymap.t -> intf:'m sum -> impl:'m sum -> bool =
   fun poly ~intf ~impl ->
    match (intf.row, impl.row) with
    | `Closed, `Closed ->
        Polymap.equal poly
          (fun intf impl -> check_interface_record ~intf ~impl)
          intf.cases impl.cases
    | `Open, `Open ->
        Polymap.for_all poly
          (fun k impl ->
            match Polymap.find_opt poly k intf.cases with
            | Some intf -> check_interface_record ~intf ~impl
            | None -> false)
          impl.cases
    | _ -> false

  and check_interface_record ~intf ~impl =
    Map.String.for_all
      (fun k impl ->
        match Map.String.find_opt k !intf with
        | Some intf -> check_interface ~intf ~impl
        | None -> false)
      !impl

  (** Check for equality, but allow the interface to add additional
      fields to records and additional entries to open enums and unions. *)
  let check_interface loc ~intf ~impl =
    Map.String.iter
      (fun k impl ->
        match Map.String.find_opt k intf with
        | Some (loc, intf) ->
            if not (check_interface ~intf ~impl) then
              Error.interface_type_mismatch loc k pp intf impl
        | None -> Error.interface_missing_prop loc k pp impl)
      impl
end

type echo = [ `Var of string | `String of string | `Field of echo * string ]
type scalar = [ `Int of int | `Float of float | `String of string ]

type scalar_sum =
  | Scalar_sum_none
  | Scalar_sum_int of Set.Int.t Type.sum
  | Scalar_sum_string of Set.String.t Type.sum

type union_tag =
  | Union_tag_none
  | Union_tag_int of string * int * Type.sum_union_int
  | Union_tag_string of string * string * Type.sum_union_string

type construct = private Construct_tag
type destruct = private Destruct_tag

type _ pat =
  | Scalar : scalar * scalar_sum -> 'a pat
  | Nil : 'a pat
  | Cons : 'a pat -> 'a pat
  | Tuple : 'a pat list -> 'a pat
  | Record : union_tag * 'a pat Map.String.t * Type.record -> 'a pat
  | Dict : 'a pat Map.String.t * Set.String.t ref -> 'a pat
  | Var : string -> 'a pat
  | Block : nodes -> construct pat
  | Field : construct pat * string -> construct pat
  | Any : destruct pat

and node =
  | Text of string * Ast.trim * Ast.trim
  | Echo of (Ast.echo_format * echo) list * Ast.echo_format * echo * Ast.escape
  | Match of
      Loc.t * construct pat Nonempty.t * Type.t Nonempty.t * case Nonempty.t
  | Map_list of Loc.t * construct pat * Type.t Nonempty.t * case Nonempty.t
  | Map_dict of Loc.t * construct pat * Type.t Nonempty.t * case Nonempty.t
  | Component of string * construct pat Map.String.t

and case = {
  pats : (Loc.t * destruct pat Nonempty.t) Nonempty.t;
  nodes : nodes;
}

and nodes = node list

type t = { nodes : nodes; types : Type.t Map.String.t }

module Context = struct
  type used = Unused | Used

  type binding = {
    loc : Loc.t;
    name : string;
    mutable used : used;
    ty : Type.t;
  }

  type 'a t = {
    global : Type.t Map.String.t ref;
    scope : binding Map.String.t;
    all_bindings : binding Queue.t;
        (** This is for checking for unused variables. A queue preserves the
            order for friendlier debugging. *)
    interface : (Loc.t * Type.t) Map.String.t ref option ref;
    interface_loc : Loc.t ref;
        (** When there's more than one interface, this is for the last one. *)
  }
  (** We have to wrap each mutable value in a [ref] instead of using mutable
      fields because a new context record is created for each scope. *)

  let make () =
    {
      global = ref Map.String.empty;
      scope = Map.String.empty;
      all_bindings = Queue.create ();
      interface = ref None;
      interface_loc = ref Loc.dummy;
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
    | Some v' -> Type.unify loc Construct_var v v'

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
                      Type.unify loc Construct_literal a b;
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
    | "int" -> Type.int ()
    | "string" -> Type.string ()
    | "float" -> Type.float ()
    | "_" -> Type.unknown ()
    | s -> Error.interface_bad_name loc s

  let error_tag expected = function
    | Ast.Tag_int (loc, _) ->
        Error.type_mismatch loc Type.pp expected (Type.int ())
    | Ast.Tag_bool (loc, 0) ->
        Error.type_mismatch loc Type.pp expected (Type.enum_false_only ())
    | Ast.Tag_bool (loc, _) ->
        Error.type_mismatch loc Type.pp expected (Type.enum_true_only ())
    | Ast.Tag_string (loc, _) ->
        Error.type_mismatch loc Type.pp expected (Type.string ())

  let tag_int = function
    | Ast.Tag_int (_, i) -> i
    | t -> error_tag (Type.int ()) t

  let tag_string = function
    | Ast.Tag_string (_, s) -> s
    | t -> error_tag (Type.string ()) t

  let tag_bool = function
    | Ast.Tag_bool (_, i) -> i
    | t -> error_tag (Type.enum_false_and_true ()) t

  let rec make_ty = function
    | Ast.Ty_named (loc, s) -> named loc s
    | Ast.Ty_nullable t -> Type.nullable (make_ty t)
    | Ast.Ty_list t -> Type.list (make_ty t)
    | Ast.Ty_dict t -> Type.dict (make_ty t)
    | Ast.Ty_tuple l -> Type.tuple (List.map make_ty l)
    | Ast.Ty_enum_int (l, (_, r)) ->
        Type.enum_int (Type.sum (Nonempty.to_seq l |> Set.Int.of_seq) r)
    | Ast.Ty_enum_bool l ->
        Type.enum_bool (Nonempty.to_seq l |> Set.Int.of_seq |> Type.sum_bool)
    | Ast.Ty_enum_string (l, (_, r)) ->
        Type.enum_string (Type.sum (Nonempty.to_seq l |> Set.String.of_seq) r)
    | Ast.Ty_record ((loc, hd) :: tl, (row_l, row)) -> (
        match assoc_to_record hd with
        | Untagged m -> (
            match (tl, row) with
            | [], `Closed -> Map.String.map make_ty m |> ref |> Type.record
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
                Type.union_int tagk (Type.sum m row)
            | Tag_bool (_, i) -> (
                match row with
                | `Closed ->
                    Type.union_bool tagk
                      (Type.sum_bool
                         Map.Int.(singleton i (ref m) |> aux update tag_bool))
                | `Open -> Error.interface_open_bool_union row_l)
            | Tag_string (_, s) ->
                let m =
                  Map.String.(singleton s (ref m) |> aux update tag_string)
                in
                Type.union_string tagk (Type.sum m row)))

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
  | Ast.Fmt_string -> Type.string ()
  | Ast.Fmt_int -> Type.int ()
  | Ast.Fmt_float -> Type.float ()
  | Ast.Fmt_bool -> Type.enum_false_and_true ()

let[@tail_mod_cons] rec make_echo ctx ty = function
  | Ast.Echo_var (loc, var) ->
      Context.update ctx loc var ty;
      `Var var
  | Ast.Echo_field (var, field) ->
      let ty = Map.String.singleton field ty |> ref |> Type.record in
      `Field (make_echo ctx ty var, field)
  | Ast.Echo_string (loc, s) ->
      Type.unify loc Construct_literal ty (Type.string ());
      `String s

let[@tail_mod_cons] rec make_nullable_echoes ctx = function
  | [] -> []
  | (fmt, echo) :: tl ->
      let ty = make_echo_type fmt in
      let echo = make_echo ctx (Type.nullable ty) echo in
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
  | Type.Destruct_expand -> `Closed
  | Type.Construct_literal | Type.Construct_var -> `Open

(** This compliments the [mode] type with more information for [make_pat]. *)
type (_, _) var_action =
  | Destruct_add_vars :
      (Loc.t * string * Type.t) Queue.t
      -> (destruct, 'b) var_action
      (** When we destructure a pattern, we add all new variables to a queue. *)
  | Construct_update_vars : 'b Context.t -> (construct, 'b) var_action
      (** When we construct a pattern, we update the context for each new
          variable. *)

type _ Effect.t += Get_component_types : string -> Type.t Map.String.t Effect.t

(** When we type-check a pattern, we create a temporary type and unify it
    with the input type. Information retained in the resulting typed-pattern
    structure, for example dictionary keys or sum-type data, must come from the
    input type if possible, not the newly created one. The input type is the
    master copy which gets reused, and the temporary copy will become stale. *)

let rec make_pat :
    type a. (a, 'b) var_action -> Type.unify_mode -> Type.t -> Ast.pat -> a pat
    =
 fun var_action mode ty -> function
  | Int (loc, i) ->
      Type.unify loc mode ty (Type.int ());
      Scalar (`Int i, Scalar_sum_none)
  | String (loc, s) ->
      Type.unify loc mode ty (Type.string ());
      Scalar (`String s, Scalar_sum_none)
  | Block (loc, nodes) -> (
      Type.unify loc mode ty (Type.string ());
      match var_action with
      | Destruct_add_vars _ -> Error.bad_block loc
      | Construct_update_vars ctx -> Block (make_nodes ctx nodes))
  | Field (loc, rec_pat, field) -> (
      let rec_ty = Map.String.singleton field ty |> ref |> Type.record in
      let rec_pat = make_pat var_action mode rec_ty rec_pat in
      match var_action with
      | Destruct_add_vars _ -> Error.bad_field loc
      | Construct_update_vars _ -> Field (rec_pat, field))
  | Float (loc, f) ->
      Type.unify loc mode ty (Type.float ());
      Scalar (`Float f, Scalar_sum_none)
  | Bool (loc, b) ->
      let temp_sum =
        match var_action with
        | Destruct_add_vars _ -> Type.sum_enum_bool_singleton b
        | Construct_update_vars _ -> Type.sum_enum_false_and_true ()
      in
      let temp_ty = Type.enum_bool temp_sum in
      let sum = match !ty with Enum_int (s, _) -> s | _ -> temp_sum in
      Type.unify loc mode ty temp_ty;
      Scalar (`Int b, Scalar_sum_int sum)
  | Enum_string (loc, s) ->
      let temp_sum = Type.sum_enum_string_singleton s (make_row mode) in
      let temp_ty = Type.enum_string temp_sum in
      let sum = match !ty with Enum_string s -> s | _ -> temp_sum in
      Type.unify loc mode ty temp_ty;
      Scalar (`String s, Scalar_sum_string sum)
  | Enum_int (loc, i) ->
      let temp_sum = Type.sum_enum_int_singleton i (make_row mode) in
      let temp_ty = Type.enum_int temp_sum in
      let sum = match !ty with Enum_int (s, _) -> s | _ -> temp_sum in
      Type.unify loc mode ty temp_ty;
      Scalar (`Int i, Scalar_sum_int sum)
  | Nullable (loc, pat) ->
      let tyvar = match !ty with Nullable ty -> ty | _ -> Type.unknown () in
      let pat =
        match pat with
        | None -> Nil
        | Some pat -> Cons (Tuple [ make_pat var_action mode tyvar pat ])
      in
      Type.unify loc mode ty (Type.nullable tyvar);
      pat
  | List (loc, l, tl) ->
      let tyvar = match !ty with List ty -> ty | _ -> Type.unknown () in
      Type.unify loc mode ty (Type.list tyvar);
      let tl =
        match tl with None -> Nil | Some tl -> make_pat var_action mode ty tl
      in
      make_list ~tl var_action mode tyvar l
  | Tuple (loc, l) ->
      let temp_tyvars = List.map Type.unknown l in
      let tyvars = match !ty with Tuple tys -> tys | _ -> temp_tyvars in
      Type.unify loc mode ty (Type.tuple tyvars);
      Tuple (List.map2 (make_pat var_action mode) tyvars l)
  | Record (loc, r) -> (
      match assoc_to_record r with
      | Untagged m ->
          let temp_tyvars = Map.String.map Type.unknown m |> ref in
          let tyvars = match !ty with Record tys -> tys | _ -> temp_tyvars in
          Type.unify loc mode ty (Type.record temp_tyvars);
          let r = make_record var_action loc mode !tyvars m in
          Record (Union_tag_none, r, tyvars)
      | Tagged (k, v, m) -> (
          let row = make_row mode in
          let temp_tyvars = Map.String.map Type.unknown m |> ref in
          match v with
          | Tag_int (_, i) ->
              let temp_sum = Type.sum_union_int_singleton i temp_tyvars row in
              let sum =
                match !ty with Union_int (_, sum, _) -> sum | _ -> temp_sum
              in
              let tyvars =
                try Map.Int.find i sum.cases with Not_found -> temp_tyvars
              in
              Type.unify loc mode ty (Type.union_int k temp_sum);
              let r = make_record var_action loc mode !tyvars m in
              Record (Union_tag_int (k, i, sum), r, tyvars)
          | Tag_bool (_, i) ->
              let temp_sum = Type.sum_union_bool_singleton i temp_tyvars in
              let sum =
                match !ty with Union_int (_, sum, _) -> sum | _ -> temp_sum
              in
              let tyvars =
                try Map.Int.find i sum.cases with Not_found -> temp_tyvars
              in
              Type.unify loc mode ty (Type.union_bool k temp_sum);
              let r = make_record var_action loc mode !tyvars m in
              Record (Union_tag_int (k, i, sum), r, tyvars)
          | Tag_string (_, s) ->
              let temp_sum =
                Type.sum_union_string_singleton s temp_tyvars row
              in
              let sum =
                match !ty with Union_string (_, sum) -> sum | _ -> temp_sum
              in
              let tyvars =
                try Map.String.find s sum.cases with Not_found -> temp_tyvars
              in
              Type.unify loc mode ty (Type.union_string k temp_sum);
              let r = make_record var_action loc mode !tyvars m in
              Record (Union_tag_string (k, s, sum), r, tyvars)))
  | Dict (loc, m) ->
      let m = assoc_to_map m in
      let temp_kys =
        Map.String.to_seq m |> Seq.map fst |> Set.String.of_seq |> ref
      in
      let tyvar, kys =
        match !ty with
        | Dict (ty, ks) -> (ty, ks)
        | _ -> (Type.unknown (), temp_kys)
      in
      Type.unify loc mode ty (Type.dict_keys tyvar temp_kys);
      let d = Map.String.map (make_pat var_action mode tyvar) m in
      Dict (d, kys)
  | Var (loc, "_") -> (
      match var_action with
      | Destruct_add_vars _ -> Type.open_rows ty; Any
      | Construct_update_vars _ -> Error.underscore_in_construct loc)
  | Var (loc, b) ->
      (match var_action with
      | Destruct_add_vars queue ->
          Type.open_rows ty;
          Queue.add (loc, b, ty) queue
      | Construct_update_vars ctx -> Context.update ctx loc b ty);
      Var b

and[@tail_mod_cons] make_list :
    type a.
    tl:a pat ->
    (a, 'b) var_action ->
    Type.unify_mode ->
    Type.t ->
    Ast.pat list ->
    a pat =
 fun ~tl var_action mode ty -> function
  | [] -> tl
  | p :: l ->
      let hd = make_pat var_action mode ty p in
      Cons (Tuple [ hd; make_list ~tl var_action mode ty l ])

and make_record :
    type a.
    (a, 'b) var_action ->
    Loc.t ->
    Type.unify_mode ->
    Type.t Map.String.t ->
    Ast.pat Map.String.t ->
    a pat Map.String.t =
 fun var_action loc mode tyvars m ->
  match var_action with
  | Destruct_add_vars _ ->
      Map.String.merge
        (fun _ pat ty ->
          match (pat, ty) with
          | Some pat, None ->
              Some (make_pat var_action mode (Type.unknown ()) pat)
          | Some pat, Some ty -> Some (make_pat var_action mode ty pat)
          | None, Some ty -> Some (make_pat var_action mode ty Ast.dummy_var)
          | None, None -> None)
        m tyvars
  | Construct_update_vars _ ->
      Map.String.merge
        (fun k pat ty ->
          match (pat, ty) with
          | Some pat, Some ty -> Some (make_pat var_action mode ty pat)
          | None, Some ty -> Error.missing_field loc k Type.pp ty
          | _ -> None)
        m tyvars

and make_component_props loc name ctx tyvars m =
  Map.String.merge
    (fun k pat ty ->
      match (pat, ty) with
      | Some pat, Some ty ->
          Some (make_pat (Construct_update_vars ctx) Construct_literal ty pat)
      | None, Some ty -> Error.missing_field loc k Type.pp ty
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
        Type.unify loc Construct_literal (key ()) tl;
        ty hd
    | _ -> Error.map_pat_num_mismatch loc
  in
  let p = make_pat (Construct_update_vars ctx) Construct_literal hd_ty pat in
  (p, tys, cases)

and make_cases ctx cases =
  let tys =
    let _, pats = (Nonempty.hd cases).Ast.pats |> Nonempty.hd in
    Nonempty.map Type.unknown pats
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
      | Ast.Text (s, l, r) -> Some (Text (s, l, r))
      | Ast.Echo (nullables, fmt, default, esc) ->
          let nullables = make_nullable_echoes ctx nullables in
          let ty = make_echo_type fmt in
          let default = make_echo ctx ty default in
          Some (Echo (nullables, fmt, default, esc))
      | Ast.Component (loc, comp, comp', props) ->
          if comp <> comp' then Error.component_name_mismatch loc comp comp';
          let types = Effect.perform (Get_component_types comp) in
          (* The original should not mutate*)
          let types = Type.copy_record types in
          let missing_to_nullable _ ty prop =
            match (prop, ty) with
            | None, Some { contents = Type.Nullable _ } ->
                Some (Ast.Nullable (Loc.dummy, None))
            | prop, _ -> prop
          in
          let props =
            assoc_to_map props
            |> Map.String.merge missing_to_nullable types
            |> make_component_props loc comp ctx types
          in
          Some (Component (comp, props))
      | Ast.Match (loc, pats, cases) ->
          let tys, cases = make_cases ctx cases in
          let patterns =
            try unify_match_cases pats tys ctx
            with Invalid_argument _ -> Error.pat_num_mismatch loc
          in
          Some (Match (loc, patterns, tys, cases))
      | Ast.Map_list (loc, pattern, cases) ->
          let cases = add_default_wildcard cases |> make_cases ctx in
          let pattern, ty, cases =
            unify_map ~ty:Type.list ~key:Type.int loc cases pattern ctx
          in
          Some (Map_list (loc, pattern, ty, cases))
      | Ast.Map_dict (loc, pattern, cases) ->
          let cases = add_default_wildcard cases |> make_cases ctx in
          let pattern, ty, cases =
            unify_map ~ty:Type.dict ~key:Type.string loc cases pattern ctx
          in
          Some (Map_dict (loc, pattern, ty, cases))
      | Ast.Interface (loc, i) -> Interface.make loc ctx i; None
      | Comment _ -> None)
    nodes

let make ast =
  let ctx = Context.make () in
  let nodes = make_nodes ctx ast in
  Queue.iter
    (function
      | Context.{ used = Used; _ } -> ()
      | { used = Unused; loc; name; _ } -> (
          match name.[0] with '_' -> () | _ -> Error.var_unused loc name))
    ctx.all_bindings;
  match !(ctx.interface) with
  | None -> { nodes; types = !(ctx.global) }
  | Some intf ->
      Type.check_interface !(ctx.interface_loc) ~intf:!intf ~impl:!(ctx.global);
      { nodes; types = Map.String.map snd !intf }

(** Components need to form a directed acyclic graph. We use EXPERIMENTAL
    effects to manage the graph's state and and compile components on-demand by
    their dependents.

    Effects allow us to keep all of the graph's data and logic local inside its
    handler, and without needing to manually thread the state or callbacks
    through the rest of the typechecker. *)

type ('a, 'b) source =
  | Src of string * 'a
  | Fun of string * Type.t Map.String.t * 'b

type 'a graph = {
  not_linked : (Ast.t, 'a) source Map.String.t;
  linked : (t, 'a) source Map.String.t;
  stack : string list;
}

let make_components =
  let error stack name =
    if List.exists (String.equal name) stack then Error.cycle (name :: stack)
    else Error.missing_component stack name
  in
  let rec continue :
      type a.
      'f graph -> (a, t) Effect.Shallow.continuation -> a -> t * 'f graph =
   fun ({ linked; not_linked; stack } as graph) k v ->
    let retc ret = (ret, graph) in
    let exnc = raise in
    let rec effc :
        type b.
        b Effect.t ->
        ((b, t) Effect.Shallow.continuation -> t * 'f graph) option = function
      | Get_component_types key ->
          Some
            (fun k ->
              match Map.String.find_opt key linked with
              | Some (Src (_, { types; _ }) | Fun (_, types, _)) ->
                  continue graph k types
              | None -> (
                  match Map.String.find_opt key not_linked with
                  | Some (Src (key', ast)) ->
                      let not_linked = Map.String.remove key not_linked in
                      let typed, { not_linked; linked; _ } =
                        continue
                          { not_linked; linked; stack = key :: stack }
                          (Effect.Shallow.fiber make)
                          ast
                      in
                      let linked =
                        Map.String.add key (Src (key', typed)) linked
                      in
                      continue { not_linked; linked; stack } k typed.types
                  | Some (Fun (_, types, _) as f) ->
                      let not_linked = Map.String.remove key not_linked in
                      let linked = Map.String.add key f linked in
                      continue { not_linked; linked; stack } k types
                  | None ->
                      Effect.Shallow.discontinue_with k (error stack key)
                        { retc; exnc; effc }))
      | _ -> None
    in
    Effect.Shallow.continue_with k v { retc; exnc; effc }
  in
  let rec loop ~not_linked ~linked =
    match Map.String.choose_opt not_linked with
    | None -> linked
    | Some (key, Src (key', ast)) ->
        let not_linked = Map.String.remove key not_linked in
        let typed, { not_linked; linked; _ } =
          continue
            { not_linked; linked; stack = [ key ] }
            (Effect.Shallow.fiber make)
            ast
        in
        loop ~not_linked ~linked:(Map.String.add key (Src (key', typed)) linked)
    | Some (key, Fun (key', types, f)) ->
        loop
          ~not_linked:(Map.String.remove key not_linked)
          ~linked:(Map.String.add key (Fun (key', types, f)) linked)
  in
  fun not_linked -> loop ~not_linked ~linked:Map.String.empty

let make ~root components ast =
  let rec continue : type a. (a, t) Effect.Shallow.continuation -> a -> t =
    let retc = Fun.id in
    let exnc = raise in
    let rec effc :
        type b. b Effect.t -> ((b, t) Effect.Shallow.continuation -> t) option =
      function
      | Get_component_types key ->
          Some
            (fun k ->
              match Map.String.find_opt key components with
              | Some (Src (_, { types; _ }) | Fun (_, types, _)) ->
                  continue k types
              | None ->
                  Effect.Shallow.discontinue_with k
                    (Error.missing_component [ root ] key)
                    { retc; exnc; effc })
      | _ -> None
    in
    fun k v -> Effect.Shallow.continue_with k v { retc; exnc; effc }
  in
  continue (Effect.Shallow.fiber make) ast
