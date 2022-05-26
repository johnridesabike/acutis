(**************************************************************************)
(*                                                                        *)
(*                   Copyright (c) 2022 John Jackson.                     *)
(*                                                                        *)
(*  This Source Code Form is subject to the terms of the Mozilla Public   *)
(*  License, v. 2.0. If a copy of the MPL was not distributed with this   *)
(*  file, You can obtain one at http://mozilla.org/MPL/2.0/.              *)
(*                                                                        *)
(**************************************************************************)

open Utils
module F = Format
module Ty = Typescheme
module V = Ty.Variant

exception Type_error of string
exception Clash of string

(*
  When we unify a type during destructuring, we expand any structural types
  (records, enums).

  When we unify a type during constructing literal values, record types are
  narrowed (we use a subset of both) and enum behavior depends on its row
  property.

  When we unify a type during constructing with a variable, then we expand
  records and open enums in the variable's type.
*)

let rec open_rows_bool_union_aux = function
  | None -> Some (ref MapString.empty)
  | Some ty as x ->
      MapString.iter (fun _ v -> open_rows v) !ty;
      x

and open_rows ty =
  match !ty with
  | Ty.Enum ty -> (
      match ty.extra with
      | Extra_none -> ty.row <- `Open
      | Extra_bool -> ty.cases <- Ty.Enum.false_and_true_cases)
  | Union (_, ty) -> (
      match (ty.extra, ty.cases) with
      | Extra_bool, Int cases ->
          let f = open_rows_bool_union_aux in
          ty.cases <- Int (cases |> MapInt.update 0 f |> MapInt.update 1 f)
      | _, Int cases ->
          MapInt.iter
            (fun _ v -> MapString.iter (fun _ v -> open_rows v) !v)
            cases;
          ty.row <- `Open
      | _, String cases ->
          MapString.iter
            (fun _ v -> MapString.iter (fun _ v -> open_rows v) !v)
            cases;
          ty.row <- `Open)
  | Tuple a -> List.iter open_rows a
  | Record ty -> MapString.iter (fun _ v -> open_rows v) !ty
  | Nullable ty | List ty | Dict (ty, _) -> open_rows ty
  | Unknown row -> row := `Open
  | Int | Float | String | Echo -> ()

type mode =
  | Destructure_expand (* Record a and b both take on each other's fields. *)
  | Construct_literal (* Record a is narrowed to a subset of a and b. *)
  | Construct_var (* Record a expands into b only. *)

let show_mode = function
  | Destructure_expand -> "destructure"
  | Construct_literal -> "construct_literal"
  | Construct_var -> "construct_var"

let unify_enum_cases _ a b =
  match (a, b) with
  | V.String a, V.String b -> V.String (SetString.union a b)
  | Int a, Int b -> Int (SetInt.union a b)
  | _ -> raise (Clash "unify_enum_cases")

let subset_enum_cases _ a b =
  let success =
    match (a, b) with
    | V.String a, V.String b -> SetString.subset b a
    | Int a, Int b -> SetInt.subset b a
    | _ -> false
  in
  if not success then raise (Clash "subset_enum_cases")

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
      | `Closed, `Open -> raise (Clash "unify_variant"))

let rec unify mode aty bty =
  match (!aty, !bty) with
  | Ty.Int, Ty.Int | Float, Float | String, String | Echo, Echo -> ()
  | Echo, ((Int | Float | String | Enum _) as t) -> aty := t
  | ((Int | Float | String | Enum _) as t), Echo -> bty := t
  | Nullable t1, Nullable t2 -> unify mode t1 t2
  | List t1, List t2 -> unify mode t1 t2
  | Dict (t1, ks1), Dict (t2, ks2) ->
      let ks' = SetString.union !ks1 !ks2 in
      ks1 := ks';
      ks2 := ks';
      unify mode t1 t2
  | Tuple t1, Tuple t2 -> (
      try List.iter2 (unify mode) t1 t2
      with Invalid_argument _ -> raise (Type_error "tuple size"))
  | Record a, Record b -> unify_record mode a b
  | Enum a, Enum b ->
      unify_variant ~unify_cases:unify_enum_cases
        ~subset_cases:subset_enum_cases mode a b
  | Union (ka, a), Union (kb, b) when String.equal ka kb ->
      unify_variant ~unify_cases:unify_union_cases
        ~subset_cases:subset_union_cases mode a b
  | Unknown { contents = `Open }, t ->
      open_rows bty;
      aty := t
  | Unknown _, t -> aty := t
  | t, Unknown _ -> bty := t
  | _ -> raise (Clash "unify")

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
      a := MapString.merge f !a !b
  | Construct_var ->
      let f _ a b =
        match (a, b) with
        | Some a, (Some b as x) ->
            unify mode a b;
            x
        | (Some _ as x), None | None, (Some _ as x) -> x
        | None, None -> None
      in
      b := MapString.merge f !a !b
  | Construct_literal ->
      let f _ a b =
        match (a, b) with
        | (Some a as x), Some b ->
            unify mode a b;
            x
        | _ -> None
      in
      let r = MapString.merge f !a !b in
      if MapString.is_empty r then raise (Type_error "can't narrow") else a := r

and unify_union_cases mode a b =
  let f _ a b =
    match (a, b) with
    | (Some a as x), Some b ->
        unify_record mode a b;
        x
    | x, None | None, x -> x
  in
  match (a, b) with
  | V.String a, V.String b -> V.String (MapString.merge f a b)
  | Int a, Int b -> Int (MapInt.merge f a b)
  | _ -> raise (Clash "unify_union_cases")

and subset_union_cases mode a b =
  let f _ a b =
    match (a, b) with
    | (Some _ as x), None -> x
    | (Some a as x), Some b ->
        unify_record mode a b;
        x
    | None, Some _ -> raise (Clash "subset_union_cases")
    | None, None -> None
  in
  match (a, b) with
  | V.String a, V.String b -> MapString.merge f a b |> ignore
  | Int a, Int b -> MapInt.merge f a b |> ignore
  | _ -> raise (Clash "subset_union_cases")

let unify mode a b =
  try unify mode a b
  with Clash s ->
    raise (Type_error ("type mismatch " ^ show_mode mode ^ " " ^ s))

module Pattern = struct
  type constant = TString of string | TInt of int | TFloat of float
  [@@deriving eq]

  type construct = TList | TNullable [@@deriving eq, show]

  type t =
    | TConst of constant * Ty.Enum.t option
    | TConstruct of construct * t option
    | TTuple of t list
    | TRecord of
        (string * constant * Ty.ty Ty.Union.t) option
        * t MapString.t
        * Ty.ty MapString.t ref
    | TDict of t MapString.t * SetString.t ref
    | TVar of string
    | TOptionalVar of string
    | TAny
  [@@deriving eq]

  let make_enum_aux extra row tyvars = function
    | TInt i -> { V.cases = Int (MapInt.singleton i tyvars); row; extra }
    | TString s ->
        { V.cases = String (MapString.singleton s tyvars); row; extra }
    | _ -> raise (Type_error "bad union tag")

  let unknown _ = Ty.unknown ()

  let rec make ~f mode ty = function
    | Ast.Pattern.Int i ->
        unify mode ty (Ty.int ());
        TConst (TInt i, None)
    | String s ->
        unify mode ty (Ty.string ());
        TConst (TString s, None)
    | Float f ->
        unify mode ty (Ty.float ());
        TConst (TFloat f, None)
    | Bool b ->
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
        unify mode ty new_ty;
        TConst (TInt b, Some enum)
    | Enum_string s ->
        let new_enum =
          match mode with
          | Destructure_expand -> Ty.Enum.string_singleton s `Closed
          | Construct_var | Construct_literal ->
              Ty.Enum.string_singleton s `Open
        in
        let new_ty = ref (Ty.Enum new_enum) in
        let enum = match !ty with Enum e -> e | _ -> new_enum in
        unify mode ty new_ty;
        TConst (TString s, Some enum)
    | Enum_int i ->
        let new_enum =
          match mode with
          | Destructure_expand -> Ty.Enum.int_singleton i `Closed
          | Construct_var | Construct_literal -> Ty.Enum.int_singleton i `Open
        in
        let new_ty = ref (Ty.Enum new_enum) in
        let enum = match !ty with Enum e -> e | _ -> new_enum in
        unify mode ty new_ty;
        TConst (TInt i, Some enum)
    | Nullable pat ->
        let tyvar = match !ty with Nullable ty -> ty | _ -> Ty.unknown () in
        let pat =
          match pat with
          | None -> None
          | Some pat -> Some (TTuple [ make ~f mode tyvar pat ])
        in
        unify mode ty (Ty.nullable tyvar);
        TConstruct (TNullable, pat)
    | List (l, tl) ->
        let tyvar = match !ty with List ty -> ty | _ -> Ty.unknown () in
        unify mode ty (Ty.list tyvar);
        let tl =
          match tl with
          | None -> TConstruct (TList, None)
          | Some tl -> make ~f mode ty tl
        in
        make_list ~tl ~f mode tyvar l
    | Tuple l ->
        let new_tyvars = List.map unknown l in
        let tyvars = match !ty with Tuple tys -> tys | _ -> new_tyvars in
        unify mode ty (Ty.tuple tyvars);
        TTuple (List.map2 (make ~f mode) tyvars l)
    | Record (Untagged m) ->
        let m = Ast.Dict.to_map m in
        let new_tyvars = m |> MapString.map unknown |> ref in
        let tyvars = match !ty with Record tys -> tys | _ -> new_tyvars in
        unify mode ty (Ty.internal_record new_tyvars);
        let r = make_record ~f mode !tyvars m in
        TRecord (None, r, tyvars)
    | Record (Tagged (k, v, m)) ->
        let m = Ast.Dict.to_map m in
        let new_tyvars = m |> MapString.map unknown |> ref in
        let new_tag_ty = Ty.unknown () in
        let row =
          match mode with
          | Destructure_expand -> `Closed
          | Construct_literal | Construct_var -> `Open
        in
        let tag, tag_extra =
          match make ~f mode new_tag_ty v with
          | TConst (tag, None) -> (tag, V.Extra_none)
          | TConst (tag, Some { extra; _ }) -> (tag, extra) (* booleans *)
          | _ -> raise (Type_error "bad union tag")
        in
        let tyvars =
          match !ty with
          | Union (_, enum) -> (
              let tyvars =
                match (tag, enum) with
                | TInt i, { cases = Int cases; _ } -> MapInt.find_opt i cases
                | TString s, { cases = String cases; _ } ->
                    MapString.find_opt s cases
                | _ -> None
              in
              match tyvars with Some tv -> tv | None -> new_tyvars)
          | _ -> new_tyvars
        in
        let new_enum = make_enum_aux tag_extra row new_tyvars tag in
        unify mode ty (ref (Ty.Union (k, new_enum)));
        let r = make_record ~f mode !tyvars m in
        TRecord (Some (k, tag, new_enum), r, tyvars)
    | Dict m ->
        let new_kys = ref SetString.empty in
        let tyvar, kys =
          match !ty with
          | Dict (ty, ks) -> (ty, ks)
          | _ -> (Ty.unknown (), new_kys)
        in
        unify mode ty (Ty.internal_dict_keys tyvar new_kys);
        let d = m |> Ast.Dict.to_map |> MapString.map (make ~f mode tyvar) in
        TDict (d, kys)
    | Var "_" -> (
        match mode with
        | Destructure_expand ->
            open_rows ty;
            TAny
        | Construct_literal | Construct_var ->
            raise (Type_error "underscore in construct"))
    | Var b ->
        (match mode with
        | Destructure_expand -> open_rows ty
        | Construct_literal | Construct_var -> ());
        f b ty;
        TVar b

  and make_list ~f ~tl mode ty = function
    | [] -> tl
    | p :: l ->
        let hd = make ~f mode ty p in
        let tl = make_list ~f ~tl mode ty l in
        TConstruct (TList, Some (TTuple [ hd; tl ]))

  and make_record ~f mode tyvars m =
    match mode with
    | Destructure_expand ->
        let f _ pat ty =
          match (pat, ty) with
          | Some pat, None -> Some (make ~f mode (Ty.unknown ()) pat)
          | Some pat, Some ty -> Some (make ~f mode ty pat)
          | None, Some _ -> Some TAny
          | None, None -> None
        in
        MapString.merge f m tyvars
    | Construct_var | Construct_literal ->
        let f _ pat ty =
          match (pat, ty) with
          | Some pat, Some ty -> Some (make ~f mode ty pat)
          | None, Some _ -> raise (Type_error "missing record field")
          | Some _, None | None, None -> None
        in
        MapString.merge f m tyvars

  let to_list l =
    let rec aux acc = function
      | TTuple [ hd; TConstruct (_, Some tl) ] -> aux (hd :: acc) tl
      | TTuple [ hd; TConstruct (_, None) ] -> (List.rev (hd :: acc), None)
      | TTuple [ hd; tl ] -> (List.rev (hd :: acc), Some tl)
      | l -> (List.rev acc, Some l)
    in
    aux [] l

  let pp_constant ppf (x, e) =
    match (x, e) with
    | TInt 0, Some { V.extra = Extra_bool; _ } -> F.pp_print_string ppf "false"
    | TInt _, Some { V.extra = Extra_bool; _ } -> F.pp_print_string ppf "true"
    | TInt i, Some _ -> F.fprintf ppf "%@%i" i
    | TInt i, None -> F.fprintf ppf "%i" i
    | TString s, Some _ -> F.fprintf ppf "%@%S" s
    | TString s, None -> F.fprintf ppf "%S" s
    | TFloat f, _ -> F.pp_print_float ppf f

  let pp_sep_comma ppf () = F.fprintf ppf ",@ "

  let rec pp ppf = function
    | TConst (x, e) -> pp_constant ppf (x, e)
    | TTuple t ->
        F.fprintf ppf "(@[%a@])" (F.pp_print_list ~pp_sep:pp_sep_comma pp) t
    | TRecord (Some (k, tag, var), r, _) ->
        F.fprintf ppf "{@[%@%S: %a, %a@]}" k pp_constant (tag, Some var)
          pp_bindings r
    | TRecord (None, r, _) -> F.fprintf ppf "{@[%a@]}" pp_bindings r
    | TDict (m, _) -> F.fprintf ppf "<%a>" pp_bindings m
    | TVar v | TOptionalVar v -> F.pp_print_string ppf v
    | TConstruct (TNullable, None) -> F.pp_print_string ppf "null"
    | TConstruct (TNullable, Some x) -> F.fprintf ppf "!%a" pp x
    | TConstruct (TList, None) -> F.pp_print_string ppf "[]"
    | TConstruct (TList, Some l) ->
        let l, rest = to_list l in
        F.fprintf ppf "[@[%a%a@]]"
          (F.pp_print_list ~pp_sep:pp_sep_comma pp)
          l
          (F.pp_print_option pp_rest)
          rest
    | TAny -> F.pp_print_string ppf "_"

  and pp_key_values ppf (k, v) = F.fprintf ppf "%S: %a" k pp v

  and pp_bindings ppf m =
    F.pp_print_seq ~pp_sep:pp_sep_comma pp_key_values ppf (MapString.to_seq m)

  and pp_rest ppf t = F.fprintf ppf ",@ ...%a" pp t
end

type node =
  | TText of string * Ast.trim * Ast.trim
  | TEcho of Ast.echo list * Ast.echo
  | TMatch of Pattern.t Nonempty.t * case Nonempty.t
  | TMap_list of Pattern.t * case Nonempty.t
  | TMap_dict of Pattern.t * case Nonempty.t
  | TComponent of string * Pattern.t MapString.t * child MapString.t

and case = { pats : Pattern.t Nonempty.t Nonempty.t; nodes : nodes }
and child = TChild_name of string | TChild_block of nodes
and nodes = node list

type t = { nodes : nodes; prop_types : Ty.t; child_types : Ty.Child.t }

let unify_child a b =
  if Ty.Child.equal_ty a b then () else raise (Type_error "child type mismatch")

module Context = struct
  type root = [ `Root | `Component ]

  type t = {
    global : Ty.ty MapString.t ref;
    scope : Ty.ty MapString.t;
    children : Ty.Child.ty MapString.t ref;
    root : root;
  }

  let make root =
    {
      global = ref MapString.empty;
      scope = MapString.empty;
      children = ref MapString.empty;
      root;
    }

  let get k scope global =
    match MapString.find_opt k scope with
    | None -> MapString.find_opt k !global
    | Some _ as x -> x

  let update { scope; global; _ } k v =
    match get k scope global with
    | None -> global := MapString.add k v !global
    | Some v' -> unify Construct_var v v'

  let update_child { root; children; _ } (k, v) =
    match root with
    | `Root -> raise (Type_error "child not allowed in root")
    | `Component ->
        let f = function
          | None -> Some v
          | Some v' as r ->
              unify_child v v';
              r
        in
        children := MapString.update k f !children

  let add_scope bindings ctx =
    let bindings' = Queue.create () in
    Queue.iter
      (fun q ->
        Queue.add
          (Queue.fold
             (fun newscope (k, v) ->
               if MapString.mem k newscope then
                 raise (Type_error "name bound multiple")
               else MapString.add k v newscope)
             MapString.empty q)
          bindings')
      bindings;
    match Queue.take_opt bindings' with
    | None -> ctx
    | Some hd ->
        let newscope =
          Queue.fold
            (fun acc m ->
              MapString.merge
                (fun _ a b ->
                  match (a, b) with
                  | (Some a as a'), Some b ->
                      unify Construct_literal a b;
                      a'
                  | Some _, None | None, Some _ ->
                      raise (Type_error "var missing")
                  | None, None -> None)
                acc m)
            hd bindings'
        in
        let scope =
          MapString.merge
            (fun _ a b ->
              match (a, b) with
              | None, None -> None
              | Some x, None | _, Some x -> Some x)
            ctx.scope newscope
        in
        { ctx with scope }
end

let unify_match_cases pats tys ctx =
  try
    Nonempty.map2
      (Pattern.make ~f:(Context.update ctx) Construct_literal)
      tys pats
  with Invalid_argument _ -> raise (Type_error "pattern number mismatch")

let rec unify_echoes ctx default = function
  | [] -> (
      match default with
      | Ast.Ech_var (b, _) -> Context.update ctx b (Ty.echo ())
      | Ech_component c -> Context.update_child ctx (Ty.Child.child c)
      | Ech_string _ -> ())
  | Ast.Ech_var (b, _) :: tl ->
      Context.update ctx b (Ty.nullable (Ty.echo ()));
      unify_echoes ctx default tl
  | Ech_component c :: tl ->
      Context.update_child ctx (Ty.Child.nullable c);
      unify_echoes ctx default tl
  | Ech_string _ :: _ -> raise (Type_error "nullable echo literal")

let get_types = function
  | Source.Acutis (_, { prop_types; child_types; _ }) ->
      (prop_types, child_types)
  | Function (_, props, children, _) -> (props, children)

let add_default_wildcard cases =
  let f = function
    | Nonempty.[ h ] -> Nonempty.[ h; Ast.Pattern.Var "_" ]
    | pat -> pat
  in
  Nonempty.map
    (fun case -> { case with Ast.pats = Nonempty.map f case.Ast.pats })
    cases

let unify_map ~ty ~key (tys, cases) pat ctx =
  let hd_ty =
    match tys with
    | Nonempty.[ hd; tl ] ->
        unify Construct_literal (key ()) tl;
        ty hd
    | _ -> raise (Type_error "pattern number mismatch")
  in
  let p = Pattern.make ~f:(Context.update ctx) Construct_literal hd_ty pat in
  (p, cases)

let rec make_cases ctx g cases =
  let tys =
    (Nonempty.hd cases).Ast.pats |> Nonempty.hd |> Nonempty.map Pattern.unknown
  in
  (* Type-check all of the cases BEFORE running [make_nodes]. *)
  let cases =
    cases
    |> Nonempty.map (fun Ast.{ pats; nodes } ->
           let bindings_all = Queue.create () in
           let pats =
             Nonempty.map
               (fun pat ->
                 let bindings = Queue.create () in
                 Queue.add bindings bindings_all;
                 let f k v = Queue.add (k, v) bindings in
                 try Nonempty.map2 (Pattern.make ~f Destructure_expand) tys pat
                 with Invalid_argument _ ->
                   raise (Type_error "pattern number mismatch"))
               pats
           in
           let ctx = Context.add_scope bindings_all ctx in
           (pats, nodes, ctx))
    |> Nonempty.map (fun (pats, nodes, ctx) ->
           { pats; nodes = make_nodes ctx g nodes })
  in
  (tys, cases)

and make_nodes ctx g nodes =
  let f = function
    | Ast.Text (s, l, r) -> TText (s, l, r)
    | Echo (nullables, default) ->
        unify_echoes ctx default nullables;
        TEcho (nullables, default)
    | Component (comp, props, children) ->
        let prop_types, prop_types_child = get_types (DagMap.get comp g) in
        (* The original should not mutate*)
        let prop_types = Ty.internal_copy_record prop_types in
        let missing_to_nullable _ ty prop =
          match (prop, ty) with
          | None, Some { contents = Ty.Nullable _ } ->
              Some (Ast.Pattern.Nullable None)
          | prop, _ -> prop
        in
        let props =
          props |> Ast.Dict.to_map
          |> MapString.merge missing_to_nullable prop_types
          |> Pattern.make_record ~f:(Context.update ctx) Construct_literal
               prop_types
        in
        let f _ c ty =
          match (c, ty) with
          | None, Some ty ->
              if Ty.Child.is_nullable ty then None
              else raise (Type_error "missing child")
          | Some (Ast.Child_name c), Some ty ->
              Context.update_child ctx (c, ty);
              Some (TChild_name c)
          | Some (Child_block nodes), Some _ ->
              Some (TChild_block (make_nodes ctx g nodes))
          | Some _, None -> raise (Type_error "extra child")
          | None, None -> None
        in
        let children =
          MapString.merge f (Ast.Dict.to_map children) prop_types_child
        in
        TComponent (comp, props, children)
    | Match (bindings, cases) ->
        let tys, cases = make_cases ctx g cases in
        let patterns = unify_match_cases bindings tys ctx in
        TMatch (patterns, cases)
    | Map_list (pattern, cases) ->
        let cases = add_default_wildcard cases |> make_cases ctx g in
        let pattern, cases =
          unify_map ~ty:Ty.list ~key:Ty.int cases pattern ctx
        in
        TMap_list (pattern, cases)
    | Map_dict (pattern, cases) ->
        let cases = add_default_wildcard cases |> make_cases ctx g in
        let pattern, cases =
          unify_map ~ty:Ty.dict ~key:Ty.string cases pattern ctx
        in
        TMap_dict (pattern, cases)
  in
  List.map f nodes

let make root g ast =
  let ctx = Context.make root in
  let nodes = make_nodes ctx g ast in
  { nodes; prop_types = !(ctx.global); child_types = !(ctx.children) }

let make_src x g =
  match x with
  | Source.Acutis (name, ast) -> Source.src ~name (make `Component g ast)
  | Function (name, p, c, f) -> Source.fn ~name p c f

let make_components m = m |> DagMap.make ~f:make_src |> DagMap.link_all
let make components ast = make `Root (DagMap.prelinked components) ast
