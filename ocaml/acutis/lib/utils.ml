module MapString = Map.Make (String)
module MapInt = Map.Make (Int)
module SetString = Set.Make (String)
module SetInt = Set.Make (Int)

module DagMap = struct
  type ('a, 'b) t = {
    queue : string list;
    mutable notlinked : 'a MapString.t;
    mutable linked : 'b MapString.t;
    stack : string list;
    f : 'a -> ('a, 'b) t -> 'b;
  }

  let id a _ = a
  let key (k, _) = k

  let make ~f m =
    {
      queue = MapString.bindings m |> List.map key;
      notlinked = m;
      linked = MapString.empty;
      stack = [];
      f;
    }

  let prelinked m =
    { queue = []; notlinked = MapString.empty; linked = m; stack = []; f = id }

  let get k g =
    match MapString.find_opt k g.linked with
    | Some x -> x (* It was linked already in a previous search. *)
    | None -> (
        match MapString.find_opt k g.notlinked with
        | Some x ->
            (* Remove it form the unlinked map so a cycle isn't possible. *)
            g.notlinked <- MapString.remove k g.notlinked;
            let x = g.f x { g with stack = k :: g.stack } in
            g.linked <- MapString.add k x g.linked;
            x
        | None ->
            (* It is either being linked (in a cycle) or it doesn't exist. *)
            if List.exists (String.equal k) g.stack then failwith "cycle"
            else failwith "missing")

  let link_all g =
    let f k =
      match MapString.find_opt k g.notlinked with
      | Some x ->
          g.notlinked <- MapString.remove k g.notlinked;
          g.linked <-
            MapString.add k (g.f x { g with stack = k :: g.stack }) g.linked
      | None -> () (* It was already processed by a dependent. *)
    in
    List.iter f g.queue;
    g.linked
end
