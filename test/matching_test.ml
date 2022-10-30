open Acutis
module Ty = Typescheme
module MS = Map.String
module SI = Set.Int

let check = Alcotest.(check (module DebugMatching))

let get_tree_aux acc = function
  | Compile.Match (_, { tree; _ }) -> Some tree
  | _ -> acc

let nodes x = x.Compile.nodes

let get_tree src =
  Compile.(from_string ~fname:"<test>" Components.empty src)
  |> nodes
  |> List.fold_left get_tree_aux None
  |> Option.get

let e = Matching.Exit.unsafe_key
let map l = List.to_seq l |> MS.of_seq
let set = SI.of_list

let basic_tree () =
  let open Matching in
  let src = {|{% match a with !_a %} {% with null %} {% /match %}|} in
  check "Basic tree"
    (Construct
       {
         key = 0;
         ids = SI.empty;
         nil = Some (End { names = MS.empty; exit = e 1 });
         cons =
           Some
             (Nest
                {
                  key = 0;
                  ids = SI.empty;
                  debug = Not_dict;
                  child =
                    Int_keys
                      (Wildcard
                         {
                           key = 0;
                           ids = SI.of_list [ 0 ];
                           child =
                             End (End { names = map [ ("_a", 0) ]; exit = e 0 });
                         });
                  wildcard = None;
                });
       })
    (get_tree src);
  let src =
    {|{% match a
         with 0 with 10 with 20 with 30 %} {% with 15 %} {% with _ %}
      {% /match %}|}
  in
  check "Cases are sorted correctly"
    (Switch
       {
         key = 0;
         ids = SI.empty;
         debug_row = `Open;
         cases =
           {
             data = Int 0;
             if_match = End { names = MS.empty; exit = e 0 };
             next =
               Some
                 {
                   data = Int 10;
                   if_match = End { names = MS.empty; exit = e 0 };
                   next =
                     Some
                       {
                         data = Int 15;
                         if_match = End { names = MS.empty; exit = e 1 };
                         next =
                           Some
                             {
                               data = Int 20;
                               if_match = End { names = MS.empty; exit = e 0 };
                               next =
                                 Some
                                   {
                                     data = Int 30;
                                     if_match =
                                       End { names = MS.empty; exit = e 0 };
                                     next = None;
                                   };
                             };
                       };
                 };
           };
         wildcard = Some (End { names = MS.empty; exit = e 2 });
       })
    (get_tree src);
  let src =
    {|{% match a, b, c
       with 10, 11, 12 %}
    {% with  _x, 21, 22 %}
    {% with 30, 31, 32 %}
    {% with 30,  _y, 42 %}
    {% with  _a,  _b,  _c %}
    {% /match %}|}
  in
  let exit_0 = { names = MS.empty; exit = e 0 } in
  let exit_1 = { names = map [ ("_x", 0) ]; exit = e 1 } in
  let exit_2 = { names = MS.empty; exit = e 2 } in
  let exit_3 = { names = map [ ("_y", 1) ]; exit = e 3 } in
  let exit_4 =
    { exit = e 4; names = map [ ("_a", 2); ("_b", 3); ("_c", 4) ] }
  in
  let int_21 =
    {
      data = Int 21;
      if_match =
        Switch
          {
            key = 2;
            ids = set [ 4 ];
            debug_row = `Open;
            cases = { data = Int 22; if_match = End exit_1; next = None };
            wildcard = Some (End exit_4);
          };
      next = None;
    }
  in
  let int_11 =
    {
      data = Int 11;
      if_match =
        Switch
          {
            key = 2;
            ids = set [ 4 ];
            debug_row = `Open;
            cases = { data = Int 12; if_match = End exit_0; next = None };
            wildcard = Some (End exit_4);
          };
      next = Some int_21;
    }
  in
  let int_42 = { data = Int 42; if_match = End exit_3; next = None } in
  let int_31 =
    {
      data = Int 31;
      if_match =
        Switch
          {
            key = 2;
            ids = set [ 4 ];
            debug_row = `Open;
            cases = { data = Int 32; if_match = End exit_2; next = Some int_42 };
            wildcard = Some (End exit_4);
          };
      next = None;
    }
  in
  let wildcard_2 = Wildcard { key = 2; ids = set [ 4 ]; child = End exit_4 } in
  let int_30 =
    {
      data = Int 30;
      if_match =
        Switch
          {
            key = 1;
            ids = set [ 1; 3 ];
            debug_row = `Open;
            cases =
              {
                data = Int 21;
                if_match =
                  Switch
                    {
                      key = 2;
                      ids = set [ 4 ];
                      debug_row = `Open;
                      cases =
                        {
                          data = Int 22;
                          if_match = End exit_1;
                          next = Some int_42;
                        };
                      wildcard = Some (End exit_4);
                    };
                next = Some int_31;
              };
            wildcard =
              Some
                (Switch
                   {
                     key = 2;
                     ids = set [ 4 ];
                     debug_row = `Open;
                     cases = int_42;
                     wildcard = Some (End exit_4);
                   });
          };
      next = None;
    }
  in
  check "A basic decision tree works"
    (Switch
       {
         key = 0;
         ids = set [ 0; 2 ];
         debug_row = `Open;
         cases =
           {
             data = Int 10;
             if_match =
               Switch
                 {
                   key = 1;
                   ids = set [ 3 ];
                   debug_row = `Open;
                   cases = int_11;
                   wildcard = Some wildcard_2;
                 };
             next = Some int_30;
           };
         wildcard =
           Some
             (Switch
                {
                  key = 1;
                  ids = set [ 3 ];
                  debug_row = `Open;
                  cases = int_21;
                  wildcard = Some wildcard_2;
                });
       })
    (get_tree src)

let nests_merge () =
  let open Matching in
  let src =
    {|{% match a,        b,  c
         with  _,        _, 12 %}
      {% with  _, (20, 21), 22 %}
      {% with  _, (20, 21), 32 %}
      {% with  _, ( _,  _),  _ %}
      {% /match %}|}
  in
  let exit_0 = { names = MS.empty; exit = e 0 } in
  let exit_1 = { names = MS.empty; exit = e 1 } in
  let exit_2 = { names = MS.empty; exit = e 2 } in
  let exit_3 = { names = MS.empty; exit = e 3 } in
  let if_int_21 =
    Switch
      {
        key = 2;
        ids = SI.empty;
        debug_row = `Open;
        cases =
          {
            data = Int 12;
            if_match = End exit_0;
            next =
              Some
                {
                  data = Int 22;
                  if_match = End exit_1;
                  next =
                    Some { data = Int 32; if_match = End exit_2; next = None };
                };
          };
        wildcard = Some (End exit_3);
      }
  in
  let int_12_exit_0 =
    Matching.{ data = Int 12; if_match = End exit_0; next = None }
  in
  let int_12_exit_3 =
    Matching.(
      Switch
        {
          key = 2;
          ids = SI.empty;
          debug_row = `Open;
          cases = int_12_exit_0;
          wildcard = Some (End exit_3);
        })
  in
  check "Nests merge correctly"
    (Wildcard
       {
         key = 0;
         ids = SI.empty;
         child =
           Nest
             {
               key = 1;
               ids = SI.empty;
               debug = Not_dict;
               child =
                 Int_keys
                   (Switch
                      {
                        key = 0;
                        ids = SI.empty;
                        debug_row = `Open;
                        cases =
                          {
                            data = Int 20;
                            if_match =
                              Switch
                                {
                                  key = 1;
                                  ids = SI.empty;
                                  debug_row = `Open;
                                  cases =
                                    {
                                      data = Int 21;
                                      if_match = End if_int_21;
                                      next = None;
                                    };
                                  wildcard = Some (End int_12_exit_3);
                                };
                            next = None;
                          };
                        wildcard =
                          Some
                            (Wildcard
                               {
                                 key = 1;
                                 ids = SI.empty;
                                 child = End int_12_exit_3;
                               });
                      });
               wildcard =
                 Some
                   (Switch
                      {
                        key = 2;
                        ids = SI.empty;
                        debug_row = `Open;
                        cases = int_12_exit_0;
                        wildcard = None;
                      });
             };
       })
    (get_tree src)

let nests_merge_wildcards () =
  let open Matching in
  let src =
    {|{% match              a,  b
       with               _x, 41 %}
    {% with  ((10, 20), 30), 40 %}
    {% with               _y,  _z %}
    {% /match %}|}
  in
  let exit_0 = { names = map [ ("_x", 0) ]; exit = e 0 } in
  let exit_1 = { names = MS.empty; exit = e 1 } in
  let exit_2 = { names = map [ ("_y", 1); ("_z", 2) ]; exit = e 2 } in
  let int_40 =
    Switch
      {
        key = 1;
        ids = set [ 2 ];
        debug_row = `Open;
        cases =
          {
            data = Int 40;
            if_match = End exit_1;
            next = Some { data = Int 41; if_match = End exit_0; next = None };
          };
        wildcard = Some (End exit_2);
      }
  in
  let int_30 =
    Switch
      {
        key = 1;
        ids = SI.empty;
        debug_row = `Open;
        cases = { data = Int 30; if_match = End int_40; next = None };
        wildcard = None;
      }
  in
  check "Wildcards merge after nests correctly"
    (Nest
       {
         key = 0;
         ids = set [ 0; 1 ];
         debug = Not_dict;
         child =
           Int_keys
             (Nest
                {
                  key = 0;
                  ids = SI.empty;
                  debug = Not_dict;
                  child =
                    Int_keys
                      (Switch
                         {
                           key = 0;
                           ids = SI.empty;
                           debug_row = `Open;
                           cases =
                             {
                               data = Int 10;
                               if_match =
                                 Switch
                                   {
                                     key = 1;
                                     ids = SI.empty;
                                     debug_row = `Open;
                                     cases =
                                       {
                                         data = Int 20;
                                         if_match = End int_30;
                                         next = None;
                                       };
                                     wildcard = None;
                                   };
                               next = None;
                             };
                           wildcard = None;
                         });
                  wildcard = None;
                });
         wildcard =
           Some
             (Switch
                {
                  key = 1;
                  ids = set [ 2 ];
                  debug_row = `Open;
                  cases = { data = Int 41; if_match = End exit_0; next = None };
                  wildcard = Some (End exit_2);
                });
       })
    (get_tree src)

let lists () =
  let open Matching in
  let src =
    {|{% match a
       with [] %}
    {% with [_x] %}
    {% with [_x, ..._y] %}
    {% /match %}|}
  in
  let exit_0 = { names = MS.empty; exit = e 0 } in
  let exit_1 = { names = map [ ("_x", 0) ]; exit = e 1 } in
  let exit_2 = { names = map [ ("_x", 1); ("_y", 2) ]; exit = e 2 } in
  check "Different-sized lists merge correctly"
    (Construct
       {
         key = 0;
         ids = SI.empty;
         nil = Some (End exit_0);
         cons =
           Some
             (Nest
                {
                  key = 0;
                  ids = SI.empty;
                  debug = Not_dict;
                  child =
                    Int_keys
                      (Wildcard
                         {
                           key = 0;
                           ids = set [ 0; 1 ];
                           child =
                             Construct
                               {
                                 key = 1;
                                 ids = set [ 2 ];
                                 nil = Some (End (End exit_1));
                                 cons =
                                   Some
                                     (Wildcard
                                        {
                                          key = 1;
                                          ids = set [ 2 ];
                                          child = End (End exit_2);
                                        });
                               };
                         });
                  wildcard = None;
                });
       })
    (get_tree src);
  let src =
    {|{% match a, b
       with  [10, 11], 12 %}
    {% with  [10, 11, ..._x], 22 %}
    {% with  [30], 32 %}
    {% with  _y, 42 %}
    {% with  _, _ %}
    {% /match %}|}
  in
  let exit_0 = { names = MS.empty; exit = e 0 } in
  let exit_1 = { names = map [ ("_x", 0) ]; exit = e 1 } in
  let exit_2 = { names = MS.empty; exit = e 2 } in
  let exit_3 = { names = map [ ("_y", 1) ]; exit = e 3 } in
  let exit_4 = { names = MS.empty; exit = e 4 } in
  let int_42 = { data = Int 42; if_match = End exit_3; next = None } in
  let int_22 = { data = Int 22; if_match = End exit_1; next = Some int_42 } in
  let int_11 =
    {
      data = Int 11;
      if_match =
        Construct
          {
            key = 1;
            ids = set [ 0 ];
            nil =
              Some
                (End
                   (End
                      (Switch
                         {
                           key = 1;
                           ids = SI.empty;
                           debug_row = `Open;
                           cases =
                             {
                               data = Int 12;
                               if_match = End exit_0;
                               next = Some int_22;
                             };
                           wildcard = Some (End exit_4);
                         })));
            cons =
              Some
                (Wildcard
                   {
                     key = 1;
                     ids = set [ 0 ];
                     child =
                       End
                         (End
                            (Switch
                               {
                                 key = 1;
                                 ids = SI.empty;
                                 debug_row = `Open;
                                 cases = int_22;
                                 wildcard = Some (End exit_4);
                               }));
                   });
          };
      next = None;
    }
  in
  let int_10 =
    {
      data = Int 10;
      if_match =
        Construct
          {
            key = 1;
            ids = SI.empty;
            cons =
              Some
                (Nest
                   {
                     key = 1;
                     ids = SI.empty;
                     debug = Not_dict;
                     child =
                       Int_keys
                         (Switch
                            {
                              key = 0;
                              ids = SI.empty;
                              debug_row = `Open;
                              cases = int_11;
                              wildcard = None;
                            });
                     wildcard = None;
                   });
            nil = None;
          };
      next =
        Some
          {
            data = Int 30;
            if_match =
              Construct
                {
                  key = 1;
                  ids = SI.empty;
                  nil =
                    Some
                      (End
                         (Switch
                            {
                              key = 1;
                              ids = SI.empty;
                              debug_row = `Open;
                              cases =
                                {
                                  data = Int 32;
                                  if_match = End exit_2;
                                  next = Some int_42;
                                };
                              wildcard = Some (End exit_4);
                            }));
                  cons = None;
                };
            next = None;
          };
    }
  in
  check "A big list pattern works"
    (Construct
       {
         key = 0;
         ids = set [ 1 ];
         cons =
           Some
             (Nest
                {
                  key = 0;
                  ids = set [ 1 ];
                  debug = Not_dict;
                  child =
                    Int_keys
                      (Switch
                         {
                           key = 0;
                           ids = SI.empty;
                           debug_row = `Open;
                           cases = int_10;
                           wildcard = None;
                         });
                  wildcard =
                    Some
                      (Switch
                         {
                           key = 1;
                           ids = SI.empty;
                           debug_row = `Open;
                           cases = int_42;
                           wildcard = Some (End exit_4);
                         });
                });
         nil =
           Some
             (Switch
                {
                  key = 1;
                  ids = SI.empty;
                  debug_row = `Open;
                  cases = int_42;
                  wildcard = Some (End exit_4);
                });
       })
    (get_tree src)

let wildcards_constructs () =
  let open Matching in
  let src =
    {|
    {% match a,  b,   c
        with 1,  _,   0 %} 0
    {%  with _, !1,   0 %} 1
    {%  with _, null, 0 %} 2
    {%  with 1, !1,   1 %} 3
    {%  with _, _,    _ %} 4
    {% /match %}|}
  in
  let exit_0 = { names = MS.empty; exit = e 0 } in
  let exit_1 = { names = MS.empty; exit = e 1 } in
  let exit_2 = { names = MS.empty; exit = e 2 } in
  let exit_3 = { names = MS.empty; exit = e 3 } in
  let exit_4 = { names = MS.empty; exit = e 4 } in
  check
    "Constructs can merge into wildcards correctly (nil path fails to merge)."
    (Switch
       {
         key = 0;
         ids = SI.empty;
         cases =
           {
             data = Int 1;
             if_match =
               Construct
                 {
                   key = 1;
                   ids = SI.empty;
                   nil =
                     Some
                       (Switch
                          {
                            key = 2;
                            ids = SI.empty;
                            cases =
                              {
                                data = Int 0;
                                if_match = End exit_0;
                                next = None;
                              };
                            wildcard = Some (End exit_4);
                            debug_row = `Open;
                          });
                   cons =
                     Some
                       (Nest
                          {
                            key = 1;
                            ids = SI.empty;
                            child =
                              Int_keys
                                (Switch
                                   {
                                     key = 0;
                                     ids = SI.empty;
                                     cases =
                                       {
                                         data = Int 1;
                                         if_match =
                                           End
                                             (Switch
                                                {
                                                  key = 2;
                                                  ids = SI.empty;
                                                  cases =
                                                    {
                                                      data = Int 0;
                                                      if_match = End exit_0;
                                                      next =
                                                        Some
                                                          {
                                                            data = Int 1;
                                                            if_match =
                                                              End exit_3;
                                                            next = None;
                                                          };
                                                    };
                                                  wildcard = Some (End exit_4);
                                                  debug_row = `Open;
                                                });
                                         next = None;
                                       };
                                     wildcard = None;
                                     debug_row = `Open;
                                   });
                            wildcard =
                              Some
                                (Switch
                                   {
                                     key = 2;
                                     ids = SI.empty;
                                     cases =
                                       {
                                         data = Int 0;
                                         if_match = End exit_0;
                                         next = None;
                                       };
                                     wildcard = Some (End exit_4);
                                     debug_row = `Open;
                                   });
                            debug = Not_dict;
                          });
                 };
             next = None;
           };
         wildcard =
           Some
             (Construct
                {
                  key = 1;
                  ids = SI.empty;
                  nil =
                    Some
                      (Switch
                         {
                           key = 2;
                           ids = SI.empty;
                           cases =
                             {
                               data = Int 0;
                               if_match = End exit_2;
                               next = None;
                             };
                           wildcard = Some (End exit_4);
                           debug_row = `Open;
                         });
                  cons =
                    Some
                      (Nest
                         {
                           key = 1;
                           ids = SI.empty;
                           child =
                             Int_keys
                               (Switch
                                  {
                                    key = 0;
                                    ids = SI.empty;
                                    cases =
                                      {
                                        data = Int 1;
                                        if_match =
                                          End
                                            (Switch
                                               {
                                                 key = 2;
                                                 ids = SI.empty;
                                                 cases =
                                                   {
                                                     data = Int 0;
                                                     if_match = End exit_1;
                                                     next = None;
                                                   };
                                                 wildcard = Some (End exit_4);
                                                 debug_row = `Open;
                                               });
                                        next = None;
                                      };
                                    wildcard = None;
                                    debug_row = `Open;
                                  });
                           wildcard =
                             Some
                               (Wildcard
                                  {
                                    key = 2;
                                    ids = SI.empty;
                                    child = End exit_4;
                                  });
                           debug = Not_dict;
                         });
                });
         debug_row = `Open;
       })
    (get_tree src);
  let src =
    {|
    {% match a,  b,   c
        with 1,  _,   0 %} 0
    {%  with _, !1,   0 %} 1
    {%  with _, null, 0 %} 2
    {%  with 1, null, 1 %} 3
    {%  with _, _,    _ %} 4
    {% /match %}|}
  in
  let exit_0 = { names = MS.empty; exit = e 0 } in
  let exit_2 = { names = MS.empty; exit = e 2 } in
  let exit_3 = { names = MS.empty; exit = e 3 } in
  let exit_4 = { names = MS.empty; exit = e 4 } in
  let exit_1 =
    Switch
      {
        key = 2;
        ids = SI.empty;
        cases =
          {
            data = Int 0;
            if_match = End { names = MS.empty; exit = e 1 };
            next = None;
          };
        wildcard = Some (End exit_4);
        debug_row = `Open;
      }
  in
  check
    "Constructs can merge into wildcards correctly (cons path fails to merge)."
    (Switch
       {
         key = 0;
         ids = SI.empty;
         cases =
           {
             data = Int 1;
             if_match =
               Construct
                 {
                   key = 1;
                   ids = SI.empty;
                   nil =
                     Some
                       (Switch
                          {
                            key = 2;
                            ids = SI.empty;
                            cases =
                              {
                                data = Int 0;
                                if_match = End exit_0;
                                next =
                                  Some
                                    {
                                      data = Int 1;
                                      if_match = End exit_3;
                                      next = None;
                                    };
                              };
                            wildcard = Some (End exit_4);
                            debug_row = `Open;
                          });
                   cons =
                     Some
                       (Wildcard
                          {
                            key = 1;
                            ids = SI.empty;
                            child =
                              Switch
                                {
                                  key = 2;
                                  ids = SI.empty;
                                  cases =
                                    {
                                      data = Int 0;
                                      if_match = End exit_0;
                                      next = None;
                                    };
                                  wildcard = Some (End exit_4);
                                  debug_row = `Open;
                                };
                          });
                 };
             next = None;
           };
         wildcard =
           Some
             (Construct
                {
                  key = 1;
                  ids = SI.empty;
                  nil =
                    Some
                      (Switch
                         {
                           key = 2;
                           ids = SI.empty;
                           cases =
                             {
                               data = Int 0;
                               if_match = End exit_2;
                               next = None;
                             };
                           wildcard = Some (End exit_4);
                           debug_row = `Open;
                         });
                  cons =
                    Some
                      (Nest
                         {
                           key = 1;
                           ids = SI.empty;
                           child =
                             Int_keys
                               (Switch
                                  {
                                    key = 0;
                                    ids = SI.empty;
                                    cases =
                                      {
                                        data = Int 1;
                                        if_match = End exit_1;
                                        next = None;
                                      };
                                    wildcard = None;
                                    debug_row = `Open;
                                  });
                           wildcard =
                             Some
                               (Wildcard
                                  {
                                    key = 2;
                                    ids = SI.empty;
                                    child = End exit_4;
                                  });
                           debug = Not_dict;
                         });
                });
         debug_row = `Open;
       })
    (get_tree src);
  let src =
    {|
   {% match a, b
       with 1, null %} 0
   {%  with _, !"a" %} 1
   {%  with _, null %} 2
   {%  with 1, _ %}    3
   {%  with _, _ %}    4
   {% /match %}|}
  in
  let exit_0 = { names = MS.empty; exit = e 0 } in
  let exit_1 =
    {
      data = String "a";
      if_match = End (End { names = MS.empty; exit = e 1 });
      next = None;
    }
  in
  let exit_2 = { names = MS.empty; exit = e 2 } in
  let exit_3 = { names = MS.empty; exit = e 3 } in
  let exit_4 = { names = MS.empty; exit = e 4 } in
  check
    "Constructs can merge into wildcards correctly (both paths fail to merge)."
    (Switch
       {
         key = 0;
         ids = SI.empty;
         cases =
           {
             data = Int 1;
             if_match =
               Construct
                 {
                   key = 1;
                   ids = SI.empty;
                   nil = Some (End exit_0);
                   cons =
                     Some
                       (Nest
                          {
                            key = 1;
                            ids = SI.empty;
                            child =
                              Int_keys
                                (Switch
                                   {
                                     key = 0;
                                     ids = SI.empty;
                                     cases = exit_1;
                                     wildcard = None;
                                     debug_row = `Open;
                                   });
                            wildcard = Some (End exit_3);
                            debug = Not_dict;
                          });
                 };
             next = None;
           };
         wildcard =
           Some
             (Construct
                {
                  key = 1;
                  ids = SI.empty;
                  nil = Some (End exit_2);
                  cons =
                    Some
                      (Nest
                         {
                           key = 1;
                           ids = SI.empty;
                           child =
                             Int_keys
                               (Switch
                                  {
                                    key = 0;
                                    ids = SI.empty;
                                    cases = exit_1;
                                    wildcard = None;
                                    debug_row = `Open;
                                  });
                           wildcard = Some (End exit_4);
                           debug = Not_dict;
                         });
                });
         debug_row = `Open;
       })
    (get_tree src)

let records_sort () =
  let open Matching in
  let src =
    {|
    {% match a, b
       with {a: 10, b: 11}, 12 %}
    {% with {b: 21, a: 20}, 22 %}
    {% with _, _ %}
    {% /match %}|}
  in
  let exit_0 = { names = MS.empty; exit = e 0 } in
  let exit_1 = { names = MS.empty; exit = e 1 } in
  let exit_2 = { names = MS.empty; exit = e 2 } in
  check "Record fields sort correctly"
    (Nest
       {
         key = 0;
         ids = SI.empty;
         debug = Not_dict;
         child =
           String_keys
             (Switch
                {
                  key = "a";
                  ids = SI.empty;
                  debug_row = `Open;
                  cases =
                    {
                      data = Int 10;
                      if_match =
                        Switch
                          {
                            key = "b";
                            ids = SI.empty;
                            debug_row = `Open;
                            cases =
                              {
                                data = Int 11;
                                if_match =
                                  End
                                    (Switch
                                       {
                                         key = 1;
                                         ids = SI.empty;
                                         debug_row = `Open;
                                         cases =
                                           {
                                             data = Int 12;
                                             if_match = End exit_0;
                                             next = None;
                                           };
                                         wildcard = Some (End exit_2);
                                       });
                                next = None;
                              };
                            wildcard = None;
                          };
                      next =
                        Some
                          {
                            data = Int 20;
                            if_match =
                              Switch
                                {
                                  key = "b";
                                  ids = SI.empty;
                                  debug_row = `Open;
                                  cases =
                                    {
                                      data = Int 21;
                                      if_match =
                                        End
                                          (Switch
                                             {
                                               key = 1;
                                               ids = SI.empty;
                                               debug_row = `Open;
                                               cases =
                                                 {
                                                   data = Int 22;
                                                   if_match = End exit_1;
                                                   next = None;
                                                 };
                                               wildcard = Some (End exit_2);
                                             });
                                      next = None;
                                    };
                                  wildcard = None;
                                };
                            next = None;
                          };
                    };
                  wildcard = None;
                });
         wildcard =
           Some (Wildcard { key = 1; ids = SI.empty; child = End exit_2 });
       })
    (get_tree src)

let records_expand () =
  let open Matching in
  let src =
    {|
    {% match a
       with {b: 10} %}
    {% with {a: 20} %}
    {% with {c: 30} %}
    {% with _x %}
    {% /match %}|}
  in
  let exit_0 = { names = MS.empty; exit = e 0 } in
  let exit_1 = { names = MS.empty; exit = e 1 } in
  let exit_2 = { names = MS.empty; exit = e 2 } in
  let exit_3 = { names = map [ ("_x", 0) ]; exit = e 3 } in
  check "New fields expand existing rows"
    (Nest
       {
         key = 0;
         ids = set [ 0 ];
         debug = Not_dict;
         child =
           String_keys
             (Switch
                {
                  key = "a";
                  ids = SI.empty;
                  debug_row = `Open;
                  cases =
                    {
                      data = Int 20;
                      if_match =
                        Switch
                          {
                            key = "b";
                            ids = SI.empty;
                            debug_row = `Open;
                            cases =
                              {
                                data = Int 10;
                                if_match =
                                  Wildcard
                                    {
                                      key = "c";
                                      ids = SI.empty;
                                      child = End (End exit_0);
                                    };
                                next = None;
                              };
                            wildcard =
                              Some
                                (Wildcard
                                   {
                                     key = "c";
                                     ids = SI.empty;
                                     child = End (End exit_1);
                                   });
                          };
                      next = None;
                    };
                  wildcard =
                    Some
                      (Switch
                         {
                           key = "b";
                           ids = SI.empty;
                           debug_row = `Open;
                           cases =
                             {
                               data = Int 10;
                               if_match =
                                 Wildcard
                                   {
                                     key = "c";
                                     ids = SI.empty;
                                     child = End (End exit_0);
                                   };
                               next = None;
                             };
                           wildcard =
                             Some
                               (Switch
                                  {
                                    key = "c";
                                    ids = SI.empty;
                                    debug_row = `Open;
                                    cases =
                                      {
                                        data = Int 30;
                                        if_match = End (End exit_2);
                                        next = None;
                                      };
                                    wildcard = None;
                                  });
                         });
                });
         wildcard = Some (End exit_3);
       })
    (get_tree src)

let () =
  let open Alcotest in
  run "Matching"
    [
      ("Basic trees", [ test_case "Basic" `Quick basic_tree ]);
      ( "Nested trees",
        [
          test_case "Basic nested" `Quick nests_merge;
          test_case "Wildcards merge with nests" `Quick nests_merge_wildcards;
          test_case "Lists" `Quick lists;
        ] );
      ( "Merging expanded trees",
        [ test_case "Wildcards and constructs" `Quick wildcards_constructs ] );
      ( "Records",
        [
          test_case "Fields sort" `Quick records_sort;
          test_case "Fields expand" `Quick records_expand;
        ] );
    ]
