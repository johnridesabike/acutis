open Acutis
module Ty = Typescheme
module MS = Map.String
module SI = Set.Int

let parse = Compile.parse_string
let pp_tree = Matching.pp_tree Matching.pp_leaf Format.pp_print_int
let equal_tree = Matching.equal_tree Matching.equal_leaf Int.equal
let check = Alcotest.(check (testable pp_tree equal_tree))

let make_nodes ast =
  Typechecker.make ~root:"<test>" MS.empty ast |> Compile.make_nodes

let get_tree_aux acc = function
  | Compile.Match (_, { tree; _ }) -> Some tree
  | _ -> acc

let get_tree nodes = List.fold_left get_tree_aux None nodes |> Option.get
let get_tree src = parse ~filename:"" src |> make_nodes |> get_tree
let e = Matching.Exit.unsafe_key
let map l = l |> List.to_seq |> MS.of_seq
let set = SI.of_list

let basic_tree () =
  let open Matching in
  let src = {|{% match a with !a %} {% with null %} {% /match %}|} in
  check "Basic tree"
    (Construct
       {
         key = 0;
         ids = SI.empty;
         debug = TNullable;
         nil = Some (End { names = MS.empty; exit = e 1 });
         cons =
           Some
             (Nest
                {
                  key = 0;
                  ids = SI.empty;
                  debug = Tuple;
                  child =
                    Int_keys
                      (Wildcard
                         {
                           key = 0;
                           ids = SI.of_list [ 0 ];
                           child =
                             End (End { names = map [ ("a", 0) ]; exit = e 0 });
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
         row = `Open;
         cases =
           {
             data = `Int 0;
             if_match = End { names = MS.empty; exit = e 0 };
             next_case =
               Some
                 {
                   data = `Int 10;
                   if_match = End { names = MS.empty; exit = e 0 };
                   next_case =
                     Some
                       {
                         data = `Int 15;
                         if_match = End { names = MS.empty; exit = e 1 };
                         next_case =
                           Some
                             {
                               data = `Int 20;
                               if_match = End { names = MS.empty; exit = e 0 };
                               next_case =
                                 Some
                                   {
                                     data = `Int 30;
                                     if_match =
                                       End { names = MS.empty; exit = e 0 };
                                     next_case = None;
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
    {% with  x, 21, 22 %}
    {% with 30, 31, 32 %}
    {% with 30,  y, 42 %}
    {% with  a,  b,  c %}
    {% /match %}|}
  in
  let exit_0 = { names = MS.empty; exit = e 0 } in
  let exit_1 = { names = map [ ("x", 0) ]; exit = e 1 } in
  let exit_2 = { names = MS.empty; exit = e 2 } in
  let exit_3 = { names = map [ ("y", 1) ]; exit = e 3 } in
  let exit_4 = { exit = e 4; names = map [ ("a", 2); ("b", 3); ("c", 4) ] } in
  let int_21 =
    {
      data = `Int 21;
      if_match =
        Switch
          {
            key = 2;
            ids = set [ 4 ];
            row = `Open;
            cases = { data = `Int 22; if_match = End exit_1; next_case = None };
            wildcard = Some (End exit_4);
          };
      next_case = None;
    }
  in
  let int_11 =
    {
      data = `Int 11;
      if_match =
        Switch
          {
            key = 2;
            ids = set [ 4 ];
            row = `Open;
            cases = { data = `Int 12; if_match = End exit_0; next_case = None };
            wildcard = Some (End exit_4);
          };
      next_case = Some int_21;
    }
  in
  let int_42 = { data = `Int 42; if_match = End exit_3; next_case = None } in
  let int_31 =
    {
      data = `Int 31;
      if_match =
        Switch
          {
            key = 2;
            ids = set [ 4 ];
            row = `Open;
            cases =
              { data = `Int 32; if_match = End exit_2; next_case = Some int_42 };
            wildcard = Some (End exit_4);
          };
      next_case = None;
    }
  in
  let wildcard_2 = Wildcard { key = 2; ids = set [ 4 ]; child = End exit_4 } in
  let int_30 =
    {
      data = `Int 30;
      if_match =
        Switch
          {
            key = 1;
            ids = set [ 1; 3 ];
            row = `Open;
            cases =
              {
                data = `Int 21;
                if_match =
                  Switch
                    {
                      key = 2;
                      ids = set [ 4 ];
                      row = `Open;
                      cases =
                        {
                          data = `Int 22;
                          if_match = End exit_1;
                          next_case = Some int_42;
                        };
                      wildcard = Some (End exit_4);
                    };
                next_case = Some int_31;
              };
            wildcard =
              Some
                (Switch
                   {
                     key = 2;
                     ids = set [ 4 ];
                     row = `Open;
                     cases = int_42;
                     wildcard = Some (End exit_4);
                   });
          };
      next_case = None;
    }
  in
  check "A basic decision tree works"
    (Switch
       {
         key = 0;
         ids = set [ 0; 2 ];
         row = `Open;
         cases =
           {
             data = `Int 10;
             if_match =
               Switch
                 {
                   key = 1;
                   ids = set [ 3 ];
                   row = `Open;
                   cases = int_11;
                   wildcard = Some wildcard_2;
                 };
             next_case = Some int_30;
           };
         wildcard =
           Some
             (Switch
                {
                  key = 1;
                  ids = set [ 3 ];
                  row = `Open;
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
        row = `Open;
        cases =
          {
            data = `Int 12;
            if_match = End exit_0;
            next_case =
              Some
                {
                  data = `Int 22;
                  if_match = End exit_1;
                  next_case =
                    Some
                      {
                        data = `Int 32;
                        if_match = End exit_2;
                        next_case = None;
                      };
                };
          };
        wildcard = Some (End exit_3);
      }
  in
  let int_12_exit_0 =
    Matching.{ data = `Int 12; if_match = End exit_0; next_case = None }
  in
  let int_12_exit_3 =
    Matching.(
      Switch
        {
          key = 2;
          ids = SI.empty;
          row = `Open;
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
               debug = Tuple;
               child =
                 Int_keys
                   (Switch
                      {
                        key = 0;
                        ids = SI.empty;
                        row = `Open;
                        cases =
                          {
                            data = `Int 20;
                            if_match =
                              Switch
                                {
                                  key = 1;
                                  ids = SI.empty;
                                  row = `Open;
                                  cases =
                                    {
                                      data = `Int 21;
                                      if_match = End if_int_21;
                                      next_case = None;
                                    };
                                  wildcard = Some (End int_12_exit_3);
                                };
                            next_case = None;
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
                        row = `Open;
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
       with               x, 41 %}
    {% with  ((10, 20), 30), 40 %}
    {% with               y,  z %}
    {% /match %}|}
  in
  let exit_0 = { names = map [ ("x", 0) ]; exit = e 0 } in
  let exit_1 = { names = MS.empty; exit = e 1 } in
  let exit_2 = { names = map [ ("y", 1); ("z", 2) ]; exit = e 2 } in
  let int_40 =
    Switch
      {
        key = 1;
        ids = set [ 2 ];
        row = `Open;
        cases =
          {
            data = `Int 40;
            if_match = End exit_1;
            next_case =
              Some { data = `Int 41; if_match = End exit_0; next_case = None };
          };
        wildcard = Some (End exit_2);
      }
  in
  let int_30 =
    Switch
      {
        key = 1;
        ids = SI.empty;
        row = `Open;
        cases = { data = `Int 30; if_match = End int_40; next_case = None };
        wildcard = None;
      }
  in
  check "Wildcards merge after nests correctly"
    (Nest
       {
         key = 0;
         ids = set [ 0; 1 ];
         debug = Tuple;
         child =
           Int_keys
             (Nest
                {
                  key = 0;
                  ids = SI.empty;
                  debug = Tuple;
                  child =
                    Int_keys
                      (Switch
                         {
                           key = 0;
                           ids = SI.empty;
                           row = `Open;
                           cases =
                             {
                               data = `Int 10;
                               if_match =
                                 Switch
                                   {
                                     key = 1;
                                     ids = SI.empty;
                                     row = `Open;
                                     cases =
                                       {
                                         data = `Int 20;
                                         if_match = End int_30;
                                         next_case = None;
                                       };
                                     wildcard = None;
                                   };
                               next_case = None;
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
                  row = `Open;
                  cases =
                    { data = `Int 41; if_match = End exit_0; next_case = None };
                  wildcard = Some (End exit_2);
                });
       })
    (get_tree src)

let lists () =
  let open Matching in
  let src =
    {|{% match a
       with [] %}
    {% with [x] %}
    {% with [x, ...y] %}
    {% /match %}|}
  in
  let exit_0 = { names = MS.empty; exit = e 0 } in
  let exit_1 = { names = map [ ("x", 0) ]; exit = e 1 } in
  let exit_2 = { names = map [ ("x", 1); ("y", 2) ]; exit = e 2 } in
  check "Different-sized lists merge correctly"
    (Construct
       {
         key = 0;
         ids = SI.empty;
         debug = TList;
         nil = Some (End exit_0);
         cons =
           Some
             (Nest
                {
                  key = 0;
                  ids = SI.empty;
                  debug = Tuple;
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
                                 debug = TList;
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
    {% with  [10, 11, ...x], 22 %}
    {% with  [30], 32 %}
    {% with  y, 42 %}
    {% with  _, _ %}
    {% /match %}|}
  in
  let exit_0 = { names = MS.empty; exit = e 0 } in
  let exit_1 = { names = map [ ("x", 0) ]; exit = e 1 } in
  let exit_2 = { names = MS.empty; exit = e 2 } in
  let exit_3 = { names = map [ ("y", 1) ]; exit = e 3 } in
  let exit_4 = { names = MS.empty; exit = e 4 } in
  let int_42 = { data = `Int 42; if_match = End exit_3; next_case = None } in
  let int_22 =
    { data = `Int 22; if_match = End exit_1; next_case = Some int_42 }
  in
  let int_11 =
    {
      data = `Int 11;
      if_match =
        Construct
          {
            key = 1;
            ids = set [ 0 ];
            debug = TList;
            nil =
              Some
                (End
                   (End
                      (Switch
                         {
                           key = 1;
                           ids = SI.empty;
                           row = `Open;
                           cases =
                             {
                               data = `Int 12;
                               if_match = End exit_0;
                               next_case = Some int_22;
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
                                 row = `Open;
                                 cases = int_22;
                                 wildcard = Some (End exit_4);
                               }));
                   });
          };
      next_case = None;
    }
  in
  let int_10 =
    {
      data = `Int 10;
      if_match =
        Construct
          {
            key = 1;
            ids = SI.empty;
            debug = TList;
            cons =
              Some
                (Nest
                   {
                     key = 1;
                     ids = SI.empty;
                     debug = Tuple;
                     child =
                       Int_keys
                         (Switch
                            {
                              key = 0;
                              ids = SI.empty;
                              row = `Open;
                              cases = int_11;
                              wildcard = None;
                            });
                     wildcard = None;
                   });
            nil = None;
          };
      next_case =
        Some
          {
            data = `Int 30;
            if_match =
              Construct
                {
                  key = 1;
                  ids = SI.empty;
                  debug = TList;
                  nil =
                    Some
                      (End
                         (Switch
                            {
                              key = 1;
                              ids = SI.empty;
                              row = `Open;
                              cases =
                                {
                                  data = `Int 32;
                                  if_match = End exit_2;
                                  next_case = Some int_42;
                                };
                              wildcard = Some (End exit_4);
                            }));
                  cons = None;
                };
            next_case = None;
          };
    }
  in
  check "A big list pattern works"
    (Construct
       {
         key = 0;
         ids = set [ 1 ];
         debug = TList;
         cons =
           Some
             (Nest
                {
                  key = 0;
                  ids = set [ 1 ];
                  debug = Tuple;
                  child =
                    Int_keys
                      (Switch
                         {
                           key = 0;
                           ids = SI.empty;
                           row = `Open;
                           cases = int_10;
                           wildcard = None;
                         });
                  wildcard =
                    Some
                      (Switch
                         {
                           key = 1;
                           ids = SI.empty;
                           row = `Open;
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
                  row = `Open;
                  cases = int_42;
                  wildcard = Some (End exit_4);
                });
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
         debug = Record;
         child =
           String_keys
             (Switch
                {
                  key = "a";
                  ids = SI.empty;
                  row = `Open;
                  cases =
                    {
                      data = `Int 10;
                      if_match =
                        Switch
                          {
                            key = "b";
                            ids = SI.empty;
                            row = `Open;
                            cases =
                              {
                                data = `Int 11;
                                if_match =
                                  End
                                    (Switch
                                       {
                                         key = 1;
                                         ids = SI.empty;
                                         row = `Open;
                                         cases =
                                           {
                                             data = `Int 12;
                                             if_match = End exit_0;
                                             next_case = None;
                                           };
                                         wildcard = Some (End exit_2);
                                       });
                                next_case = None;
                              };
                            wildcard = None;
                          };
                      next_case =
                        Some
                          {
                            data = `Int 20;
                            if_match =
                              Switch
                                {
                                  key = "b";
                                  ids = SI.empty;
                                  row = `Open;
                                  cases =
                                    {
                                      data = `Int 21;
                                      if_match =
                                        End
                                          (Switch
                                             {
                                               key = 1;
                                               ids = SI.empty;
                                               row = `Open;
                                               cases =
                                                 {
                                                   data = `Int 22;
                                                   if_match = End exit_1;
                                                   next_case = None;
                                                 };
                                               wildcard = Some (End exit_2);
                                             });
                                      next_case = None;
                                    };
                                  wildcard = None;
                                };
                            next_case = None;
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
    {% with x %}
    {% /match %}|}
  in
  let exit_0 = { names = MS.empty; exit = e 0 } in
  let exit_1 = { names = MS.empty; exit = e 1 } in
  let exit_2 = { names = MS.empty; exit = e 2 } in
  let exit_3 = { names = map [ ("x", 0) ]; exit = e 3 } in
  check "New fields expand existing rows"
    (Nest
       {
         key = 0;
         ids = set [ 0 ];
         debug = Record;
         child =
           String_keys
             (Switch
                {
                  key = "a";
                  ids = SI.empty;
                  row = `Open;
                  cases =
                    {
                      data = `Int 20;
                      if_match =
                        Switch
                          {
                            key = "b";
                            ids = SI.empty;
                            row = `Open;
                            cases =
                              {
                                data = `Int 10;
                                if_match =
                                  Wildcard
                                    {
                                      key = "c";
                                      ids = SI.empty;
                                      child = End (End exit_0);
                                    };
                                next_case = None;
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
                      next_case = None;
                    };
                  wildcard =
                    Some
                      (Switch
                         {
                           key = "b";
                           ids = SI.empty;
                           row = `Open;
                           cases =
                             {
                               data = `Int 10;
                               if_match =
                                 Wildcard
                                   {
                                     key = "c";
                                     ids = SI.empty;
                                     child = End (End exit_0);
                                   };
                               next_case = None;
                             };
                           wildcard =
                             Some
                               (Switch
                                  {
                                    key = "c";
                                    ids = SI.empty;
                                    row = `Open;
                                    cases =
                                      {
                                        data = `Int 30;
                                        if_match = End (End exit_2);
                                        next_case = None;
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
      ( "Records",
        [
          test_case "Fields sort" `Quick records_sort;
          test_case "Fields expand" `Quick records_expand;
        ] );
    ]