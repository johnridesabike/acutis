open Acutis
module F = Format
module Ty = Typescheme

let parse src = Compile.parse (Lexing.from_string src)

let check =
  Alcotest.(
    check
      (testable
         (Pp.map_string Typescheme.pp)
         (Map.String.equal Typescheme.equal)))

let get_types src =
  (parse ~name:"<test>" src |> Typechecker.make ~root:"<test>" Map.String.empty)
    .prop_types

let echoes () =
  let src = {|{{ a }} {{ "b" }} {{ c ? "d" }}|} in
  check "Echoes typecheck correctly"
    Ty.(make [ ("a", echo ()); ("c", nullable (echo ())) ])
    (get_types src)

let nullables () =
  let src =
    {|
  {% match a with !1 %} {% with null %} {% with !_ %} {% /match %}
  {% match b with !!"b" %} {% with null %} {% with !_ %} {% /match %}|}
  in
  check "Nullables typecheck correctly"
    Ty.(
      make [ ("a", nullable (int ())); ("b", nullable (nullable (string ()))) ])
    (get_types src)

let nested () =
  let src =
    {|
    {% match a with {b: {c}, d } %}
      {% match c with !1 %} {{ d }} {% with null %} {% with !_ %} {% /match %}
    {% /match %}|}
  in
  check "Nested patterns typecheck correctly"
    Ty.(
      make
        [
          ( "a",
            record
              [ ("b", record [ ("c", nullable (int ())) ]); ("d", echo ()) ] );
        ])
    (get_types src);
  let src =
    {|
    {% match a, b with {c}, 1 %}
      {% match c with !{d: 1} %} {% with null %} {% with !_ %} {% /match %}
    {% with _, _ %}
    {% /match %}|}
  in
  check "Multiple patterns typecheck correctly"
    Ty.(
      make
        [
          ("a", record [ ("c", nullable (record [ ("d", int ()) ])) ]);
          ("b", int ());
        ])
    (get_types src)

let constructing () =
  let src =
    {|
      {% match x with {a, b} %} {{ a }} {{ b }} {% /match %}
      {% match y with {c} %} {{ c }} {% /match %}
      {% map [{a: "a", b: 1}, {a: "a", c: true}, x, y, z] with {a} %}
        {{ a }}
      {% /map %}|}
  in
  check "Type narrowing works"
    Ty.(
      make
        [
          ("x", record [ ("a", string ()); ("b", echo ()) ]);
          ("y", record [ ("a", string ()); ("c", echo ()) ]);
          ("z", record [ ("a", string ()) ]);
        ])
    (get_types src);
  let src =
    {|{% match a with [{a: 1}, c, {b: "b"}] %} {% with _ %} {% /match %}|}
  in
  check "Inferrence works for nested types (1)"
    Ty.(make [ ("a", list (record [ ("a", int ()); ("b", string ()) ])) ])
    (get_types src);
  let src =
    {|{% match a with [{@tag: 0, a: 1}, c, {@tag: 1, b: "b"}] %}
      {% with _ %} {% /match %}|}
  in
  check "Inferrence works for nested types (2)"
    Ty.(
      make
        [
          ( "a",
            list
              (union_int `Open "tag"
                 [ (0, [ ("a", int ()) ]); (1, [ ("b", string ()) ]) ]) );
        ])
    (get_types src)

let open_enums () =
  let src =
    {|
    {% match a with @"a" %} {% with @"b" %} {% with _ %} {% /match %}
    {% map [@"c", a] with _ %} {% /map %}
    {% match b with @1 %} {% with @2 %} {% with _ %} {% /match %}
    {% map [@3, b] with _ %} {% /map %}|}
  in
  check "Open enums are inferred correctly"
    Ty.(
      make
        [
          ("a", enum_string `Open [ "a"; "b"; "c" ]);
          ("b", enum_int `Open [ 1; 2; 3 ]);
        ])
    (get_types src);
  let src =
    {|
    {% match a with <k: @"a"> %} {% with <k: @"b"> %} {% with _ %} {% /match %}
    {% map [<k: @"c">, a] with _ %} {% /map %}|}
  in
  check "Wildcards will open enums nested inside other types"
    Ty.(make [ ("a", dict (enum_string `Open [ "a"; "b"; "c" ])) ])
    (get_types src);
  let src =
    {|
    {% match a with {a: 1} %}{% with {b: @1} %}{% with {a: _} %}{% /match %}|}
  in
  check "The row is opened for variants in newly inferred record fields"
    Ty.(make [ ("a", record [ ("a", int ()); ("b", enum_int `Open [ 1 ]) ]) ])
    (get_types src)

let closed_enums () =
  let src =
    {|
    {% match a with @"a" %} {% with @"b" %} {% /match %}
    {% match a with @"a" %} {% with _ %} {% /match %}
    {% match b with @1 %} {% with @2 %} {% /match %}
    {% match b with @3 %} {% with _ %} {% /match %}|}
  in
  check "Closed enums are inferred corerctly"
    Ty.(
      make
        [
          ("a", enum_string `Closed [ "a"; "b" ]);
          ("b", enum_int `Closed [ 1; 2 ]);
        ])
    (get_types src);
  let src =
    {|
    {% match a with @"a" %} {% with @"b" %} {% /match %}
    {% map [@"c", @"d", a, b] with _ %} {% /map %}
    {% match c with @1 %} {% with @2 %} {% /match %}
    {% map [@3, @4, c, d] with _ %} {% /map %}
    {% match e with {a: @"a"} %} {% with {a: @"b"} %} {% /match %}
    {% match f with {a: @1} %} {% with {a: @2} %} {% /match %}
  |}
  in
  check "Closed enums are inferred correctly (nested in constructs)"
    Ty.(
      make
        [
          ("a", enum_string `Closed [ "a"; "b" ]);
          ("b", enum_string `Open [ "a"; "b"; "c"; "d" ]);
          ("c", enum_int `Closed [ 1; 2 ]);
          ("d", enum_int `Open [ 1; 2; 3; 4 ]);
          ("e", record [ ("a", enum_string `Closed [ "a"; "b" ]) ]);
          ("f", record [ ("a", enum_int `Closed [ 1; 2 ]) ]);
        ])
    (get_types src);
  let src =
    {|
    {% map [@"a", a] with @"a" %} {% with @"b" %} {% /map %}
    {% map [@1, b] with @1 %} {% with @2 %} {% /map %}
    {% match c with {a: @"a"} %} {% with {a: @"b"} %} {% /match %}
    {% map [{a: @"c"}, c, d] with _ %} {% /map %}
    {% match e with {a: @1} %} {% with {a: @2} %} {% /match %}
    {% map [{a: @3}, e, f] with _ %} {% /map %}|}
  in
  check "Closed enums are inferred correctly (created nested in constructs)"
    Ty.(
      make
        [
          ("a", enum_string `Closed [ "a"; "b" ]);
          ("b", enum_int `Closed [ 1; 2 ]);
          ("c", record [ ("a", enum_string `Closed [ "a"; "b" ]) ]);
          ("d", record [ ("a", enum_string `Open [ "a"; "b"; "c" ]) ]);
          ("e", record [ ("a", enum_int `Closed [ 1; 2 ]) ]);
          ("f", record [ ("a", enum_int `Open [ 1; 2; 3 ]) ]);
        ])
    (get_types src)

let boolean_enums () =
  let src =
    {|
    {% match a with true %} {% /match %}
    {% match b with false %} {% /match %}
    {% match c with true %} {% with false %} {% /match %}
    {% match d with true %} {% with _ %} {% /match %}|}
  in
  check "Booleans are inferred correctly"
    Ty.(
      make
        [
          ("a", true_only ());
          ("b", false_only ());
          ("c", boolean ());
          ("d", boolean ());
        ])
    (get_types src)

let open_unions () =
  let src =
    {|
    {% match a
       with {@tag: "a", b} %} {{ b }}
    {% with {@tag: "b", b: 1} %}
    {% with _ %} {% /match %}
    {% match b
       with {@tag: 0, b} %} {{ b }}
    {% with {@tag: 1, b: 1} %}
    {% with _ %} {% /match %}
    {% match c
       with {@tag: "a", b} %} {{ b }}
    {% with {@tag: "b", b: 1} %}
    {% with _ %} {% /match %}
    {% map [{@tag: "c", b: 1.5}, c] with _ %} {% /map %}
  |}
  in
  check "Open tagged unions are inferred correctly"
    Ty.(
      make
        [
          ( "a",
            union_string `Open "tag"
              [ ("a", [ ("b", echo ()) ]); ("b", [ ("b", int ()) ]) ] );
          ( "b",
            union_int `Open "tag"
              [ (0, [ ("b", echo ()) ]); (1, [ ("b", int ()) ]) ] );
          ( "c",
            union_string `Open "tag"
              [
                ("a", [ ("b", echo ()) ]);
                ("b", [ ("b", int ()) ]);
                ("c", [ ("b", float ()) ]);
              ] );
        ])
    (get_types src);
  let src =
    {|
    {% match a
       with <k: {@tag: "a"}> %}
    {% with <k: {@tag: "b"}> %}
    {% with _ %} {% /match %}
    {% map [<k: {@tag: "c"}>, a] with _ %} {% /map %}|}
  in
  check "Wildcards open nested tagged unions"
    Ty.(
      make
        [
          ( "a",
            dict (union_string `Open "tag" [ ("a", []); ("b", []); ("c", []) ])
          );
        ])
    (get_types src)

let closed_unions () =
  let src =
    {|
    {% match a
       with {@tag: 1, b: "a"} %} {%
       with {@tag: 2, b: 0} %} {%
       with {@tag: 1} with {@tag: 2} %} {%
      /match %}
    {% match b
      with {@tag: "a", b: 1} %} {%
      with {@tag: "b", b: "c"} %} {%
      with {@tag: "a"} %} {%
      with {@tag: "b"} %} {%
    /match %}
    {% match b with {@tag: "a", b: 2} %} {% with _ %} {% /match %}|}
  in
  check "Closed unions are inferred correctly"
    Ty.(
      make
        [
          ( "a",
            union_int `Closed "tag"
              [ (1, [ ("b", string ()) ]); (2, [ ("b", int ()) ]) ] );
          ( "b",
            union_string `Closed "tag"
              [ ("a", [ ("b", int ()) ]); ("b", [ ("b", string ()) ]) ] );
        ])
    (get_types src);
  let src =
    {|
    {% match a
      with {@tag: "a", b: 1} %} {%
      with {@tag: "b", b: "a"} %} {%
      with {@tag: "a"} %} {%
      with {@tag: "b"} %} {%
    /match %}
    {% map [{@tag: "c", b: 1.5}, {@tag: "d", b: [1]}, a, b] with _ %} {% /map %}
    {% match c
      with {@tag: 0, b: 1} %} {%
      with {@tag: 1, b: "a"} %} {%
      with {@tag: 0} %} {%
      with {@tag: 1} %} {%
    /match %}
    {% map [{@tag: 2, b: 1.5}, {@tag: 3, b: [1]}, c, d] with _ %} {% /map %}|}
  in
  check "Mixing open and closed unions works correctly"
    Ty.(
      make
        [
          ( "a",
            union_string `Closed "tag"
              [ ("a", [ ("b", int ()) ]); ("b", [ ("b", string ()) ]) ] );
          ( "b",
            union_string `Open "tag"
              [
                ("a", [ ("b", int ()) ]);
                ("b", [ ("b", string ()) ]);
                ("c", [ ("b", float ()) ]);
                ("d", [ ("b", list (int ())) ]);
              ] );
          ( "c",
            union_int `Closed "tag"
              [ (0, [ ("b", int ()) ]); (1, [ ("b", string ()) ]) ] );
          ( "d",
            union_int `Open "tag"
              [
                (0, [ ("b", int ()) ]);
                (1, [ ("b", string ()) ]);
                (2, [ ("b", float ()) ]);
                (3, [ ("b", list (int ())) ]);
              ] );
        ])
    (get_types src)

let boolean_unions () =
  let src =
    {|
    {% match a with {@tag: true, b} %} {{ b }} {% /match %}
    {% match b with {@tag: false, b} %} {{ b }} {% /match %}
    {% match c with {@tag: true, b} %} {{ b }}
            {% with {@tag: false, c} %} {{ c }} {% /match %}
    {% match d with {@tag: true, b} %} {{ b }} {% with _ %} {% /match %}|}
  in
  check "Boolean unions work"
    Ty.(
      make
        [
          ("a", union_true_only "tag" [ ("b", echo ()) ]);
          ("b", union_false_only "tag" [ ("b", echo ()) ]);
          ("c", union_boolean "tag" ~f:[ ("c", echo ()) ] ~t:[ ("b", echo ()) ]);
          ("d", union_boolean "tag" ~f:[] ~t:[ ("b", echo ()) ]);
        ])
    (get_types src)

let other_cases () =
  let src =
    {|{%~ match collections with {frontPage} ~%}
      {%~ match frontPage with [{data: {isoDate}}] ~%}
        <updated>
          {{ isoDate }}
        </updated>
      {%~ with _ %} {* Nothing! *}
      {%~ /match ~%}
      {% map frontPage with
         {
           templateContent,
           data: {title, isoDate, page: {excerpt}, pub: {@pub: true, absoluteUrl}}
          }
        ~%}
        <entry>
          <title>{{ title }}</title>
          <link href="{{ absoluteUrl }}" />
          <updated>{{ isoDate }}</updated>
          <id>{{ absoluteUrl }}</id>
          <summary type="html">{{ excerpt }}</summary>
          <content type="html">{{ templateContent }}</content>
        </entry>
      {% with {data: {pub: {@pub: false}} } %} {* Nothing! *}
      {%~ /map %}
    {%~ /match ~%}|}
  in
  check "The typechecker's update context works"
    Ty.(
      make
        [
          ( "collections",
            record
              [
                ( "frontPage",
                  list
                    (record
                       [
                         ("templateContent", echo ());
                         ( "data",
                           record
                             [
                               ("isoDate", echo ());
                               ("page", record [ ("excerpt", echo ()) ]);
                               ( "pub",
                                 union_boolean "pub" ~f:[]
                                   ~t:[ ("absoluteUrl", echo ()) ] );
                               ("title", echo ());
                             ] );
                       ]) );
              ] );
        ])
    (get_types src)

let pathologic () =
  let src =
    {|{% match a with _ %} {% with @"a" %} {% /match %}
      {% match b with {a: _} %} {% with {a: @1 } %} {% /match %}|}
  in
  check "Enums that come after variables are open"
    Ty.(
      make
        [
          ("a", enum_string `Open [ "a" ]);
          ("b", record [ ("a", enum_int `Open [ 1 ]) ]);
        ])
    (get_types src)

let components () =
  let a =
    Compile.Components.parse_string ~name:"A"
      {|{% map a with x %} {{ x }} {% /map %}
       {% map b with x %} {{ x }} {% /map %}|}
  in
  let src = {|{% A a=[1, a] b=["b", b] /%}|} in
  let r =
    Compile.from_string ~name:"<test>" (Compile.Components.make [ a ]) src
  in
  check "Components infer correctly."
    Ty.(make [ ("a", int ()); ("b", string ()) ])
    r.prop_types

let interface_basic () =
  let src =
    {|
  {% interface
    a = ?int
    b = @"a" | @"b"
  / %}
  {% match a with !1 %} {% with _ %} {% /match %}
  {% match b with @"a" %} {% with @"b" %} {% /match %}
  |}
  in
  check "Basic interface"
    Ty.(
      make [ ("a", nullable (int ())); ("b", enum_string `Closed [ "a"; "b" ]) ])
    (get_types src)

let interface_special () =
  let src =
    {|{% interface a = int / %}
      {% match a with _ %} {% /match %}|}
  in
  check "Any type can interface with unknown."
    Ty.(make [ ("a", int ()) ])
    (get_types src);
  let src =
    {|{% interface a = int b = string c = false | true d = float / %}
      {{ a }} {{ b }} {{ c }} {{ d }}|}
  in
  check "Any constant can interface with echoable."
    Ty.(
      make
        [ ("a", int ()); ("b", string ()); ("c", boolean ()); ("d", float ()) ])
    (get_types src);
  let src =
    {|{% interface a = {a: int, b: string} / %}
      {% match a with {a} %} {{ a }} {% /match %}|}
  in
  check "Interfaces may add record fields."
    Ty.(make [ ("a", record [ ("a", int ()); ("b", string ()) ]) ])
    (get_types src);
  let src =
    {|{% interface a = @0 | @1 | @2 | ... / %}
      {% match a with @0 with @1 %} {% with _ %} {% /match %}|}
  in
  check "Interfaces may add cases to open enums."
    Ty.(make [ ("a", enum_int `Open [ 0; 1; 2 ]) ])
    (get_types src);
  let src =
    {|{% interface
        a = {@tag: 0} | {@tag: 1, a: int} | {@tag: 2} | ...
      / %}
      {% match a with {@tag: 0} with {@tag: 1} %} {% with _ %} {% /match %}|}
  in
  check "Interfaces may add cases to open unions."
    Ty.(
      make
        [
          ( "a",
            union_int `Open "tag" [ (0, []); (1, [ ("a", int ()) ]); (2, []) ]
          );
        ])
    (get_types src)

let interface_print () =
  let interface =
    "a = {a: @0 | @1, b: @\"a\" | @\"b\"}\n\
     b = {@tag: false, a: <?string>} | {@tag: true, a: [int]}\n\
     c = {@tag: 0} | {@tag: 1, a: (float, false | true)}\n\
     d =\n\
    \    {@tag: \"a\", a: float}\n\
    \  | {@tag: \"b\", a: @0 | @1 | ...}\n\
     e =\n\
    \    {@tag: 0, a: _}\n\
    \  | {@tag: 1, b: @\"a\" | @\"b\" | ...}\n\
    \  | {@tag: 2, b: int, c: false | true}\n\
    \  | ...\n\
     f =\n\
    \    @\"a very looooong string enum type\"\n\
    \  | @\"it is so very loooooong! It just keeps going!\"\n\
    \  | ...\n\
     g =\n\
    \  {\n\
    \     a:\n\
    \       [\n\
    \            @\"another loooong enum type!\"\n\
    \          | @\"it is so very loooooong! It just keeps going!\"\n\
    \          | @\"this time it is inside a record!\"\n\
    \          | ...\n\
    \       ]\n\
    \  }"
  in
  let src = "{% interface " ^ interface ^ " / %}" in
  Alcotest.(
    check string
      "The typescheme pretty-printer prints valid (and pretty) syntax" interface
      (get_types src |> Format.asprintf "%a" Ty.pp_interface))

let () =
  let open Alcotest in
  run "Typechecker"
    [
      ( "Basic tests",
        [
          test_case "Echoes" `Quick echoes;
          test_case "Nullables" `Quick nullables;
          test_case "Nested" `Quick nested;
          test_case "Constructing values" `Quick constructing;
        ] );
      ( "Enums",
        [
          test_case "Open enums" `Quick open_enums;
          test_case "Closed enums" `Quick closed_enums;
          test_case "Boolean enums" `Quick boolean_enums;
        ] );
      ( "Tagged unions",
        [
          test_case "Open unions" `Quick open_unions;
          test_case "Closed unions" `Quick closed_unions;
          test_case "Boolean unions" `Quick boolean_unions;
        ] );
      ("Components", [ test_case "Components" `Quick components ]);
      ( "Interfaces",
        [
          test_case "Basic types" `Quick interface_basic;
          test_case "Special cases" `Quick interface_special;
          test_case "Type scheme prints valid interfaces" `Quick interface_print;
        ] );
      ( "Other cases",
        [
          test_case "Other cases" `Quick other_cases;
          test_case "Pathologic cases" `Quick pathologic;
        ] );
    ]
