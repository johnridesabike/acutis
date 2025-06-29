(** This test is intended to cover all possible error states. Due to its high
    number of test cases, running it as a single executable rather than a cram
    test is easier to maintain and faster to execute. *)

module Acutis_json = Acutis.Of_decodable (struct
  type t = Yojson.Basic.t
  type 'a assoc = (string * 'a) list

  let get_int = function `Int x -> Some x | _ -> None
  let get_string = function `String x -> Some x | _ -> None

  let get_float = function
    | `Float x -> Some x
    | `Int x -> Some (Float.of_int x)
    | _ -> None

  let get_bool = function `Bool x -> Some x | _ -> None
  let get_some = function `Null -> None | x -> Some x
  let get_seq = function `List x -> Some (List.to_seq x) | _ -> None
  let get_assoc = function `Assoc x -> Some x | _ -> None
  let assoc_find = List.assoc
  let assoc_mem = List.mem_assoc
  let assoc_to_seq = List.to_seq
  let null = `Null
  let some = Fun.id
  let of_float x = `Float x
  let of_string x = `String x
  let of_bool x = `Bool x
  let of_int x = `Int x
  let of_seq x = `List (List.of_seq x)
  let of_seq_assoc x = `Assoc (List.of_seq x)
  let to_string t = Yojson.Basic.pretty_to_string t
  let marshal x = `String (Marshal.to_string x [])
end)

let print_msgs = List.iter (Format.printf "%a@,@," Acutis.pp_message)

let render ?json ?(components = ([], Some Acutis.comps_empty)) src =
  let json =
    match json with None -> `Assoc [] | Some j -> Yojson.Basic.from_string j
  in
  let lexbuf = Lexing.from_string src in
  Lexing.set_filename lexbuf "<test>";
  let msgs1, components = components in
  let msgs2, parsed = Acutis.parse lexbuf in
  let msgs3, compiled =
    match (parsed, components) with
    | Some parsed, Some components -> Acutis.compile components parsed
    | None, None | None, Some _ | Some _, None -> ([], None)
  in
  let result =
    Option.map (fun compiled -> Acutis_json.apply compiled json) compiled
  in
  (List.concat [ msgs1; msgs2; msgs3 ], result)

let print_error title (msgs, result) =
  Format.printf "%s@,---@," title;
  print_msgs msgs;
  match result with Some (Error e) -> print_msgs e | Some (Ok _) | None -> ()

let print_error_any title (msgs, _) =
  Format.printf "%s@,---@," title;
  print_msgs msgs

let component_string ~fname ~name src =
  let lexbuf = Lexing.from_string src in
  Lexing.set_filename lexbuf fname;
  let msgs, parsed = Acutis.parse lexbuf in
  (msgs, Option.map (Acutis.comp_of_parsed name) parsed)

let comps_compile l =
  List.to_seq l
  |> Seq.filter_map (fun (msgs, x) -> print_msgs msgs; x)
  |> Acutis.comps_compile

let () =
  Format.printf "@[<v>";
  print_error "Illegal character 1" (render "{% match*");
  print_error "Illegal character 2" (render "{% +a %}");
  print_error "Illegal character 3" (render "{% match a &%}");
  print_error "Illegal character 4" (render "{% a &%}");

  print_error "A number greater than the maximum integer is a syntax error."
    (render "{% match 9999999999999999999");

  print_error "Illegal name: echo null" (render "{% null %}");
  print_error "Illegal name: echo false" (render "{% false %}");
  print_error "Illegal name: echo true" (render "{% true %}");
  print_error "Illegal name: _"
    (render "{% map [_] with x %} {% x %} {% /map %}");

  print_error "Unterminated strings" (render {|{% "a|});
  print_error "Unterminated comment" (render "{* {* a *}");
  print_error "Unterminated comment (nested)" (render "{* {* a");
  print_error "Unterminated expressions" (render "{% match");
  print_error "Illegal escape sequence" (render {|{% "\a" %}|});

  print_error "Missing a closing component name"
    (render "{% A %} abcd {% / a %}");

  print_error "Unclosed components (this one is confusing.)"
    (render "{% A /A %} abcd");

  print_error "Empty expressions" (render "{% %}{%~ a %}");

  print_error "Unexpected tokens (1)" (render "{%~ with %}");
  print_error "Unexpected tokens (3)"
    (render "{% A %} {% match a with _ %} {% /match /A %}");
  print_error "Unexpected tokens (inside # blocks)"
    (render "{% A a=#%} abcd {% /A %}");

  print_error "Bad pattern (1)" (render "{% match ABC with a %} {% /match %}");
  print_error "Bad pattern (2)" (render "{% match !ABC with a %} {% /match %}");
  print_error "Bad pattern (3)" (render "{% match !ABC with a %} {% /match %}");
  print_error "Bad pattern (4)" (render "{% match @ABC with a %} {% /match %}");
  print_error "Bad pattern (5)"
    (render "{% match a, ABC with a %} {% /match %}");
  print_error "Bad pattern (6)"
    (render "{% map_dict ABC with a %} {% /map_dict %}");
  print_error "Bad pattern (7)" (render "{% map ABC with a %} {% /map %}");
  print_error "Bad pattern (8)" (render "{% match {a: with %} {% /match %}");
  print_error "Bad pattern (9)" (render "{% match ( with %} {% /match %}");
  print_error "Bad pattern (10)" (render "{% match (a, with %} {% /match %}");
  print_error "Bad pattern (11)" (render "{% match [ with %} {% /match %}");
  print_error "Bad pattern (12)" (render "{% match [... with %} {% /match %}");
  print_error "Bad pattern (13)" (render "{% match [a, with %} {% /match %}");
  print_error "Unclosed < (4)" (render "{% match <a: with %} {% /match %}");
  print_error "Illegal pattern after with"
    (render "{% match a with Abc %} {% /match %}");
  print_error "Bad pattern (14)" (render "{% Z a=ABC / %}");
  print_error "Bad pattern (15)" (render "{% match null ~%}");

  print_error "Bad echo (1)" (render "{% with %}");
  print_error "Bad echo (2)" (render "{%~ null %}");
  print_error "Bad echo (3)" (render "{{% with %}}");
  print_error "Bad echo (4)" (render "{% a ? with %}");
  print_error "Bad echo (5)" (render "{% %i with %}");
  print_error "Bad echo (6)" (render "{{% a %}");
  print_error "Bad echo (7)" (render "{% a %}}");
  print_error "Bad echo (8)" (render "{% #%} block {%# %}");
  print_error "Bad echo (9)"
    (render "{% a ? Component %} child {% /Component %}");

  print_error "Bad echo format (1)" (render "{% % a %}");
  print_error "Bad echo format (2)" (render "{% %z a %}");
  print_error "Bad echo format (3)" (render "{% % i a %}");

  print_error "Bad prop (1)" (render "{% Z null=1 / %}");
  print_error "Bad prop (2)" (render "{% A a # %}");
  print_error "Bad prop (3)" (render "{% A b # %}");
  print_error "Bad prop (4)" (render "{% A b = 1 with %}");

  print_error "Illegal field name (1)"
    (render "{% match { with %} {% /match %}");
  print_error "Illegal field name (2)"
    (render "{% match {@ with %} {% /match %}");
  print_error "Illegal field name (3)"
    (render "{% match {a, with %} {% /match %}");
  print_error "Illegal field name (4)"
    (render "{% match < with %} {% /match %}");
  print_error "Illegal field name (5)"
    (render "{% match <a, with %} {% /match %}");

  print_error "Unclosed { (1)" (render "{% match {a with %} {% /match %}");
  print_error "Unclosed { (2)"
    (render "{% match a with {@tag with} %} {% /match %}");
  print_error "Unclosed { (3)" (render "{% match {\"a\" with %} {% /match %}");
  print_error "Unclosed { (4)" (render "{% match {a:0 with %} {% /match %}");
  print_error "Unclosed { (5)" (render "{% match {@a:0 with %} {% /match %}");

  print_error "Unclosed (" (render "{% match (a with %} {% /match %}");

  print_error "Unclosed [ (1)" (render "{% match [a with %} {% /match %}");
  print_error "Unclosed [ (2)" (render "{% match [...b with %} {% /match %}");

  print_error "Unclosed < (1)" (render "{% match <a with %} {% /match %}");
  print_error "Unclosed < (2)" (render "{% match <\"a\" with %} {% /match %}");
  print_error "Unclosed < (3)" (render "{% match <a:0 with %} {% /match %}");

  print_error "Missing commas in match/map patterns (1)"
    (render "{% match a b with a b %} {% /match %}");
  print_error "Missing commas in match/map patterns (2)"
    (render "{% map_dict a b with a %} {% /map_dict %}");
  print_error "Missing commas in match/map patterns (3)"
    (render "{% map a b with a %} {% /map %}");
  print_error "Missing commas in match/map patterns (4)"
    (render "{% match !null null with a %} {% /match %}");
  print_error "Missing commas in match/map patterns (5)"
    (render "{% match null with null ) %}");
  print_error "Missing commas in match/map patterns (5)"
    (render "{% match null, null } %}");

  print_error "Unmatched match" (render "{% match x with true %} b");

  print_error "Missing /match" (render "{% match x with x %} {% /map %}");
  print_error "Missing /map" (render "{% map x with x %} {% /match %}");
  print_error "Missing /map_dict" (render "{% map_dict x with x %} {% /map %}");

  print_error "Unseparated echoes" (render "{% a b %}");

  print_error "# Blocks must contain text" (render "{% A a=# abc # / %}");

  print_error "Bad record field access (1)" (render "{% match a.1 with %}");
  print_error "Bad record field access (2)" (render "{% a.? %}");

  print_error "Duplicate field"
    (render
       {|{% match a with {a: 0, a: "a", a: false} %} {% with _ %} {% /match %}|});
  print_error "Duplicate tag field"
    (render {|{% match a with {@a: 0, a: "a"} %} {% with _ %} {% /match %}|});
  print_error "Duplicate dict field"
    (render {|{% match a with <a: 0, a: "a"> %} {% with _ %} {% /match %}|});
  print_error "Multiple record tags"
    (render {|{% match a with {@a: 0, @b: "a"} %} {% with _ %} {% /match %}|});

  print_error "Pattern count mismatch 1"
    (render "{% match a, b, c with 1, 2 %} d {% /match %}");
  print_error "Pattern count mismatch 2"
    (render "{% match a with 1, 2 %} d {% with 1, 2, 3 %} z {% /match %}");
  print_error "Pattern count mismatch 3 (error location is correct)"
    (render "{% match a, b with 1 with 2, 3 with 4 with 5, 6 %} d {% /match %}");
  print_error "Pattern count mismatch (map)"
    (render "{% map a with 1, 2, 3 %} d {% with 4, 5, 6 %} z {% /map %}");

  print_error "You can't bind a name more than once when destructuring."
    (render "{% match a with [x, x] %} a {% with _ %} b {% /match %}");

  print_error "Echoed string literals cannot appear before a ?."
    (render {|{% "ab" ? "cd" %}|});
  print_error "Record field access type errors fail correctly."
    (render {|{% a.b %} {% %i a.b %}|});

  print_error "String <> int"
    (render {|{% match a with "a" %} {% with 1 %} {% with _ %} {% /match %}|});
  print_error "Float <> int"
    (render {|{% match a with 0.0 %} {% with 1 %} {% with _ %} {% /match %}|});
  print_error "String <> float"
    (render {|{% match a with "a" %} {% with 1.0 %} {% with _ %} {% /match %}|});
  print_error "Map list key type mismatch."
    (render "{% map [1] with a, \"b\" %} {% a %} {% with _ %} {% /map %}");
  print_error "Map dict key type mismatch."
    (render
       "{% map_dict <\"a\": 1> with a, 1 %} {% a %} {% with _ %}\n\
        {% /map_dict %}");

  print_error "[int] <> [string]"
    (render
       "{% match a with [\"a\"] %} {% with _ %} {% /match %}\n\
        {% match a with [1] %} {% with _ %} {% /match %}");
  print_error "{a: int} <> {a: float}"
    (render
       "{% match a with {a: 1} %} {% with _ %} {% /match %}\n\
        {% match a with {a: 0.0} %} {% with _ %} {% /match %}");
  print_error "(string, _) <> (float, _)"
    (render
       "{% match a with (\"a\", a) %} {% a %} {% with _ %} {% /match %}\n\
        {% match a with (1.0, a) %} {% a %} {% with _ %} {% /match %}");
  print_error "Tagged record <> untagged record"
    (render
       "{% match a with {@a: 1} %} {% with _ %} {% /match %}\n\
        {% match a with {a: 1} %} {% with _ %} {% /match %}");
  print_error "Dict <> record"
    (render
       "{% match a with <a: 1> %} {% with _ %} {% /match %}\n\
        {% match a with {a: 1} %} {% with _ %} {% /match %}");
  print_error "?int <> int"
    (render
       "{% match a with !1 %} {% with _ %} {% /match %}\n\
        {% match a with 1 %} {% with _ %} {% /match %}");
  print_error "2-tuple <> 3-tuple"
    (render
       "{% match a with (1, 2) %} {% with _ %} {% /match %}\n\
        {% match a with (1, 2, 3) %} {% with _ %} {% /match %}");

  print_error "Records with missing fields (1)"
    (render
       "{% map [(1, {a: 1}), (2, {b: 2})] with (i, {a}) %}\n\
       \  {% %i i %} {% %i a %} {% /map %}");
  print_error "Records with missing fields (2)"
    (render "{% match {a: 1} with {a: _, b: _} %} {% /match %}");
  print_error "Record field access type errors fail correctly."
    (render
       {|{% match a.b.c with true %}{% with false %}{% /match %}
         {% match a.b.c with 1 %}{% with _ %}{% /match %}|});
  let comp = component_string ~fname:"a.acutis" ~name:"A" "{% a %}" in
  print_error "Records with missing fields (Component)"
    (render ~components:(comps_compile [ comp ]) "{% A / %}");

  print_error "Closed enum <> open enum"
    (render
       "{% match a with @1 %} {% with @2 %} {% with _ %} {% /match %}\n\
        {% match a with @1 %} {% /match %}");
  print_error "Enum vars with no subset are reported."
    (render
       "{% match a with @1 %} {% with @2 %} {% /match %}\n\
        {% match b with @3 %} {% with @4 %} {% /match %}\n\
        {% map [a, b] with _ %} {% /map %}");
  print_error "Enum vars + literals with no subset are reported."
    (render
       "{% match a with @1 %} {% with @2 %} {% /match %}\n\
        {% map [a, @3] with _ %} {% /map %}");
  print_error "Enum literals with no subset are reported (1)."
    (render "{% match @0 with @1 %} {% /match %}");
  print_error "Enum literals with no subset are reported (2)."
    (render "{% match @0 with @\"a\" %} {% /match %}");
  print_error "| false | true <> only false"
    (render "{% match true with false %} {% /match %}");
  print_error "Bool <> int"
    (render "{% match a with @0 with true %} {% /match %}");

  print_error "Int tag <> string tag"
    (render
       "{% match a with {@tag: 0} %} {% with {@tag: \"a\", b} %} {% b %}\n\
        {% /match %}");
  print_error "Bool tag <> int tag"
    (render "{% match a with {@tag: 0} with {@tag: true} %} {% /match %}");
  print_error "Random other tags don't compile."
    (render "{% match a with {@tag: []} %} {% /match %}");
  print_error "Tag names must be coherent."
    (render
       "{% match a with {@a: 0} %} {% with {@b: 1, c} %} {% c %} {% /match %}");

  print_error "Variable names must be coherent."
    (render
       "{% match a, b\n\
       \   with null, !b\n\
       \   with !a, null %}\n\
        {% with !_, !_ with null, null %}\n\
        {% /match %}");
  print_error "Variable types must be coherent."
    (render
       "{% match a, b with 1, \"a\" %} {% with a, _ with _, a %} {% /match %}");

  print_error "Component names match" (render "{% A %} {% /B %}");
  let comp = component_string ~fname:"comp" ~name:"Comp" "{% a %} {% b %}" in
  print_error "Components can't take extra props"
    (render ~components:(comps_compile [ comp ]) "{% Comp a b c / %}");
  let comp = component_string ~fname:"comp" ~name:"Comp" "{% %i children %}" in
  print_error "Implict children is typed and reported correctly"
    (render ~components:(comps_compile [ comp ]) "{% Comp %} {% /Comp %}");

  print_error "Basic unused bindings are reported."
    (render "{% match a with {x} %} {% /match %}");
  print_error "Unused bindings are reported in the order they appear."
    (render
       "{% match a with {x, y} %}\n\
       \  {% match x with {z} %} {% /match %}\n\
        {% /match %}");
  print_error "Shadowing bindings can report them as unused."
    (render
       "{% match a, b with {x}, {y} %}\n\
       \  {% match x with {y} %} {% y %} {%/ match %}\n\
        {% /match %}");
  print_error "Basic pattern (1)."
    (render
       "{% match a, b, c\n\
       \   with 10, 11, 12 %}\n\
        {% with _x, 21, 22 %}\n\
        {% with 10, 11, 12 %}\n\
        {% with  _,  _,  _ %}\n\
        {% /match %}");
  print_error "Basic pattern (2)."
    (render
       "{% match a, b, c\n\
       \   with 10, 11, 12 %}\n\
        {% with _x, 21, 22 %}\n\
        {% with 30, 31, 32 %}\n\
        {% with 30, _y, 42 %}\n\
        {% with 30, 31, 42 %}\n\
        {% with  _,  _,  _ %}\n\
        {% /match %}");
  print_error "Nest patterns merge into wildcard patterns correctly (1)."
    (render "{% match a, b with _x, _y %} {% with (_, _), 40 %} {% /match %}");
  print_error "Nest patterns merge into wildcard patterns correctly (2)."
    (render
       "{% match a, b\n\
       \   with _x, 1 %}\n\
        {% with (\"a\", \"b\"), 10 %}\n\
        {% with (\"a\", \"b\"), 1 %}\n\
        {% with             _, _ %}\n\
        {% /match %}");

  print_error "Partial matching with integers."
    (render "{% match a with 0 with 10 with 20 with 30 %} {% /match %}");
  print_error "Partial matching with lists (1)."
    (render "{% match a with [] with [_] %} {% /match %}");
  print_error "Partial matching with lists (2)."
    (render "{% match a with [_] %} {% /match %}");
  print_error "Partial matching with Nullables (1)."
    (render "{% match a with null %} {% /match %}");
  print_error "Partial matching with Nullables (2)."
    (render "{% match a with !_ %} {% /match %}");
  print_error "Partial matching with Nullables (3)."
    (render "{% match a with !1 %} {% with null %} {% /match %}");
  print_error "Partial matching with enums nested in nullables."
    (render "{% match a, b with !@1, 2 %} {% with null, _ %} {% /match %}");
  print_error "Partial matching with records."
    (render "{% match a with {b: 10} %} {% with {a: 20} %} {% /match %}");
  print_error "Partial matching with dictionaries (1)."
    (render "{% match a with <a: true> %} {% with <a: false> %} {% /match %}");
  print_error "Partial matching with dictionaries (2)."
    (render "{% match a with <a> %} {% a %} {% with <b> %} {% b %} {% /match %}");
  print_error "Partial matching with unions (1)."
    (render
       "{% match a with {@tag: 0, a: 10} %} {% with {@tag: 1, b: 20} %}\n\
        {% /match %}");
  print_error "Partial matching with unions (2)."
    (render
       "{% match a with {@tag: true, a: 10} %}{% with {@tag: false, b: 20} %}\n\
        {% /match %}");
  print_error "Partial matching with unions (3)."
    (render
       "{% match a with {@tag: true, a: 10} %} {% with {@tag: false, b: _} %}\n\
        {% /match %}");
  print_error "Partial matching with unions (4)."
    (render
       "{% match a with {@tag: true} with {@tag: false} %}{% /match %}\n\
        {% match b, a with 1, {@tag: true} with _, {@tag: false } %}\n\
        {% /match %}");
  print_error
    "Partial matching with unions prints counterexample patterns correctly"
    (render
       "{% match c, d\n\
       \   with 1, {@tag: 0, a: @1, b: {@tag: 10, c: \"a\"}} %}\n\
        {% with 2, {@tag: 0, a: @2, b: {@tag: 20}} %}\n\
        {% with _, {@tag: 1, c: @\"c\"} %}\n\
        {% /match %}");
  print_error "Partial matching with records."
    (render
       "{% match a with {firstName: name, favoriteColor: \"green\"} %}\n\
        {% name %}'s favorite color is green.\n\
        {% /match %}");
  print_error "Partial lists print correctly."
    (render
       "{%~ match author\n\
       \    with {@kind: \"person\", name: _, books: [_newest, ..._older]} %}\n\
        {%~  with {@kind: \"anonymous\", books: []} %}\n\
       \  This author hasn't published any books.\n\
        {% /match %}");
  print_error "Partial matching with enums, closed"
    (render
       "{% match a, b with 1, @1 %}\n\
        {% with 2, @2 %}\n\
        {% with _, @3 %}\n\
        {% /match %}");
  print_error "Partial matching with unions, closed"
    (render
       "{% match a, b with 1, {@tag: 1, a} %} {% a %}\n\
        {% with 2, {@tag: 2, b} %} {% b %}\n\
        {% with _, {@tag: 3, c} %} {% c %}\n\
        {% /match %}");
  print_error "Partial matching with enums, open"
    (render
       "{% match a, b with 1, @1 %}\n\
        {% with 2, _ %}\n\
        {% with _, @1 %}\n\
        {% with _, @2 %}\n\
        {% /match %}");
  print_error "Partial matching with unions, open"
    (render
       "{% match a, b with 1, {@tag: 1, a} %} {% a %}\n\
        {% with 2, _ %} \n\
        {% with _, {@tag: 2, b} %} {% b %}\n\
        {% with _, {@tag: 3, c} %} {% c %}\n\
        {% /match %}");

  print_error "Both nil and cons paths fail to merge into a wildcard."
    (render
       {|
    {% match a, b
        with 1, _ %}
    {%  with _, !1 %}
    {%  with _, null %}
    {%  with 1, !2 %}
    {%  with _, _ %}
    {% /match %}|});

  print_error "Unused cases are caught after an exhaustive nest."
    (render "{% match a with (_, _) %} {% with _ %} {% /match %}");

  print_error "Dict patterns match a subset of the input."
    (render
       "{% match a with <a: 1> %} {% with <a: 1, b: 2> %} {% with _ %}\n\
        {% /match %}");
  print_error "Empty dicts (<>) match all inputs (like _)."
    (render "{% match a with <> %} {% with <a> %} {% a %} {% /match %}");

  print_error "Template blocks are not allowed in destructure patterns."
    (render "{% match a with {b: #%} {%#} %} {% /match %}");
  print_error "Record accessors are not allowed in destructure patterns."
    (render "{% match a with {b: x.z} %} {% /match %}");

  print_error_any "Cyclic dependencies are reported."
    (comps_compile
       [
         component_string ~fname:"a.acutis" ~name:"A" "{% B /%}";
         component_string ~fname:"b.acutis" ~name:"B" "{% C /%}";
         component_string ~fname:"c.acutis" ~name:"C" "{% D /%}";
         component_string ~fname:"d.acutis" ~name:"D" "{% B /%}";
       ]);
  print_error_any "Missing components are reported."
    (comps_compile
       [
         component_string ~fname:"a.acutis" ~name:"A" "{% B /%}";
         component_string ~fname:"b.acutis" ~name:"B" "{% C /%}";
         component_string ~fname:"c.acutis" ~name:"C" "{% D /%}";
       ]);
  print_error "Missing components are reported (by root)." (render "{% A /%}");
  print_error_any "Duplicate names are reported."
    (comps_compile
       [
         component_string ~fname:"a.acutis" ~name:"A" "{% B /%}";
         component_string ~fname:"a.acutis" ~name:"A" "{% B /%}";
         component_string ~fname:"b.acutis" ~name:"B" "";
         component_string ~fname:"b.acutis" ~name:"B" "";
       ]);

  print_error "Basic type mismatch."
    (render "{% match a with {b} %} {% b %} {% /match %}"
       ~json:{|{"a": {"b": []}}|});
  let json = {|{"a": "a", "b": true, "c": [], "d": {"e": 0}}|} in
  print_error "Map type mismatch (1)."
    (render "{% map a with {a} %}{% a %}{% /map %}" ~json);
  print_error "Map type mismatch (2)."
    (render "{% map [1, 2, ...a] with a %}{% %i a %}{% /map %}" ~json);
  print_error "Missing bindings are reported" (render "{% z %}" ~json);
  print_error "Missing bindings are reported (with nested record)"
    (render "{% aa %} {% %i d.e %}" ~json);
  print_error "Bad enums are reported: boolean."
    (render "{% match b with false %} {% /match %}" ~json);
  print_error "Bad enums are reported: int."
    (render "{% match a with @1 %} {% with @2 %} {% /match %}"
       ~json:{|{"a": 3}|});
  print_error "Bad enums are reported: int."
    (render "{% match a with @\"a\" %} {% with @\"b\" %} {% /match %}"
       ~json:{|{"a": "c"}|});
  print_error "Tuple size mismatch."
    (render "{% match a with (a, b) %} {% a %} {% b %} {% /match %}"
       ~json:{|{"a": ["a"]}|});
  print_error "Bad unions are reported (1)."
    (render "{% match a with {@tag: 1, a} %} {% a %} {% /match %}"
       ~json:{|{"a": {"tag": "a", "a": "a"}}|});
  print_error "Bad unions are reported (2)."
    (render "{% match a with {@tag: 1, a} %} {% a %} {% /match %}"
       ~json:{|{"a": {"tag": 2, "a": "a"}}|});
  print_error "Multiple decode errors are reported."
    (render "{% match a with {b} %} {% b %} {% /match %} {% c %} {% d.e %}"
       ~json:{|{"a": {"b": []}, "c": 0, "d": {}}|});
  print_error "Zero-length record fields are reported."
    (render {|{% zero."" %}|} ~json:{|{"zero": {}}|});

  (* Interface parse *)
  print_error "Invalid field names."
    (render "{% interface a = {a: int, with: string } %}");
  print_error "Missing , or }."
    (render "{% interface a = {a: int b: string } %}");

  print_error "Unclosed interface" (render "{% interface a = int");

  print_error "Bad prop name (1)." (render "{% interface with = int %}");
  print_error "Bad prop name (2)." (render "{% interface x = int with = int %}");

  print_error "Bad token after a variant (1)."
    (render "{% interface x={@a: 1} with %}");
  print_error "Bad token after a variant (2)."
    (render "{% interface x=@\"a\" with %}");
  print_error "Bad token after a variant (3)."
    (render "{% interface x=@0 with %}");
  print_error "Bad token after a variant (4)."
    (render "{% interface x=false with %}");

  print_error "Missing equals." (render "{% interface x int %}");

  print_error "Invalid type (1)." (render "{% interface x=with %}");
  print_error "Invalid type (2)." (render "{% interface x=?with %}");
  print_error "Invalid type (3)." (render "{% interface x=[with] %}");
  print_error "Invalid type (4)." (render "{% interface x={with: int} %}");
  print_error "Invalid type (5)." (render "{% interface x={@with: 1} %}");
  print_error "Invalid type (6)." (render "{% interface x={a: with} %}");
  print_error "Invalid type (7)." (render "{% interface x=<with> %}");
  print_error "Invalid type (8)." (render "{% interface x=(with) %}");
  print_error "Invalid type (9)." (render "{% interface x=(int, with) %}");

  print_error "Missing colon (1)." (render "{% interface x={a b} %}");
  print_error "Missing colon (2)." (render "{% interface x={@a b} %}");

  print_error "Bad enum" (render "{% interface x=@a %}");

  print_error "Bad token after record pipe."
    (render "{% interface x={@a: 1} | y %}");
  print_error "Bad token after string enum pipe."
    (render "{% interface x=@\"a\" | y %}");
  print_error "Bad token after int enum pipe."
    (render "{% interface x=@0 | y %}");

  print_error "Missing >" (render "{% interface x=<int} %}");
  print_error "Missing ]" (render "{% interface x=[int} %}");
  print_error "Missing )" (render "{% interface x=(int} %}");

  print_error "Bad string enum" (render "{% interface x=@\"a\" | @0 %}");
  print_error "Bad int enum" (render "{% interface x=@0 | @\"a\" %}");
  print_error "Bad boolean" (render "{% interface x=false | @0 %}");

  print_error "Bad union tag type" (render "{% interface x={@x: [int]} %}");

  (* Interface type parse *)
  print_error "Duplicate declarations" (render "{% interface x=int x=string %}");
  print_error "Non-existent type names." (render "{% interface x=lmao %}");
  print_error "Untagged unions (1)"
    (render "{% interface x = {a: int} | {b: string} %}");
  print_error "Untagged unions (2)"
    (render "{% interface x = {@tag: 1, a: int} | {b: string} %}");
  print_error "Tagged unions with mismatched tag names"
    (render "{% interface x = {@tag: 1} | {@badtag: 2} %}");
  print_error "Duplicate tags."
    (render "{% interface x = {@tag: 1} | {@tag: 1} %}");
  print_error "Duplicate enums."
    (render "{% interface x = @1 | @1 y = @\"a\" | @\"a\" z = true | true  %}");
  print_error "Open boolean unions."
    (render "{% interface x = {@tag: false} | ... %}");
  print_error "Tag type error: int <> string."
    (render "{% interface x = {@tag: 1} | {@tag: \"a\"} %}");
  print_error "Tag type error: string <> bool."
    (render "{% interface x = {@tag: \"a\"} | {@tag: true} %}");
  print_error "Tag type error: bool <> int."
    (render "{% interface x = {@tag: false} | {@tag: 100} %}");

  (* Interface type check *)
  print_error "Interface type error: bool <> int."
    (render "{% interface x = int %}{% match x with true %}{% /match %}");
  print_error "Interface is missing props."
    (render "{% interface x = int %} {% y %}");
  print_error "Interface is missing record fields."
    (render
       "{% interface x = {a: int} %} \n\
        {% match x with {a, b} %} {% a %} {% b %} {% /match %}");
  print_error "Interface is missing enum cases."
    (render
       "{% interface x = @0 | @1 | ... %} \n\
        {% match x with @0 %} {% with @1 %} {% with @2 %} {% with _ %}\n\
        {% /match %}");
  print_error "Interface is missing union cases."
    (render
       "{% interface x = {@tag: 0} | {@tag: 1, a: int} | ... %} \n\
        {% match x\n\
       \  with {@tag: 0} %}\n\
        {% with {@tag: 1, a} %} {% a %}\n\
        {% with {@tag: 2, b} %} {% b %}\n\
        {% with _ %}\n\
        {% /match %}");
  print_error "Unknown is not equal to any other type."
    (render "{% interface x = _ %} {% x %}");

  (* Custom decodable interface parse *)
  let jsonintf json = Yojson.Basic.from_string json |> Acutis_json.interface in
  print_error_any "JSON interface: null." (jsonintf {|{"prop": null}|});
  print_error_any "JSON interface: integer." (jsonintf {|{"prop": 1}|});
  print_error_any "JSON interface: string props." (jsonintf {|"prop"|});
  print_error_any "JSON interface: empty array type."
    (jsonintf {|{ prop: [] }|});
  print_error_any "JSON interface: empty object."
    (jsonintf {|{ prop: { a: {} } }|});
  print_error_any "JSON interface: empty array tag."
    (jsonintf {|{ prop: { a: ["tag", []] } }|});
  print_error_any "JSON interface: enum with int & string."
    (jsonintf {|{ prop: ["enum", 1, "a"] }|});
  print_error_any "JSON interface: enum with bool & string."
    (jsonintf {|{ prop: ["enum", true, "a"] }|});
  print_error_any "JSON interface: enum with string & bool."
    (jsonintf {|{ prop: ["enum", "a", true] }|});
  print_error_any "JSON interface: tag with extra values."
    (jsonintf {|{ prop: ["union", { tag: ["tag", 0, 1] }] }|});
  print_error_any "JSON interface: nullable with extra params."
    (jsonintf {|{ prop: ["nullable", "int", "string"] }|});
  print_error_any "JSON interface: list with extra params."
    (jsonintf {|{ prop: ["list", "int", "string"] }|});
  print_error_any "JSON interface: dict with extra params."
    (jsonintf {|{ prop: ["dict", "int", "string"] }|});

  Format.printf "@]"
