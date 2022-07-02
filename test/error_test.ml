open Acutis
module RenderSync = Render.Make (Sync) (Acutis_data_json.Data)

let render ?(json = "{}") src () =
  let temp = Compile.(from_string ~name:"<test>" Components.empty src) in
  let json = Yojson.Basic.from_string json in
  ignore @@ RenderSync.make temp json

exception E = Error.Acutis_error

let illegal_chars () =
  let open Alcotest in
  check_raises "Illegal character 1"
    (E "File <test>, 1:9-1:10\nSyntax error.\n") (render "{% match*");
  check_raises "Illegal character 2" (E "File <test>, 1:4-1:5\nSyntax error.\n")
    (render "{% .a %}");
  check_raises "Illegal character 3"
    (E "File <test>, 1:12-1:13\nSyntax error.\n") (render "{% match a %%}");
  check_raises "Illegal character 4" (E "File <test>, 1:6-1:7\nSyntax error.\n")
    (render "{{ b %}");
  check_raises "Illegal character 5"
    (E "File <test>, 1:6-1:7\nParse error.\n92") (render "{{ a b }}");
  check_raises "Illegal character 6"
    (E "File <test>, 1:4-1:5\nParse error.\n86") (render "{{ ? }}");
  check_raises "Illegal character 7" (E "File <test>, 1:6-1:7\nSyntax error.\n")
    (render "{{ a }%}")

let illegal_names () =
  let open Alcotest in
  check_raises "Illegal name: echo null"
    (E "File <test>, 1:4-1:8\nSyntax error.\n") (render "{{ null }}");
  check_raises "Illegal name: echo false"
    (E "File <test>, 1:4-1:9\nSyntax error.\n") (render "{{ false }}");
  check_raises "Illegal name: echo true"
    (E "File <test>, 1:4-1:8\nSyntax error.\n") (render "{{ true }}");
  check_raises "Illegal name: match Abc"
    (E "File <test>, 1:17-1:20\nParse error.\n63")
    (render "{% match a with Abc %} {% /match %}");
  check_raises "Illegal name: match null"
    (E "File <test>, 1:18-1:22\nParse error.\n14")
    (render "{% match a with {null} %} {% /match %}");
  check_raises "Illegal name: match null"
    (E
       "File <test>, 1:9-1:10\n\
        Type error.\n\
        Underscore (`_`) is not a valid name.")
    (render "{% map [_] with x %} {{ x }} {% /map %}")

let unterminated () =
  let open Alcotest in
  check_raises "Unterminated strings"
    (E "File <test>, 1:6-1:6\nSyntax error.\n") (render {|{{ "a|});
  check_raises "Unterminated comments"
    (E "File <test>, 1:11-1:11\nSyntax error.\n") (render "{* {* a *}");
  check_raises "Unterminated expressions"
    (E "File <test>, 1:9-1:9\nSyntax error.\n") (render "{% match")

let illegal_escape () =
  let open Alcotest in
  check_raises "Illegal escape sequence"
    (E "File <test>, 1:5-1:6\nSyntax error.\n") (render {|{{ "\a" }}|})

let dup_record_field () =
  let open Alcotest in
  check_raises "Duplicate field"
    (E "File <test>, 1:18-1:30\nParse error.\nDuplicate field \"a\".")
    (render {|{% match a with {a: 0, a: "a"} %} {% with _ %} {% /match %}|});
  check_raises "Duplicate tag field"
    (E "File <test>, 1:18-1:31\nParse error.\nDuplicate field \"a\".")
    (render {|{% match a with {@a: 0, a: "a"} %} {% with _ %} {% /match %}|});
  check_raises "Duplicate dict field"
    (E "File <test>, 1:18-1:30\nParse error.\nDuplicate field \"a\".")
    (render {|{% match a with <a: 0, a: "a"> %} {% with _ %} {% /match %}|});
  check_raises "Multiple record tags"
    (E
       "File <test>, 1:18-1:32\n\
        Parse error.\n\
        This tagged record has multiple tags.")
    (render {|{% match a with {@a: 0, @b: "a"} %} {% with _ %} {% /match %}|})

let bad_ast () =
  let open Alcotest in
  check_raises "Unmatched match" (E "File <test>, 1:26-1:26\nParse error.\n70")
    (render "{% match x with true %} b");
  check_raises "Unmatched map" (E "File <test>, 1:29-1:29\nParse error.\n70")
    (render "{% map x with {y} %} {{ y }}");
  check_raises "Invalid child" (E "File <test>, 1:8-1:9\nParse error.\n104")
    (render "{% Z A=1 / %}");
  check_raises "Bad binding name" (E "File <test>, 1:6-1:10\nParse error.\n99")
    (render "{% Z null=1 / %}");
  check_raises "Invalid statements" (E "File <test>, 1:4-1:5\nParse error.\n3")
    (render "{% a %}");
  check_raises "You can't unescape children"
    (E "File <test>, 1:5-1:6\nParse error.\n90") (render "{{ &A }}");
  check_raises "Components without implicit Children end"
    (E "File <test>, 1:16-1:16\nParse error.\n111") (render "{% A /A %} abcd")

let pat_count_mismatch () =
  let open Alcotest in
  check_raises "Pattern count mismatch 1"
    (E "File <test>, 1:4-1:42\nType error.\nPattern count mismatch.")
    (render "{% match a, b, c with 1, 2 %} d {% /match %}");
  check_raises "Pattern count mismatch 2"
    (E "File <test>, 1:30-1:42\nType error.\nPattern count mismatch.")
    (render "{% match a with 1, 2 %} d {% with 1, 2, 3 %} z {% /match %}");
  check_raises "Pattern count mismatch (map)"
    (E
       "File <test>, 1:4-1:56\n\
        Type error.\n\
        Expressions `map` and `map_dict` can only have one or two patterns for \
        each\n\
        `with` expression.")
    (render "{% map a with 1, 2, 3 %} d {% with 4, 5, 6 %} z {% /map %}")

let dup_name_destructure () =
  let open Alcotest in
  check_raises "You can't bind a name more than once when destructuring."
    (E
       "File <test>, 1:21-1:22\n\
        Type error.\n\
        The name `x` is already bound in this pattern.")
    (render "{% match a with [x, x] %} a {% with _ %} b {% /match %}")

let type_error_echo () =
  let open Alcotest in
  check_raises "Echoed string literals cannot appear before a ?."
    (E
       "File <test>, 1:4-1:8\n\
        Type error.\n\
        Echoed string literals cannot appear before a ?.")
    (render {|{{ "ab" ? "cd" }}|})

let type_error_const () =
  let open Alcotest in
  check_raises "String <> int"
    (E
       "File <test>, 1:32-1:33\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: string\n\
        Received: int")
    (render {|{% match a with "a" %} {% with 1 %} {% with _ %} {% /match %}|});
  check_raises "Float <> int"
    (E
       "File <test>, 1:32-1:33\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: float\n\
        Received: int")
    (render {|{% match a with 0.0 %} {% with 1 %} {% with _ %} {% /match %}|});
  check_raises "String <> float"
    (E
       "File <test>, 1:32-1:35\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: string\n\
        Received: float")
    (render {|{% match a with "a" %} {% with 1.0 %} {% with _ %} {% /match %}|});
  check_raises "Map list key type mismatch."
    (E
       "File <test>, 1:4-1:55\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: int\n\
        Received: string")
    (render "{% map [1] with a, \"b\" %} {{ a }} {% with _ %} {% /map %}");
  check_raises "Map dict key type mismatch."
    (E
       "File <test>, 1:4-2:13\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: string\n\
        Received: int")
    (render
       "{% map_dict <\"a\": 1> with a, 1 %} {{ a }} {% with _ %}\n\
        {% /map_dict %}")

let type_error_nest () =
  let open Alcotest in
  check_raises "[int] <> [string]"
    (E
       "File <test>, 2:10-2:11\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: [int]\n\
        Received: [string]")
    (render
       "{% match a with [\"a\"] %} {% with _ %} {% /match %}\n\
        {% match a with [1] %} {% with _ %} {% /match %}");
  check_raises "{a: int} <> {a: float}"
    (E
       "File <test>, 2:10-2:11\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: {a: float}\n\
        Received: {a: int}")
    (render
       "{% match a with {a: 1} %} {% with _ %} {% /match %}\n\
        {% match a with {a: 0.0} %} {% with _ %} {% /match %}");
  check_raises "(string, _) <> (float, _)"
    (E
       "File <test>, 2:10-2:11\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: (float, echoable)\n\
        Received: (string, echoable)")
    (render
       "{% match a with (\"a\", a) %} {{ a }} {% with _ %} {% /match %}\n\
        {% match a with (1.0, a) %} {{ a }} {% with _ %} {% /match %}");
  check_raises "Tagged record <> untagged record"
    (E
       "File <test>, 2:10-2:11\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: {a: int}\n\
        Received: | {@a: 1} | ...")
    (render
       "{% match a with {@a: 1} %} {% with _ %} {% /match %}\n\
        {% match a with {a: 1} %} {% with _ %} {% /match %}");
  check_raises "Dict <> record"
    (E
       "File <test>, 2:10-2:11\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: {a: int}\n\
        Received: <int>")
    (render
       "{% match a with <a: 1> %} {% with _ %} {% /match %}\n\
        {% match a with {a: 1} %} {% with _ %} {% /match %}");
  check_raises "?int <> int"
    (E
       "File <test>, 2:10-2:11\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: int\n\
        Received: ?int")
    (render
       "{% match a with !1 %} {% with _ %} {% /match %}\n\
        {% match a with 1 %} {% with _ %} {% /match %}");
  check_raises "2-tuple <> 3-tuple"
    (E
       "File <test>, 2:10-2:11\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: (int, int, int)\n\
        Received: (int, int)")
    (render
       "{% match a with (1, 2) %} {% with _ %} {% /match %}\n\
        {% match a with (1, 2, 3) %} {% with _ %} {% /match %}")

let type_error_record () =
  let open Alcotest in
  check_raises "Records with missing fields (1)"
    (E
       "File <test>, 1:26-1:32\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: {a: int}\n\
        Received: {b: _}")
    (render
       "{% map [(1, {a: 1}), (2, {b: 2})] with (i, {a}) %}\n\
       \  {{ i }} {{ a }} {% /map %}");
  check_raises "Records with missing fields (2)"
    (E
       "File <test>, 1:10-1:16\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: {a: _, b: _}\n\
        Received: {a: _}")
    (render "{% match {a: 1} with {a, b} %} {% /match %}");
  let comps = Compile.Components.(make [ parse_string ~name:"A" "{{ a }}" ]) in
  check_raises "Records with missing fields (Component)"
    (E
       "File <test>, 1:4-1:7\n\
        Type error.\n\
        This is missing key `a` of type echoable") (fun () ->
      ignore @@ Compile.from_string ~name:"<test>" comps "{% A / %}")

let type_error_enum () =
  let open Alcotest in
  check_raises "Closed enum <> open enum"
    (E
       "File <test>, 2:10-2:11\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: | @1\n\
        Received: | @1 | @2 | ...")
    (render
       "{% match a with @1 %} {% with @2 %} {% with _ %} {% /match %}\n\
        {% match a with @1 %} {% /match %}");
  check_raises "Enum vars with no subset are reported."
    (E
       "File <test>, 3:12-3:13\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: | @1 | @2\n\
        Received: | @3 | @4")
    (render
       "{% match a with @1 %} {% with @2 %} {% /match %}\n\
        {% match b with @3 %} {% with @4 %} {% /match %}\n\
        {% map [a, b] with _ %} {% /map %}");
  check_raises "Enum vars + literals with no subset are reported."
    (E
       "File <test>, 2:12-2:14\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: | @1 | @2\n\
        Received: | @3 | ...")
    (render
       "{% match a with @1 %} {% with @2 %} {% /match %}\n\
        {% map [a, @3] with _ %} {% /map %}");
  check_raises "Enum literals with no subset are reported (1)."
    (E
       "File <test>, 1:10-1:12\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: | @1\n\
        Received: | @0 | ...")
    (render "{% match @0 with @1 %} {% /match %}");
  check_raises "Enum literals with no subset are reported (2)."
    (E
       "File <test>, 1:10-1:12\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: | @\"a\"\n\
        Received: | @0 | ...")
    (render "{% match @0 with @\"a\" %} {% /match %}");
  check_raises "| false | true <> only false"
    (E
       "File <test>, 1:10-1:14\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: | false\n\
        Received: | false | true")
    (render "{% match true with false %} {% /match %}")

let type_error_union () =
  let open Alcotest in
  check_raises "Int tag <> string tag"
    (E
       "File <test>, 1:38-1:52\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: | {@tag: 0}\n\
        Received: | {@tag: \"a\", b: _}")
    (render
       "{% match a with {@tag: 0} %} {% with {@tag: \"a\", b} %} {{ b }}\n\
        {% /match %}");
  check_raises "Float tags don't compile."
    (E
       "File <test>, 1:17-1:28\n\
        Type error.\n\
        Only `int`, `string`, and `boolean` types may be union tags.\n\
        Received: float")
    (render "{% match a with {@tag: 1.5} %} {% /match %}");
  check_raises "Other random tags don't compile."
    (E
       "File <test>, 1:17-1:27\n\
        Type error.\n\
        Only `int`, `string`, and `boolean` types may be union tags.\n\
        Received: [_]")
    (render "{% match a with {@tag: []} %} {% /match %}");
  check_raises "Tag names must be coherent."
    (E
       "File <test>, 1:36-1:46\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: | {@a: 0}\n\
        Received: | {@b: 1, c: _}")
    (render
       "{% match a with {@a: 0} %} {% with {@b: 1, c} %} {{ c }} {% /match %}")

let vars_with_clauses () =
  let open Alcotest in
  check_raises "Variable names must be coherent."
    (E
       "File <test>, 2:16-2:17\n\
        Type error.\n\
        Variable `b` must occur in each `with` pattern.")
    (render
       "{% match a, b\n\
       \   with null, !b\n\
       \   with !a, null %}\n\
        {% with !_, !_ with null, null %}\n\
        {% /match %}");
  check_raises "Variable types must be coherent."
    (E
       "File <test>, 1:38-1:39\n\
        Type error.\n\
        Type mismatch.\n\
        Expected: int\n\
        Received: string")
    (render
       "{% match a, b with 1, \"a\" %} {% with a, _ with _, a %} {% /match %}")

let component_typechecker () =
  let open Alcotest in
  check_raises "Component names match"
    (E
       "File <test>, 1:4-1:14\n\
        Type error.\n\
        Component name mismatch.\n\
        Expected: A.\n\
        Received: B.")
    (render "{% A %} {% /B %}");
  check_raises "Child components aren't allowed in the root."
    (E
       "File <test>, 1:4-1:5\n\
        Type error.\n\
        Children are not allowed in the root template.")
    (render "{{ A }}");
  let a = Compile.Components.parse_string ~name:"A" "{{ B }}" in
  let components = Compile.Components.make [ a ] in
  let src = "{% A /%}" in
  check_raises "Missing children are reported."
    (E "File <test>, 1:4-1:7\nType error.\nMissing child: B.") (fun () ->
      ignore @@ Compile.from_string ~name:"<test>" components src);
  let src = "{% A B=#%}{%/# C=#%}{%/# /%}" in
  check_raises "Extra children are reported."
    (E "File <test>, 1:4-1:27\nType error.\nComponent A does not allow child C.")
    (fun () -> ignore @@ Compile.from_string ~name:"<test>" components src);
  check_raises "Child type mismatch."
    (E
       "File Y, 1:17-1:19\n\
        Type error.\n\
        Child type mismatch.\n\
        Expected: `child`\n\
        Received: `nullable child`") (fun () ->
      ignore
        Compile.Components.(
          make
            [
              parse_string ~name:"Z" "{{ A }}";
              parse_string ~name:"Y" "{{ A ? z }} {% Z A / %}";
            ]))

let matching_unused () =
  let open Alcotest in
  check_raises "Basic pattern (1)."
    (E "File <test>, 4:4-4:19\nMatching error.\nThis match case is unused.")
    (render
       "{% match a, b, c\n\
       \   with 10, 11, 12 %}\n\
        {% with x, 21, 22 %}\n\
        {% with 10, 11, 12 %}\n\
        {% /match %}");
  check_raises "Basic pattern (2)."
    (E "File <test>, 6:4-6:19\nMatching error.\nThis match case is unused.")
    (render
       "{% match a, b, c\n\
       \   with 10, 11, 12 %}\n\
        {% with x, 21, 22 %}\n\
        {% with 30, 31, 32 %}\n\
        {% with 30, y, 42 %}\n\
        {% with 30, 31, 42 %}\n\
        {% /match %}");
  check_raises "Nest patterns merge into wildcard patterns correctly (1)."
    (E "File <test>, 1:31-1:46\nMatching error.\nThis match case is unused.")
    (render "{% match a, b with x, y %} {% with (_, _), 40 %} {% /match %}");
  check_raises "Nest patterns merge into wildcard patterns correctly (1)."
    (E "File <test>, 4:4-4:22\nMatching error.\nThis match case is unused.")
    (render
       "{% match a, b\n\
       \   with x, 1 %}\n\
        {% with (\"a\", \"b\"), 10 %}\n\
        {% with (\"a\", \"b\"), 1 %}\n\
        {% /match %}")

let parmatch () =
  let open Alcotest in
  check_raises "Partial matching with integers."
    (E
       "File <test>, 1:4-1:55\n\
        Matching error.\n\
        This pattern-matching is not exhaustive. Here's an example of a \
        pattern which\n\
        is not matched:\n\
        _")
    (render "{% match a with 0 with 10 with 20 with 30 %} {% /match %}");
  check_raises "Partial matching with lists (1)."
    (E
       "File <test>, 1:4-1:41\n\
        Matching error.\n\
        This pattern-matching is not exhaustive. Here's an example of a \
        pattern which\n\
        is not matched:\n\
        [_, ..._]")
    (render "{% match a with [] with [_] %} {% /match %}");
  check_raises "Partial matching with lists (2)."
    (E
       "File <test>, 1:4-1:33\n\
        Matching error.\n\
        This pattern-matching is not exhaustive. Here's an example of a \
        pattern which\n\
        is not matched:\n\
        []")
    (render "{% match a with [_] %} {% /match %}");
  check_raises "Partial matching with Nullables (1)."
    (E
       "File <test>, 1:4-1:34\n\
        Matching error.\n\
        This pattern-matching is not exhaustive. Here's an example of a \
        pattern which\n\
        is not matched:\n\
        !_")
    (render "{% match a with null %} {% /match %}");
  check_raises "Partial matching with Nullables (2)."
    (E
       "File <test>, 1:4-1:32\n\
        Matching error.\n\
        This pattern-matching is not exhaustive. Here's an example of a \
        pattern which\n\
        is not matched:\n\
        null")
    (render "{% match a with !_ %} {% /match %}");
  check_raises "Partial matching with Nullables (3)."
    (E
       "File <test>, 1:4-1:48\n\
        Matching error.\n\
        This pattern-matching is not exhaustive. Here's an example of a \
        pattern which\n\
        is not matched:\n\
        !_")
    (render "{% match a with !1 %} {% with null %} {% /match %}");
  check_raises "Partial matching with enums nested in nullables."
    (E
       "File <test>, 1:4-1:58\n\
        Matching error.\n\
        This pattern-matching is not exhaustive. Here's an example of a \
        pattern which\n\
        is not matched:\n\
        !@1, _")
    (render "{% match a, b with !@1, 2 %} {% with null, _ %} {% /match %}");
  check_raises "Partial matching with records."
    (E
       "File <test>, 1:4-1:56\n\
        Matching error.\n\
        This pattern-matching is not exhaustive. Here's an example of a \
        pattern which\n\
        is not matched:\n\
        {a: _, b: _}")
    (render "{% match a with {b: 10} %} {% with {a: 20} %} {% /match %}");
  check_raises "Partial matching with dictionaries."
    (E
       "File <test>, 1:4-1:61\n\
        Matching error.\n\
        This pattern-matching is not exhaustive. Here's an example of a \
        pattern which\n\
        is not matched:\n\
        _")
    (render "{% match a with <a: true> %} {% with <a: false> %} {% /match %}");
  check_raises "Partial matching with unions (1)."
    (E
       "File <test>, 1:4-2:10\n\
        Matching error.\n\
        This pattern-matching is not exhaustive. Here's an example of a \
        pattern which\n\
        is not matched:\n\
        {@tag: 0, a: _}")
    (render
       "{% match a with {@tag: 0, a: 10} %} {% with {@tag: 1, b: 20} %}\n\
        {% /match %}");
  check_raises "Partial matching with unions (2)."
    (E
       "File <test>, 1:4-2:10\n\
        Matching error.\n\
        This pattern-matching is not exhaustive. Here's an example of a \
        pattern which\n\
        is not matched:\n\
        {@tag: false, b: _}")
    (render
       "{% match a with {@tag: true, a: 10} %}{% with {@tag: false, b: 20} %}\n\
        {% /match %}");
  check_raises "Partial matching with unions (3)."
    (E
       "File <test>, 1:4-2:10\n\
        Matching error.\n\
        This pattern-matching is not exhaustive. Here's an example of a \
        pattern which\n\
        is not matched:\n\
        {@tag: true, a: _}")
    (render
       "{% match a with {@tag: true, a: 10} %} {% with {@tag: false, b} %}\n\
        {% /match %}");
  check_raises "Partial matching with records."
    (E
       "File <test>, 1:4-3:10\n\
        Matching error.\n\
        This pattern-matching is not exhaustive. Here's an example of a \
        pattern which\n\
        is not matched:\n\
        {favoriteColor: _, firstName: _}")
    (render
       "{% match a with {firstName: name, favoriteColor: \"green\"} %}\n\
        {{ name }}'s favorite color is green.\n\
        {% /match %}")

let component_graph () =
  let open Alcotest in
  let a = Compile.Components.parse_string ~name:"A" "{% B /%}" in
  let b = Compile.Components.parse_string ~name:"B" "{% C /%}" in
  let c = Compile.Components.parse_string ~name:"C" "{% D /%}" in
  let d = Compile.Components.parse_string ~name:"D" "{% B /%}" in
  check_raises "Cyclic dependencies are reported."
    (E "Dependency cycle detected.\nA -> B -> C -> D -> B") (fun () ->
      ignore @@ Compile.Components.make [ a; b; c; d ]);
  check_raises "Missing components are reported."
    (E "Missing template: D.\nRequired by: C.") (fun () ->
      ignore @@ Compile.Components.make [ a; b; c ]);
  check_raises "Missing components are reported (by root)."
    (E "Missing template: A.\nRequired by: <test>.") (render "{% A /%}");
  check_raises "Duplicate names are reported."
    (E "Compile error.\nThere are multiple components with name `A`.")
    (fun () -> ignore @@ Compile.Components.make [ a; b; a ])

let decode_mismatch () =
  let open Alcotest in
  check_raises "Basic type mismatch."
    (E
       "Decode error.\n\
        Stack: [key: \"a\" -> key: \"b\"]\n\
        Expected type: echoable\n\
        Received value: []")
    (render "{% match a with {b} %} {{ b }} {% /match %}"
       ~json:{|{"a": {"b": []}}|});
  let json = {|{"a": "a", "b": true, "c": []}|} in
  check_raises "Map type mismatch (1)."
    (E
       "Decode error.\n\
        Stack: [key: \"a\"]\n\
        Expected type: [{a: echoable}]\n\
        Received value: \"a\"")
    (render "{% map a with {a} %}{{ a }}{% /map %}" ~json);
  check_raises "Map type mismatch (2)."
    (E
       "Decode error.\n\
        Stack: [key: \"a\"]\n\
        Expected type: [int]\n\
        Received value: \"a\"")
    (render "{% map [1, 2, ...a] with a %}{{ a }}{% /map %}" ~json);
  check_raises "Missing bindings are reported"
    (E
       "Decode error.\n\
        Stack: []\n\
        Expected type: {z: echoable}\n\
        Input is missing key: \"z\"")
    (render "{{ z }}" ~json);
  check_raises "Bad enums are reported: boolean."
    (E
       "Decode error.\n\
        Stack: [key: \"b\"]\n\
        Expected type: | false\n\
        This type does not allow the given value: true")
    (render "{% match b with false %} {% /match %}" ~json);
  check_raises "Bad enums are reported: int."
    (E
       "Decode error.\n\
        Stack: [key: \"a\"]\n\
        Expected type: | @1 | @2\n\
        This type does not allow the given value: 3")
    (render "{% match a with @1 %} {% with @2 %} {% /match %}"
       ~json:{|{"a": 3}|});
  check_raises "Bad enums are reported: int."
    (E
       "Decode error.\n\
        Stack: [key: \"a\"]\n\
        Expected type: | @\"a\" | @\"b\"\n\
        This type does not allow the given value: \"c\"")
    (render "{% match a with @\"a\" %} {% with @\"b\" %} {% /match %}"
       ~json:{|{"a": "c"}|});
  check_raises "Tuple size mismatch."
    (E
       "Decode error.\n\
        Stack: [key: \"a\"]\n\
        Expected type: (echoable, echoable)\n\
        Received value: [ \"a\" ]")
    (render "{% match a with (a, b) %} {{ a }} {{ b }} {% /match %}"
       ~json:{|{"a": ["a"]}|});
  check_raises "Bad unions are reported (1)."
    (E
       "Decode error.\n\
        Stack: [key: \"a\"]\n\
        Expected type: | {@tag: 1, a: echoable}\n\
        Received value: { \"tag\": \"a\", \"a\": \"a\" }")
    (render "{% match a with {@tag: 1, a} %} {{ a }} {% /match %}"
       ~json:{|{"a": {"tag": "a", "a": "a"}}|});
  check_raises "Bad unions are reported (2)."
    (E
       "Decode error.\n\
        Stack: [key: \"a\"]\n\
        Expected type: | {@tag: 1, a: echoable}\n\
        Received value: { \"tag\": 2, \"a\": \"a\" }")
    (render "{% match a with {@tag: 1, a} %} {{ a }} {% /match %}"
       ~json:{|{"a": {"tag": 2, "a": "a"}}|})

let () =
  let open Alcotest in
  run "Errors"
    [
      ( "Parser & lexer errors",
        [
          test_case "Illegal characters" `Quick illegal_chars;
          test_case "Illegal names" `Quick illegal_names;
          test_case "Unterminated sections" `Quick unterminated;
          test_case "Illegal escape sequences" `Quick illegal_escape;
          test_case "Bad ASTs" `Quick bad_ast;
          test_case "Duplicate fields" `Quick dup_record_field;
        ] );
      ( "Type clash",
        [
          test_case "Echoes" `Quick type_error_echo;
          test_case "Constants" `Quick type_error_const;
          test_case "Nested types" `Quick type_error_nest;
          test_case "Records" `Quick type_error_record;
          test_case "Enums" `Quick type_error_enum;
          test_case "Tagged unions" `Quick type_error_union;
        ] );
      ( "Typechecker errors",
        [
          test_case "Pattern count mismatch" `Quick pat_count_mismatch;
          test_case "Multiple names bound" `Quick dup_name_destructure;
          test_case "Variables in with clauses" `Quick vars_with_clauses;
          test_case "Component errors" `Quick component_typechecker;
        ] );
      ( "Matching errors",
        [
          test_case "Unused patterns" `Quick matching_unused;
          test_case "Partial patterns" `Quick parmatch;
        ] );
      ( "Other compile errors",
        [ test_case "Dependency graphs" `Quick component_graph ] );
      ("Render errors", [ test_case "Decode errors" `Quick decode_mismatch ]);
    ]
