open Acutis
module F = Format

let check = Alcotest.(check string)

module RenderSync = Render.Make (Sync) (Acutis_json.Data)

let render ?(components = []) src json =
  let json = Yojson.Basic.from_string json in
  let temp =
    Compile.(from_string ~fname:"<test>" (Components.make components) src)
  in
  RenderSync.make temp json

let basic () =
  let props = {|{
  "a": "Hello",
  "b": "World",
  "c": "&\"'></`="
  }|} in
  let src = {|{{ a }} {{ b }}! {{ c }} {{ &c }} {{ "<" }} {{ "d" }}|} in
  check "Echoes work"
    "Hello World! &amp;&quot;&apos;&gt;&lt;&#x2F;&#x60;&#x3D; &\"'></`= < d"
    (render src props)

let unbound_vars () =
  let src =
    {|{% match a
          with !a %} {{ a }}
        {% with null ~%} a doesn't exist.
      {%~ /match %}|}
  in
  check "Unbound variables default to null." "a doesn't exist."
    (render src "{}")

let ignore_bindings () =
  let src = "{% match a with {x: _x} ~%} x is ignored. {%~ /match %}" in
  check
    "Prefixing a name with an underscore suppresses unused-variable warnings."
    "x is ignored."
    (render src "{\"a\": {\"x\": null}}")

let whitespace () =
  let props = {|{"a": {"b": {"c": "hi"}}}|} in
  let src =
    "{% match a with {b: {c}} ~%}\n\t  _ {{~ c ~}} _\t \r  {%~ /match %}"
  in
  check "Whitespace control works (1)" "_hi_" (render src props);
  let oh_hai =
    Compile.Components.parse_string ~fname:"ohHai.acutis" ~name:"OhHai"
      "{{ children }} Oh hai {{ name }}."
  in
  let src =
    {|{% OhHai name="Mark" ~%} I did not. {%~ /OhHai %}
      {%~ OhHai name="Lisa" children=#%} Cheep cheep cheep. {%~# / %}|}
  in
  check "Whitespace control works (2)"
    "I did not. Oh hai Mark. Cheep cheep cheep. Oh hai Lisa."
    (render ~components:[ oh_hai ] src "{}")

let nullish_coalescing () =
  let props = {|{"b": "b", "c": "c"}|} in
  let comp =
    Compile.Components.parse_string ~fname:"comp.acutis" ~name:"Comp"
      "{{ a ? b ? c ? z }}"
  in
  let src =
    {|{{ a ? b ? c }} {% Comp b z=#%}z{%# / %} {% Comp z=#%}z{%# / %}|}
  in
  check "Nullish coalescing works" "b b z"
    (render ~components:[ comp ] src props)

let list_literal_append () =
  let src =
    {|{%~ map ["a", "b", ...["c", "d"]] with x ~%} {{ x }} {% /map %}|}
  in
  check "Appending list literals works" "a b c d " (render src "{}")

let map_list () =
  let props = {|{"a": [{"name": "John"}, {"name": "Carlo"}]}|} in
  let src = {|{% map a with {name} %} {{~ name }} {% /map %}|} in
  check "Mapping list variables works" "John Carlo " (render src props);
  let src =
    {|{% map [{"name": "John"}, {"name": "Carlo"}] with {name} %}
      {{~ name }} {% /map %}|}
  in
  check "Mapping list literals works" "John Carlo " (render src "{}");
  let src =
    {|{% map [{"name": "Paul"}, ...a] with {name} %} {{~ name }} {% /map %}|}
  in
  check "Mapping list variables appended with literals works" "Paul John Carlo "
    (render src props);
  let src =
    {|{% map a
         with {name}, 0 %} {{~ name ~}}
      {% with {name}    %} {{ name ~}} {% /map %}|}
  in
  check "Mapping lists with the index works" "John Carlo" (render src props)

let map_dict () =
  let props =
    {|{
      "people": {
        "Tommy": {"job": "banking"},
        "Lisa": {"job": "computers"}
        }
      }|}
  in
  let src =
    {|{% map_dict people
        with {job}, key ~%} {{ key }}: {{ job }}. {% /map_dict %}|}
  in
  check "Mapping dictionary variables works" "Lisa: computers. Tommy: banking. "
    (render src props);
  let src =
    {|{% map_dict <"Tommy": {"job": "banking"}, "Lisa": {"job": "computers"}>
        with {job}, key ~%} {{ key }}: {{ job }}. {% /map_dict %}|}
  in
  check "Mapping dictionary literals works" "Lisa: computers. Tommy: banking. "
    (render src props)

let nullable_props () =
  let a =
    Compile.Components.parse_string ~fname:"a.acutis" ~name:"A"
      {|{{ x ? "fail" }} {{ y ? "pass"}}|}
  in
  let src = {|{% A x=!"pass" / %}|} in
  check "Nullable props default to null" "pass pass"
    (render ~components:[ a ] src "{}")

let default_children () =
  let a =
    Compile.Components.parse_string ~fname:"a.acutis" ~name:"A" "{{ children }}"
  in
  let src = "{% A ~%} a {%~ /A %} {% A children=#~%} b {%~# / %}" in
  check "The default [children] child works" "a b"
    (render ~components:[ a ] src "{}")

let template_sections () =
  let x =
    Compile.Components.parse_string ~fname:"x.acutis" ~name:"X"
      "{{ passthroughChild }}"
  in
  let y =
    Compile.Components.parse_string ~fname:"y.acutis" ~name:"Y"
      "{% X passthroughChild=a / %}"
  in
  let src = "{% Y a=#~%} a {%~# / %}" in
  check "Children are passed correctly" "a"
    (render ~components:[ x; y ] src "{}");
  let y =
    Compile.Components.parse_string ~fname:"y.acutis" ~name:"Y"
      "{% X passthroughChild / %}"
  in
  let src = "{% Y passthroughChild=#~%} a {%~# / %}" in
  check "Children are passed correctly (with punning)" "a"
    (render ~components:[ x; y ] src "{}")

let tagged_unions () =
  let props =
    {|{
      "a": {"tag": 0,     "a": "a"},
      "b": {"tag": 1,     "a": 1},
      "c": {"tag": "a",   "a": "a"},
      "d": {"tag": "b",   "a": 1},
      "e": {"tag": true,  "a": "a"},
      "f": {"tag": false, "a": 1}
    }|}
  in
  let src =
    {|
    {%~ map [a, b]
        with {@tag: 0, a: "a"}
        with {@tag: 1, a: 1} ~%} success {%
        with _ %} fail {%
    /map ~%}
    {%~ map [c, d]
        with {@tag: "a", a: "a"}
        with {@tag: "b", a: 1} ~%} success {%
        with _ %} fail {%
    /map ~%}
    {%~ map [e, f]
        with {@tag: true, a: "a"}
        with {@tag: false, a: 1} ~%} success {%
        with _ %} fail {%
    /map ~%}|}
  in
  check "Tagged unions work" "success success success success success success "
    (render src props)

let constructing_values () =
  let src =
    {|{%~
      match [(1, 2), (3, 4)],
            [@"y", @"z"],
            [@99, @100],
            [!{@tag: "a", a: 1.5}, null],
            {@tag: 0, a: !"z"},
            <a: "a">
      with  [(1, 2), (3, 4)],
            [@"y", @"z"],
            [@99, @100],
            [!{@tag: "a", a: 1.5}, null],
            {@tag: 0, a: !"z"},
            <a: "a">
    ~%} success {%~
      with _, _, _, _, _, _ %} fail {%
      /match ~%}|}
  in
  check "Constructing values works" "success" (render src "{}")

let nested_blocks_data () =
  let src =
    {|{%~
        match
          {
            a:
              !#~%}
                {% match b with true ~%} yes {%~ with false %} {% /match %}
              {%~#
          }
        with {a: !"yes"} ~%} success
      {%~ with _ %} fail
      {% /match %}|}
  in
  check "Constructing nested template blocks inside data patterns works"
    "success"
    (render src {|{"b": true}|})

let () =
  let open Alcotest in
  run "Rendering"
    [
      ( "Basic rendering",
        [
          test_case "Basic" `Quick basic;
          test_case "Unbound variables" `Quick unbound_vars;
          test_case "Ignoring bindings" `Quick ignore_bindings;
          test_case "Whitespace control" `Quick whitespace;
          test_case "Nullish coalescing" `Quick nullish_coalescing;
          test_case "Append list literal" `Quick list_literal_append;
        ] );
      ( "Mapping",
        [
          test_case "map_list" `Quick map_list;
          test_case "map_dict" `Quick map_dict;
        ] );
      ( "Components",
        [
          test_case "Components" `Quick nullable_props;
          test_case "Default Children" `Quick default_children;
          test_case "Template sections" `Quick template_sections;
        ] );
      ( "Advanced cases",
        [
          test_case "Tagged unions" `Quick tagged_unions;
          test_case "Constructing values" `Quick constructing_values;
          test_case "Nested blocks in data" `Quick nested_blocks_data;
        ] );
    ]
