open Acutis
module RenderSync = Render.Make (Sync) (Acutis_json.Data)

let render ?(components = []) src json =
  let json = Yojson.Basic.from_string json in
  let temp =
    Compile.(from_string ~fname:"<test>" (Components.make components) src)
  in
  RenderSync.make temp json

let print_result title s =
  print_endline title;
  print_endline "---";
  print_endline s;
  print_newline ()

let () =
  print_result "Echoes work"
    (render
       {|{{ a }} {{ b }}! {{ c }} {{{ c }}} {{ "<" }} {{{ "<" }}} {{ "d" }}|}
       {|{"a": "Hello", "b": "World", "c": "&\"'></`="}|});
  print_result "Echo formats work"
    (render
       "%i: {{ %i num }} %f: {{ %f frac }} %b: {{ %b binaryf }} {{ %b binaryt \
        }}"
       {|{
          "num": 123456,
          "frac": 1234.56789,
          "binaryf": false,
          "binaryt": true
          }|});
  print_result "Unbound variables default to null."
    (render
       {|{% match a
            with !a %} {{ a }}
          {% with null ~%} a doesn't exist.
          {%~ /match %}|}
       "{}");
  print_result
    "Prefixing a name with an underscore suppresses unused-variable warnings."
    (render "{% match a with {x: _x} ~%} x is ignored. {%~ /match %}"
       {|{"a": {"x": null}}|});
  print_result "Whitespace control works (1)"
    (render
       "{% match a with {b: {c}} ~%}\n\t  _ {{~ c ~}} _\t \r  {%~ /match %}"
       {|{"a": {"b": {"c": "hi"}}}|});
  print_result "Whitespace control works (2)"
    (render
       ~components:
         [
           Compile.Components.parse_string ~fname:"ohHai.acutis" ~name:"OhHai"
             "{{ children }} Oh hai {{ name }}.";
         ]
       {|{% OhHai name="Mark" ~%} I did not. {%~ /OhHai %}
         {%~ OhHai name="Lisa" children=#%} Cheep cheep cheep. {%~# / %}|}
       "{}");
  print_result "Nullish coalescing works"
    (render
       ~components:
         [
           Compile.Components.parse_string ~fname:"comp.acutis" ~name:"Comp"
             "{{ a ? b ? c ? z }}";
         ]
       {|{{ a ? b ? c }} {% Comp b z=#%}z{%# / %} {% Comp z=#%}z{%# / %}|}
       {|{"b": "b", "c": "c"}|});
  print_result "Appending list literals works"
    (render {|{%~ map ["a", "b", ...["c", "d"]] with x ~%} {{ x }} {% /map %}|}
       "{}");
  print_result "Record field access works"
    (render
       {|
      {%~ match {x: a.b."c".d} with {x: true} ~%} pass {% with _ %}{% /match ~%}
      {{~ a.e.f.z ? a.e."f".g ~}}|}
       {|{"a": {"b": {"c": {"d": true}}, "e": {"f": {"g": "pass"}}}}|});
  let props = {|{"a": [{"name": "John"}, {"name": "Carlo"}]}|} in
  print_result "Mapping list variables works"
    (render {|{% map a with {name} %} {{~ name }} {% /map %}|} props);
  print_result "Mapping list literals works"
    (render
       {|{% map [{"name": "John"}, {"name": "Carlo"}] with {name} %}
          {{~ name }} {% /map %}|}
       "{}");
  print_result "Mapping list variables appended with literals works"
    (render
       {|{% map [{"name": "Paul"}, ...a] with {name} %} {{~ name }} {% /map %}|}
       props);
  print_result "Mapping lists with the index works"
    (render
       {|{% map a
         with {name}, 0 %} {{~ name ~}}
      {% with {name}    %} {{ name ~}} {% /map %}|}
       props);
  let props =
    {|{"people": {"Tommy": {"job": "banking"}, "Lisa": {"job": "computers"}}}|}
  in
  print_result "Mapping dictionary variables works"
    (render
       {|{% map_dict people
        with {job}, key ~%} {{ key }}: {{ job }}. {% /map_dict %}|}
       props);
  print_result "Mapping dictionary literals works"
    (render
       {|{% map_dict <"Tommy": {"job": "banking"}, "Lisa": {"job": "computers"}>
        with {job}, key ~%} {{ key }}: {{ job }}. {% /map_dict %}|}
       props);
  print_result "Nullable props default to null"
    (render
       ~components:
         [
           Compile.Components.parse_string ~fname:"a.acutis" ~name:"A"
             {|{{ x ? "fail" }} {{ y ? "pass"}}|};
         ]
       {|{% A x=!"pass" / %}|} "{}");
  print_result "The default [children] child works"
    (render
       ~components:
         [
           Compile.Components.parse_string ~fname:"a.acutis" ~name:"A"
             "{{ children }}";
         ]
       "{% A ~%} a {%~ /A %} {% A children=#~%} b {%~# / %}" "{}");
  let x =
    Compile.Components.parse_string ~fname:"x.acutis" ~name:"X"
      "{{ passthroughChild }}"
  in
  let y =
    Compile.Components.parse_string ~fname:"y.acutis" ~name:"Y"
      "{% X passthroughChild=a / %}"
  in
  print_result "Children are passed correctly"
    (render ~components:[ x; y ] "{% Y a=#~%} a {%~# / %}" "{}");
  let y =
    Compile.Components.parse_string ~fname:"y.acutis" ~name:"Y"
      "{% X passthroughChild / %}"
  in
  print_result "Children are passed correctly (with punning)"
    (render ~components:[ x; y ] "{% Y passthroughChild=#~%} a {%~# / %}" "{}");
  print_result "Tagged unions work"
    (render
       {| {%~ map [a, b]
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
       {|{
          "a": {"tag": 0,     "a": "a"},
          "b": {"tag": 1,     "a": 1},
          "c": {"tag": "a",   "a": "a"},
          "d": {"tag": "b",   "a": 1},
          "e": {"tag": true,  "a": "a"},
          "f": {"tag": false, "a": 1}
        }|});
  print_result "Decoding open tagged unions works"
    (render
       {| {%~ match a
              with {@tag: "something", x}
              with {@tag: "else", x} %} fail {{ x }}
          {%  with _ ~%} success
          {%~ /match ~%}|}
       {|{"a": {"tag": "unexpected", "a": "a"}}|});
  print_result "Constructing values works"
    (render
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
       "{}");
  print_result "Constructing nested template blocks inside data patterns works"
    (render
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
       {|{"b": true}|})
