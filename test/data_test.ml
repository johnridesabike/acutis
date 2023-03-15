open Acutis
module F = Format
open Js_of_ocaml

let check = Alcotest.(check string)

module RenderJson = Render.Make (Sync) (Acutis_json.Data)
module RenderJs = Render.Make (Sync) (Acutis_js.Data)

let render_js ?(components = []) src js =
  let temp =
    Compile.(from_string ~fname:"<test>" (Components.make components) src)
  in
  RenderJs.make temp (Js.Unsafe.inject js)

let render_yojson ?(components = []) src json =
  let json = Yojson.Basic.from_string json in
  let temp =
    Compile.(from_string ~fname:"<test>" (Components.make components) src)
  in
  RenderJson.make temp json

let records () =
  let props_js =
    object%js
      val a =
        object%js
          val a = Js.string "a"
          val b = Js.number_of_float 1.0
          val c = Js.number_of_float 1.0
        end
    end
  in
  let props_json = {|{"a": {"a": "a", "b": 1, "c": 1.0}}|} in
  let src =
    "{% match a with {a: \"z\", b: 0, c: 0.0} %}\n\
     {% with {a, b, c} %} {{ a }} {{ %i b }} {{ %g c }}\n\
     {%~ /match %}"
  in
  let result = " a 1 1" in
  check "Record 1 yojoson" result (render_yojson src props_json);
  check "Record 1 js" result (render_js src props_js)

let dicts () =
  let props_js =
    object%js
      val a =
        object%js
          val a = Js.string "a"
          val b = Js.string "b"
          val c = Js.string "c"
        end
    end
  in
  let props_json = {|{"a": {"a": "a", "b": "b", "c": "c"}}|} in
  let src = "{% map_dict a with a %} {{ a }} {% /map_dict %}" in
  let result = " a  b  c " in
  check "Dict 1 yojoson" result (render_yojson src props_json);
  check "Dict 1 js" result (render_js src props_js)

let lists () =
  let props_js =
    object%js
      val a =
        Js.(array [| Unsafe.inject (string "a"); Unsafe.inject (string "b") |])
    end
  in
  let props_json = {|{"a": ["a", "b"]}|} in
  let src = "{% map a with a %} {{ a }} {%~ /map %}" in
  let result = " a b" in
  check "List 1 yojoson" result (render_yojson src props_json);
  check "List 1 js" result (render_js src props_js)

let nullables () =
  let props_js =
    object%js
      val a = Js.(array [| Unsafe.inject (string "a"); Unsafe.inject null |])
    end
  in
  let props_json = {|{"a": ["a", null]}|} in
  let src = "{% map a with !a %} {{ a }} {%~ with null %} null {%~ /map %}" in
  let result = " a null" in
  check "List 1 yojoson" result (render_yojson src props_json);
  check "List 1 js" result (render_js src props_js)

let tuples () =
  let props_js =
    Js.(
      object%js
        val a =
          array
            [|
              Unsafe.inject (string "a"); Unsafe.inject (number_of_float 1.0);
            |]
      end)
  in
  let props_json = {|{"a": ["a", 1.0]}|} in
  let src = "{% match a with (a, b) %} {{ a }} {{ %g b }} {% /match %}" in
  let result = " a 1 " in
  check "List 1 yojoson" result (render_yojson src props_json);
  check "List 1 js" result (render_js src props_js)

let booleans () =
  let props_js =
    Js.(
      object%js
        val a = _true
        val b = _false
      end)
  in
  let props_json = {|{"a": true, "b": false}|} in
  let src =
    "{% match a, b with true, false %} pass {% with _, _ %} fail {% /match %}"
  in
  let result = " pass " in
  check "Booleans 1 yojoson" result (render_yojson src props_json);
  check "Booleans 1 js" result (render_js src props_js)

let unions () =
  let props_js =
    Js.(
      object%js
        val a =
          object%js
            val tag = number_of_float 0.0
            val a = string "pass"
          end
      end)
  in
  let props_json = {|{"a": {"tag": 0, "a": "pass"}}|} in
  let src =
    "{% match a with {@tag: 0, a} ~%} {{ a }} \n\
     {%~ with {@tag: 1} %} fail {% /match %}"
  in
  let result = "pass" in
  check "Unions 1 yojoson" result (render_yojson src props_json);
  check "Unions 1 js" result (render_js src props_js)

let unknown () =
  let typescheme =
    Typescheme.(
      make
        [
          ("dict", unknown ());
          ("arr", unknown ());
          ("arrEmpty", unknown ());
          ("some", unknown ());
          ("none", unknown ());
          ("t", unknown ());
          ("f", unknown ());
        ])
  in
  let js_comp =
    Compile.Components.from_fun ~name:"Comp" typescheme (fun x ->
        Js._JSON##stringify x |> Js.to_string)
  in
  let json_comp =
    Compile.Components.from_fun ~name:"Comp" typescheme (fun x ->
        Yojson.Basic.to_string x)
  in
  let js_props =
    Js.(
      object%js
        val dict =
          object%js
            val a = number_of_float 0.0
            val b = string "b"
          end

        val arr = Js.array [| string "x"; string "y" |]
        val arrEmpty = Js.array [||]
        val some = string "some"
        val none = null
        val t = _true
        val f = _false
      end)
  in
  let json_props =
    {|{
        "dict": {"a": 0, "b": "b"},
        "arr": ["x", "y"],
        "arrEmpty": [],
        "some": "some",
        "none": null,
        "t": true,
        "f": false
      }|}
  in
  let src =
    {|{% match dict with {a: 0, b: "b"}%}{% with _ %}{% /match ~%}
  {% match arr with ["x"] %}{% with _ %}{% /match ~%}
  {% match arrEmpty with ["x"] %}{% with _ %}{% /match ~%}
  {% match some with !"some" %}{% with _ %}{% /match ~%}
  {% match none with !"some" %}{% with _ %}{% /match ~%}
  {% match t with true %}{% with false %}{% /match ~%}
  {% match f with true %}{% with false %}{% /match ~%}
  {% Comp dict arr arrEmpty some none t f / %}|}
  in
  let result =
    {|{"arr":["x",["y",null]],"arrEmpty":null,"dict":{"a":0,"b":"b"},"f":0,"none":null,"some":["some"],"t":1}|}
  in
  check "Encoding unknown JS data works" result
    (render_js src ~components:[ js_comp ] js_props);
  check "Encoding unknown JSON data works" result
    (render_yojson src ~components:[ json_comp ] json_props)

let () =
  let open Alcotest in
  run "Data"
    [
      ( "Data",
        [
          test_case "Records" `Quick records;
          test_case "Dicts" `Quick dicts;
          test_case "Lists" `Quick lists;
          test_case "Nullables" `Quick nullables;
          test_case "Tuples" `Quick tuples;
          test_case "Booleans" `Quick booleans;
          test_case "Unions" `Quick unions;
        ] );
      ( "Unknown data in function components",
        [ test_case "Unknown data" `Quick unknown ] );
    ]
