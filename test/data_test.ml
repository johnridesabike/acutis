open Acutis
module F = Format
open Js_of_ocaml

let check = Alcotest.(check string)

module RenderJson = Render.Make (Sync) (DataYojson)
module RenderJs = Render.Make (Sync) (DataJs)

let render_js ?(components = []) src js =
  let temp = Compile.(make ~filename:"" (Components.make components) src) in
  RenderJs.make temp (Js.Unsafe.inject js)

let render_yojson ?(components = []) src json =
  let json = Yojson.Basic.from_string json in
  let temp = Compile.(make ~filename:"" (Components.make components) src) in
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
     {% with {a, b, c} %} {{ a }} {{ b }} {{ c }}\n\
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
  let src = "{% match a with (a, b) %} {{ a }} {{ b }} {% /match %}" in
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
    ]
