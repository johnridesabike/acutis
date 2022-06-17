open Acutis
module F = Format
open Lwt.Syntax

let check = Alcotest.(check string)

module RenderLwt = Render.Make (Lwt) (DataYojson)

let render ?(components = []) src json =
  let json = Yojson.Basic.from_string json in
  let temp = Compile.(make ~filename:"" (Components.make components) src) in
  RenderLwt.make temp json

let basic _ () =
  let a =
    Compile.Components.fn ~name:"Slow" Typescheme.empty Typescheme.Child.empty
      (fun _ _ ->
        let+ () = Lwt_unix.sleep 0.05 in
        "Short sleep.")
  in
  let b =
    Compile.Components.fn ~name:"Slower" Typescheme.empty Typescheme.Child.empty
      (fun _ _ ->
        let+ () = Lwt_unix.sleep 0.1 in
        "Long sleep.")
  in
  let src = "Before. {% Slower / %} {% Slow / %} After." in
  let+ result = render ~components:[ a; b ] src "{}" in
  check "Slow components render correctly."
    "Before. Long sleep. Short sleep. After." result

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "Lwt"
       [ ("Waiting for async components", [ test_case "Basic" `Quick basic ]) ]
