open Acutis
open Js_of_ocaml

(*
let () =
  let src = {|{{ a }} {{ "b" }} {{ d ? "f" }}|} in
  let template = Compile.make ~filename:"<main>" Compile.Components.empty src in
  print_endline @@ Render.sync template [ ("a", `String "a"); ("d", `Int 1) ]
*)
let _ =
  Js.export "myMathLib"
    (object%js
       method add x y = x +. y
       method abs x = abs_float x
       val zero = 0.

       method lmao src =
         Compile.make ~filename:"test" Compile.Components.empty
           (Js.to_string src)

       method lol template json = Render.sync template json
    end)
