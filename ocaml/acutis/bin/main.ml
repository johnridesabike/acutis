open Acutis

let () =
  let src = {|{{ a }} {{ "b" }} {{ d ? "f" }}|} in
  let template = Compile.make ~filename:"<main>" Compile.Components.empty src in
  print_endline @@ Render.sync template [ ("a", `String "a"); ("d", `Int 1) ]
