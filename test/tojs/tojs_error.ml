open Acutis

let () =
  let src =
    {|
  {% match bool with false %} {% /match %}
  {% match int_enum with @1 %} {% with @2 %} {% /match %}
  {% match string_enum with @"a" %} {% with @"b" %} {% /match %}
  {% match tuple with (a, b) %} {{ a }} {{ b }} {% /match %}
|}
  in
  ToJs.pp Format.std_formatter
    Compile.(from_string_nolink ~fname:"<test>" Components.empty src)
