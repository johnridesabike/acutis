open Acutis

let () =
  let component =
    Compile.Components.parse_string ~fname:"Component" ~name:"Component"
      {|
{%~ interface
  optional = ?int
  children = string
  list = [int]
/ ~%}
{{ %i optional ? children }}
{% map list with i ~%} {{ %i i }} {%~ /map ~%}
|}
  in
  let external_component =
    Compile.(
      Components.from_fun ~name:"Stringify"
        (interface_from_string ~fname:"foo"
           "record = {int_enum: @8 | @40, string_enum: @\"yes\" | @\"no\"}\n\
            tagged_record_bool =\n\
           \    {@tag: false, a: string}\n\
           \  | {@tag: true, b: int}\n\
            null_string_dict = <?string>\n\
            int_list = [int]\n\
            tagged_record_int =\n\
           \    {@tag: 0}\n\
           \  | {@tag: 1, tuple: (float, string, false | true)}\n\
            tagged_record_string =\n\
           \    {@tag: \"a\", record_list: [{name: string, job: string}]}\n\
           \  | {@tag: \"b\", open_enum: @0 | @1 | ...}\n\
            tagged_record_open =\n\
           \    {@tag: 100, a: int}\n\
           \  | {@tag: 200, b: string}\n\
           \  | {@tag: 300, c: float}\n\
           \  | ...\n\
            unknown = _\n\
            nested_list = [[[int]]]\n\
            nested_nullable_list = [??false | true]\n")
        {
          module_path = "./fixture_components.mjs";
          function_path = "stringify";
        })
  in
  let another_component_same_file =
    Compile.(
      Components.from_fun ~name:"Another" Map.String.empty
        {
          module_path = "./fixture_components.mjs";
          function_path = "another_function";
        })
  in
  let src =
    {|
{%~ interface
big_int = int
big_float = float
bool1 = false | true
bool2 = true | false
dangerous = string
record = {int_enum: @8 | @40, string_enum: @"yes" | @"no"}
tagged_record_bool =
    {@tag: false, a: string}
  | {@tag: true, b: int}
null_string_dict = <?string>
int_list = [int]
tagged_record_int =
    {@tag: 0}
  | {@tag: 1, tuple: (float, string, false | true)}
tagged_record_string =
    {@tag: "a", record_list: [{name: string, job: string}]}
  | {@tag: "b", open_enum: @0 | @1 | ...}
tagged_record_open =
    {@tag: 100, a: int}
  | {@tag: 200, b: string}
  | {@tag: 300, c: float}
  | ...
unknown = _
nested_list = [[[int]]]
nested_nullable_list = [??false | true]
/ ~%}

Formatters
----------

%i    {{ %i big_int }}
%,i   {{ %,i big_int }}
%f    {{ %f big_float }}
%2f   {{ %.2f big_float }}
%e    {{ %e big_float }}
%.2e  {{ %.2e big_float }}
%g    {{ %g big_float }}
%.2g  {{ %.2g big_float }}
%b    {{ %b bool1 }}
%b    {{ %b bool2 }}

Escaping
--------

Escaped     {{ dangerous }}
Not escaped {{{ dangerous }}}

Matching
--------

{% match record.int_enum 
  with @8 ~%} 8
{% with @40 ~%} 40
{% /match ~%}

{% match record
  with {string_enum: @"yes"} ~%} yes
{% with {string_enum: @"no"} ~%} no
{% /match ~%}

{% match tagged_record_bool
  with {@tag: false, a} ~%} {{ a }}
{% with {@tag: true, b} ~%} {{ %i b }}
{% /match ~%}

{% match tagged_record_int
  with {@tag: 0} ~%} Fail
{% with {@tag: 1, tuple: (a, b, c)} ~%} {{ %f a }} {{ b }} {{ %b c }}
{% /match ~%}

{% match tagged_record_open with {@tag: 200, b} %} {{ b }}
{% with _ ~%} Another tag!
{% /match %}

Mapping
-------

{% map_dict null_string_dict
  with null, key ~%} {{ key }} is null.
{% with !str, key ~%} {{ key }} is {{ str }}
{% /map_dict ~%}

{% map int_list with i ~%}
  {{ %i i }}
{% /map ~%}

{% map int_list with i, key ~%}
  {{ %i key}} : {{ %i i }}
{% /map ~%}

{% map nested_list with l %}
  {%~ map l with l2 %}
    {%~ map l2 with i ~%} {{ %i i }} {% /map ~%}
  {% /map ~%}
{% /map %}

{% map nested_nullable_list
  with null ~%} Level 1 null
{% with !null ~%} Level 2 null (This shouldn't render.)
{% with !!b ~%} Level 3 {{ %b b }}
{% /map %}

Constructing async blocks
-------------------------

{% match {a: #%} Nested block {%~#, b: #%} Another nested block {%~#}
  with {a, b} ~%} {{ a }} {{ b }}
{% /match ~%}

Component
---------

{% Component list=[1, 2, 3] ~%} Children prop {%~ /Component %}

Complicated pattern matching
----------------------------

{% match 1, null, 3
    with 1,  _,   0 %} 0
{%  with x, !1,   0 %} 1 {{ %i x }}
{%  with _, null, y %} 2 {{ %i y }}
{%  with 1, !1,   1 %} 3
{%  with _, _,    _ %} 4
{% /match ~%}

{% match ((10, 20), 30), 40
   with              _x, 41 %}
{% with  ((10, 20), 30), 40 %} Pass
{% with              _y,  z %} {{ %i z }}
{% /match ~%}

{% match ((10, 20), 99), 40
   with              _x, 41 %}
{% with  ((10, 20), 30), 40 %} Fail
{% with              _y,  z %} {{ %i z }}
{% /match ~%}

String encoding
---------------

😇👨‍💻😇
\" \ \ \"

External JavaScript template component: stringify arbitrary data

{% Stringify
  record
  tagged_record_bool
  null_string_dict
  int_list
  tagged_record_int
  tagged_record_string
  tagged_record_open
  unknown
  nested_list
  nested_nullable_list
/ %}
|}
  in
  ToJs.pp Format.std_formatter
    Compile.(
      from_string_nolink ~fname:"<test>"
        (Components.make
           [ component; external_component; another_component_same_file ])
        src)
