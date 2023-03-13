open Acutis
module RenderSync = Render.Make (Sync) (Acutis_json.Data)

let render ?(json = "{}") src () =
  let temp = Compile.(from_string ~fname:"<test>" Components.empty src) in
  let json = Yojson.Basic.from_string json in
  ignore @@ RenderSync.make temp json

exception E = Error.Acutis_error

let illegal_chars () =
  let open Alcotest in
  check_raises "Illegal character 1"
    (E "File \"<test>\", 1:9-1:10\nSyntax error.\n") (render "{% match*");
  check_raises "Illegal character 2"
    (E "File \"<test>\", 1:4-1:5\nSyntax error.\n") (render "{% +a %}");
  check_raises "Illegal character 3"
    (E "File \"<test>\", 1:12-1:13\nSyntax error.\n") (render "{% match a &%}");
  check_raises "Illegal character 4"
    (E "File \"<test>\", 1:6-1:7\nSyntax error.\n") (render "{{ a &%}")

let number_parsing () =
  let open Alcotest in
  check_raises "A number greater than the maximum integer is a syntax error."
    (E "File \"<test>\", 1:10-1:29\nSyntax error.\n")
    (render "{% match 9999999999999999999")

let illegal_names () =
  let open Alcotest in
  check_raises "Illegal name: echo null"
    (E "File \"<test>\", 1:4-1:8\nParse error.\nThis is not a valid echo.\n")
    (render "{{ null }}");
  check_raises "Illegal name: echo false"
    (E "File \"<test>\", 1:4-1:9\nParse error.\nThis is not a valid echo.\n")
    (render "{{ false }}");
  check_raises "Illegal name: echo true"
    (E "File \"<test>\", 1:4-1:8\nParse error.\nThis is not a valid echo.\n")
    (render "{{ true }}");
  check_raises "Illegal name: _"
    (E
       "File \"<test>\", 1:9-1:10\n\
        Type error.\n\
        Underscore ('_') is not a valid name.")
    (render "{% map [_] with x %} {{ x }} {% /map %}")

let unterminated () =
  let open Alcotest in
  check_raises "Unterminated strings"
    (E "File \"<test>\", 1:6-1:6\nSyntax error.\n") (render {|{{ "a|});
  check_raises "Unterminated comments"
    (E "File \"<test>\", 1:11-1:11\nSyntax error.\n") (render "{* {* a *}");
  check_raises "Unterminated expressions"
    (E "File \"<test>\", 1:9-1:9\nSyntax error.\n") (render "{% match")

let illegal_escape () =
  let open Alcotest in
  check_raises "Illegal escape sequence"
    (E "File \"<test>\", 1:5-1:6\nSyntax error.\n") (render {|{{ "\a" }}|})

let parser_errors () =
  let open Alcotest in
  check_raises "Missing a closing component name"
    (E "File \"<test>\", 1:19-1:20\nParse error.\nExpected a component name.\n")
    (render "{% A %} abcd {% / a %}");

  check_raises "Unclosed components (this one is confusing.)"
    (E "File \"<test>\", 1:16-1:16\nParse error.\nUnclosed component.\n")
    (render "{% A /A %} abcd");

  check_raises "Unexpected tokens (1)"
    (E
       "File \"<test>\", 1:4-1:5\n\
        Parse error.\n\
        Expected a '%}', 'match', 'map', 'map_dict', or a component name.\n")
    (render "{% a %}");
  check_raises "Unexpected tokens (2)"
    (E
       "File \"<test>\", 1:5-1:6\n\
        Parse error.\n\
        Expected a '%}', 'match', 'map', 'map_dict', or a component name.\n")
    (render "{%~ a %}");
  check_raises "Unexpected tokens (3)"
    (E
       "File \"<test>\", 1:5-1:9\n\
        Parse error.\n\
        Expected a '%}', 'match', 'map', 'map_dict', or a component name.\n")
    (render "{%~ with %}");
  check_raises "Unexpected tokens (4)"
    (E
       "File \"<test>\", 1:10-1:11\n\
        Parse error.\n\
        Expected a '%}', 'match', 'map', 'map_dict', or a component name.\n")
    (render "{% %}{%~ a %}");
  check_raises "Unexpected tokens (5)"
    (E "File \"<test>\", 1:40-1:41\nParse error.\nExpected a '%}'.\n")
    (render "{% A %} {% match a with _ %} {% /match /A %}");
  check_raises "Unexpected tokens (inside # blocks)"
    (E
       "File \"<test>\", 1:20-1:24\n\
        Parse error.\n\
        Expected a '%}', '#', 'match', 'map', 'map_dict', or a component name.\n")
    (render "{% A a=#%} abcd {% with /A %}");

  check_raises "Bad pattern (1)"
    (E
       "File \"<test>\", 1:10-1:13\n\
        Parse error.\n\
        This is not a valid pattern.\n")
    (render "{% match ABC with a %} {% /match %}");
  check_raises "Bad pattern (2)"
    (E
       "File \"<test>\", 1:11-1:14\n\
        Parse error.\n\
        This is not a valid pattern.\n")
    (render "{% match !ABC with a %} {% /match %}");
  check_raises "Bad pattern (3)"
    (E
       "File \"<test>\", 1:11-1:14\n\
        Parse error.\n\
        This is not a valid pattern.\n")
    (render "{% match !ABC with a %} {% /match %}");
  check_raises "Bad pattern (4)"
    (E
       "File \"<test>\", 1:11-1:14\n\
        Parse error.\n\
        This is not a valid pattern.\n")
    (render "{% match @ABC with a %} {% /match %}");
  check_raises "Bad pattern (5)"
    (E
       "File \"<test>\", 1:13-1:16\n\
        Parse error.\n\
        This is not a valid pattern.\n")
    (render "{% match a, ABC with a %} {% /match %}");
  check_raises "Bad pattern (6)"
    (E
       "File \"<test>\", 1:13-1:16\n\
        Parse error.\n\
        This is not a valid pattern.\n")
    (render "{% map_dict ABC with a %} {% /map_dict %}");
  check_raises "Bad pattern (7)"
    (E "File \"<test>\", 1:8-1:11\nParse error.\nThis is not a valid pattern.\n")
    (render "{% map ABC with a %} {% /map %}");
  check_raises "Bad pattern (8)"
    (E
       "File \"<test>\", 1:14-1:18\n\
        Parse error.\n\
        This is not a valid pattern.\n")
    (render "{% match {a: with %} {% /match %}");
  check_raises "Bad pattern (9)"
    (E
       "File \"<test>\", 1:12-1:16\n\
        Parse error.\n\
        This is not a valid pattern.\n")
    (render "{% match ( with %} {% /match %}");
  check_raises "Bad pattern (10)"
    (E
       "File \"<test>\", 1:12-1:16\n\
        Parse error.\n\
        This is not a valid pattern.\n")
    (render "{% match [ with %} {% /match %}");
  check_raises "Bad pattern (11)"
    (E
       "File \"<test>\", 1:15-1:19\n\
        Parse error.\n\
        This is not a valid pattern.\n")
    (render "{% match [... with %} {% /match %}");
  check_raises "Bad pattern (12)"
    (E
       "File \"<test>\", 1:14-1:18\n\
        Parse error.\n\
        This is not a valid pattern.\n")
    (render "{% match [a, with %} {% /match %}");
  check_raises "Bad pattern (13)"
    (E
       "File \"<test>\", 1:18-1:22\n\
        Parse error.\n\
        This is not a valid pattern.\n")
    (render "{% match [a, ... with %} {% /match %}");
  check_raises "Unclosed < (4)"
    (E
       "File \"<test>\", 1:14-1:18\n\
        Parse error.\n\
        This is not a valid pattern.\n")
    (render "{% match <a: with %} {% /match %}");
  check_raises "Illegal pattern after with"
    (E
       "File \"<test>\", 1:17-1:20\n\
        Parse error.\n\
        This is not a valid pattern.\n")
    (render "{% match a with Abc %} {% /match %}");
  check_raises "Bad pattern (14)"
    (E "File \"<test>\", 1:8-1:11\nParse error.\nThis is not a valid pattern.\n")
    (render "{% Z a=ABC / %}");
  check_raises "Bad pattern (15)"
    (E
       "File \"<test>\", 1:15-1:18\n\
        Parse error.\n\
        This is not a valid pattern.\n")
    (render "{% match null ~%}");

  check_raises "Bad echo (1)"
    (E "File \"<test>\", 1:4-1:5\nParse error.\nThis is not a valid echo.\n")
    (render "{{ ? }}");
  check_raises "Bad echo (2)"
    (E "File \"<test>\", 1:5-1:6\nParse error.\nThis is not a valid echo.\n")
    (render "{{{ A }}}");
  check_raises "Bad echo (3)"
    (E "File \"<test>\", 1:8-1:9\nParse error.\nThis is not a valid echo.\n")
    (render "{{ a ? ? }}");
  check_raises "Bad echo (4)"
    (E "File \"<test>\", 1:7-1:8\nParse error.\nThis is not a valid echo.\n")
    (render "{{ %i ? }}");
  check_raises "Bad echo (5)"
    (E "File \"<test>\", 1:7-1:9\nParse error.\nExpected a '}}}'.\n")
    (render "{{{ a }}");
  check_raises "Bad echo (6)"
    (E "File \"<test>\", 1:6-1:9\nParse error.\nExpected a '}}'.\n")
    (render "{{ a }}}");
  check_raises "Bad echo (7)"
    (E "File \"<test>\", 1:11-1:12\nParse error.\nThis is not a valid echo.\n")
    (render "{{ x ? %i ? }}");

  check_raises "Bad echo format (1)"
    (E
       "File \"<test>\", 1:6-1:7\n\
        Parse error.\n\
        This is missing a format specification.\n")
    (render "{{ % a }}");
  check_raises "Bad echo format (2)"
    (E "File \"<test>\", 1:6-1:7\nParse error.\nExpected an 'i' format type.\n")
    (render "{{ %,b a }}");
  check_raises "Bad echo format (3)"
    (E "File \"<test>\", 1:6-1:7\nParse error.\nExpected a number.\n")
    (render "{{ %.f a }}");
  check_raises "Bad echo format (4)"
    (E
       "File \"<test>\", 1:7-1:8\n\
        Parse error.\n\
        Expected an 'f', 'e', or 'g' format type.\n")
    (render "{{ %.2b a }}");

  check_raises "Bad prop (1)"
    (E
       "File \"<test>\", 1:6-1:10\n\
        Parse error.\n\
        This is not a valid component prop.\n")
    (render "{% Z null=1 / %}");
  check_raises "Bad prop (2)"
    (E
       "File \"<test>\", 1:8-1:9\n\
        Parse error.\n\
        This is not a valid component prop.\n")
    (render "{% A a # %}");
  check_raises "Bad prop (3)"
    (E
       "File \"<test>\", 1:8-1:9\n\
        Parse error.\n\
        This is not a valid component prop.\n")
    (render "{% A b # %}");
  check_raises "Bad prop (4)"
    (E
       "File \"<test>\", 1:12-1:16\n\
        Parse error.\n\
        This is not a valid component prop.\n")
    (render "{% A b = 1 with %}");

  check_raises "Illegal field name (1)"
    (E
       "File \"<test>\", 1:12-1:16\n\
        Parse error.\n\
        This is not a valid field name.\n")
    (render "{% match { with %} {% /match %}");
  check_raises "Illegal field name (2)"
    (E
       "File \"<test>\", 1:13-1:17\n\
        Parse error.\n\
        This is not a valid field name.\n")
    (render "{% match {@ with %} {% /match %}");
  check_raises "Illegal field name (3)"
    (E
       "File \"<test>\", 1:14-1:18\n\
        Parse error.\n\
        This is not a valid field name.\n")
    (render "{% match {a, with %} {% /match %}");
  check_raises "Illegal field name (4)"
    (E
       "File \"<test>\", 1:12-1:16\n\
        Parse error.\n\
        This is not a valid field name.\n")
    (render "{% match < with %} {% /match %}");
  check_raises "Illegal field name (5)"
    (E
       "File \"<test>\", 1:14-1:18\n\
        Parse error.\n\
        This is not a valid field name.\n")
    (render "{% match <a, with %} {% /match %}");

  check_raises "Unclosed { (1)"
    (E
       "File \"<test>\", 1:13-1:17\n\
        Parse error.\n\
        Expected a ',', ':', or '}'.\n")
    (render "{% match {a with %} {% /match %}");
  check_raises "Unclosed { (2)"
    (E
       "File \"<test>\", 1:23-1:27\n\
        Parse error.\n\
        Expected a ',', ':', or '}'.\n")
    (render "{% match a with {@tag with} %} {% /match %}");
  check_raises "Unclosed { (3)"
    (E "File \"<test>\", 1:15-1:19\nParse error.\nExpected a ':'.\n")
    (render "{% match {\"a\" with %} {% /match %}");
  check_raises "Unclosed { (4)"
    (E "File \"<test>\", 1:15-1:19\nParse error.\nExpected a ',' or '}'.\n")
    (render "{% match {a:0 with %} {% /match %}");
  check_raises "Unclosed { (5)"
    (E "File \"<test>\", 1:16-1:20\nParse error.\nExpected a ',' or '}'.\n")
    (render "{% match {@a:0 with %} {% /match %}");

  check_raises "Unclosed ("
    (E "File \"<test>\", 1:13-1:17\nParse error.\nExpected a ',' or ')'.\n")
    (render "{% match (a with %} {% /match %}");

  check_raises "Unclosed [ (1)"
    (E
       "File \"<test>\", 1:13-1:17\n\
        Parse error.\n\
        Expected a ',', '...', or ']'.\n")
    (render "{% match [a with %} {% /match %}");
  check_raises "Unclosed [ (2)"
    (E "File \"<test>\", 1:19-1:23\nParse error.\nExpected a ']'.\n")
    (render "{% match [a, ...b with %} {% /match %}");
  check_raises "Unclosed [ (3)"
    (E "File \"<test>\", 1:16-1:20\nParse error.\nExpected a ']'.\n")
    (render "{% match [...b with %} {% /match %}");

  check_raises "Unclosed < (1)"
    (E
       "File \"<test>\", 1:13-1:17\n\
        Parse error.\n\
        Expected a ',', ':', or '>'.\n")
    (render "{% match <a with %} {% /match %}");
  check_raises "Unclosed < (2)"
    (E "File \"<test>\", 1:15-1:19\nParse error.\nExpected a ':'.\n")
    (render "{% match <\"a\" with %} {% /match %}");
  check_raises "Unclosed < (3)"
    (E "File \"<test>\", 1:15-1:19\nParse error.\nExpected a ',' or '>'.\n")
    (render "{% match <a:0 with %} {% /match %}");

  check_raises "Missing commas in match/map patterns (1)"
    (E
       "File \"<test>\", 1:12-1:13\n\
        Parse error.\n\
        Sequential patterns must be separated by a ','.\n")
    (render "{% match a b with a b %} {% /match %}");
  check_raises "Missing commas in match/map patterns (2)"
    (E
       "File \"<test>\", 1:15-1:16\n\
        Parse error.\n\
        Sequential patterns must be separated by a ','.\n")
    (render "{% map_dict a b with a %} {% /map_dict %}");
  check_raises "Missing commas in match/map patterns (3)"
    (E
       "File \"<test>\", 1:10-1:11\n\
        Parse error.\n\
        Sequential patterns must be separated by a ','.\n")
    (render "{% map a b with a %} {% /map %}");
  check_raises "Missing commas in match/map patterns (4)"
    (E
       "File \"<test>\", 1:16-1:20\n\
        Parse error.\n\
        Sequential patterns must be separated by a ','.\n")
    (render "{% match !null null with a %} {% /match %}");
  check_raises "Missing commas in match/map patterns (5)"
    (E
       "File \"<test>\", 1:25-1:26\n\
        Parse error.\n\
        Sequential patterns must be separated by a ','.\n")
    (render "{% match null with null ) %}");
  check_raises "Missing commas in match/map patterns (5)"
    (E
       "File \"<test>\", 1:21-1:22\n\
        Parse error.\n\
        Sequential patterns must be separated by a ','.\n")
    (render "{% match null, null } %}");

  check_raises "Unmatched match"
    (E
       "File \"<test>\", 1:26-1:26\n\
        Parse error.\n\
        Unclosed block. Expected a '{% /' somewhere.\n")
    (render "{% match x with true %} b");

  check_raises "Missing /match"
    (E "File \"<test>\", 1:26-1:29\nParse error.\nExpected a '/match'.\n")
    (render "{% match x with x %} {% /map %}");
  check_raises "Missing /map"
    (E "File \"<test>\", 1:24-1:29\nParse error.\nExpected a '/map'.\n")
    (render "{% map x with x %} {% /match %}");
  check_raises "Missing /map_dict"
    (E "File \"<test>\", 1:29-1:32\nParse error.\nExpected a '/map_dict'.\n")
    (render "{% map_dict x with x %} {% /map %}");

  check_raises "Unseparated echoes (1)"
    (E
       "File \"<test>\", 1:6-1:7\n\
        Parse error.\n\
        Sequential echoes must be separated by a '?'.\n")
    (render "{{ a b }}");
  check_raises "Unseparated echoes (2)"
    (E
       "File \"<test>\", 1:10-1:11\n\
        Parse error.\n\
        Sequential echoes must be separated by a '?'.\n")
    (render "{{ a ? b c }}");

  check_raises "# Blocks must contain text"
    (E "File \"<test>\", 1:10-1:13\nParse error.\nExpected a '%}'.\n")
    (render "{% A a=# abc # / %}");

  check_raises "Bad record field access (1)"
    (E
       "File \"<test>\", 1:12-1:13\n\
        Parse error.\n\
        This is not a valid field name.\n")
    (render "{% match a.1 with %}");
  check_raises "Bad record field access (2)"
    (E
       "File \"<test>\", 1:6-1:7\n\
        Parse error.\n\
        This is not a valid field name.\n")
    (render "{{ a.% }}")

let dup_record_field () =
  let open Alcotest in
  check_raises "Duplicate field"
    (E "File \"<test>\", 1:18-1:30\nParse error.\nDuplicate field 'a'.")
    (render {|{% match a with {a: 0, a: "a"} %} {% with _ %} {% /match %}|});
  check_raises "Duplicate tag field"
    (E "File \"<test>\", 1:18-1:31\nParse error.\nDuplicate field 'a'.")
    (render {|{% match a with {@a: 0, a: "a"} %} {% with _ %} {% /match %}|});
  check_raises "Duplicate dict field"
    (E "File \"<test>\", 1:18-1:30\nParse error.\nDuplicate field 'a'.")
    (render {|{% match a with <a: 0, a: "a"> %} {% with _ %} {% /match %}|});
  check_raises "Multiple record tags"
    (E
       "File \"<test>\", 1:18-1:32\n\
        Parse error.\n\
        This tagged record has multiple tags.")
    (render {|{% match a with {@a: 0, @b: "a"} %} {% with _ %} {% /match %}|})

let pat_count_mismatch () =
  let open Alcotest in
  check_raises "Pattern count mismatch 1"
    (E "File \"<test>\", 1:4-1:42\nType error.\nPattern count mismatch.")
    (render "{% match a, b, c with 1, 2 %} d {% /match %}");
  check_raises "Pattern count mismatch 2"
    (E "File \"<test>\", 1:30-1:42\nType error.\nPattern count mismatch.")
    (render "{% match a with 1, 2 %} d {% with 1, 2, 3 %} z {% /match %}");
  check_raises "Pattern count mismatch (map)"
    (E
       "File \"<test>\", 1:4-1:56\n\
        Type error.\n\
        Expressions 'map' and 'map_dict' can only have one or two patterns for \
        each\n\
        'with' expression.")
    (render "{% map a with 1, 2, 3 %} d {% with 4, 5, 6 %} z {% /map %}")

let dup_name_destructure () =
  let open Alcotest in
  check_raises "You can't bind a name more than once when destructuring."
    (E
       "File \"<test>\", 1:21-1:22\n\
        Type error.\n\
        The name 'x' is already bound in this pattern.")
    (render "{% match a with [x, x] %} a {% with _ %} b {% /match %}")

let type_error_echo () =
  let open Alcotest in
  check_raises "Echoed string literals cannot appear before a ?."
    (E
       "File \"<test>\", 1:4-1:8\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  ?string\n\
        Received:\n\
       \  string")
    (render {|{{ "ab" ? "cd" }}|});
  check_raises "Record field access type errors fail correctly."
    (E
       "File \"<test>\", 1:17-1:18\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  {b: int}\n\
        Received:\n\
       \  {b: string}")
    (render {|{{ a.b }} {{ %i a.b }}|})

let type_error_const () =
  let open Alcotest in
  check_raises "String <> int"
    (E
       "File \"<test>\", 1:32-1:33\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  string\n\
        Received:\n\
       \  int")
    (render {|{% match a with "a" %} {% with 1 %} {% with _ %} {% /match %}|});
  check_raises "Float <> int"
    (E
       "File \"<test>\", 1:32-1:33\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  float\n\
        Received:\n\
       \  int")
    (render {|{% match a with 0.0 %} {% with 1 %} {% with _ %} {% /match %}|});
  check_raises "String <> float"
    (E
       "File \"<test>\", 1:32-1:35\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  string\n\
        Received:\n\
       \  float")
    (render {|{% match a with "a" %} {% with 1.0 %} {% with _ %} {% /match %}|});
  check_raises "Map list key type mismatch."
    (E
       "File \"<test>\", 1:4-1:55\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  int\n\
        Received:\n\
       \  string")
    (render "{% map [1] with a, \"b\" %} {{ a }} {% with _ %} {% /map %}");
  check_raises "Map dict key type mismatch."
    (E
       "File \"<test>\", 1:4-2:13\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  string\n\
        Received:\n\
       \  int")
    (render
       "{% map_dict <\"a\": 1> with a, 1 %} {{ a }} {% with _ %}\n\
        {% /map_dict %}")

let type_error_nest () =
  let open Alcotest in
  check_raises "[int] <> [string]"
    (E
       "File \"<test>\", 2:10-2:11\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  [int]\n\
        Received:\n\
       \  [string]")
    (render
       "{% match a with [\"a\"] %} {% with _ %} {% /match %}\n\
        {% match a with [1] %} {% with _ %} {% /match %}");
  check_raises "{a: int} <> {a: float}"
    (E
       "File \"<test>\", 2:10-2:11\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  {a: float}\n\
        Received:\n\
       \  {a: int}")
    (render
       "{% match a with {a: 1} %} {% with _ %} {% /match %}\n\
        {% match a with {a: 0.0} %} {% with _ %} {% /match %}");
  check_raises "(string, _) <> (float, _)"
    (E
       "File \"<test>\", 2:10-2:11\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  (float, string)\n\
        Received:\n\
       \  (string, string)")
    (render
       "{% match a with (\"a\", a) %} {{ a }} {% with _ %} {% /match %}\n\
        {% match a with (1.0, a) %} {{ a }} {% with _ %} {% /match %}");
  check_raises "Tagged record <> untagged record"
    (E
       "File \"<test>\", 2:10-2:11\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  {a: int}\n\
        Received:\n\
       \    {@a: 1} | ...")
    (render
       "{% match a with {@a: 1} %} {% with _ %} {% /match %}\n\
        {% match a with {a: 1} %} {% with _ %} {% /match %}");
  check_raises "Dict <> record"
    (E
       "File \"<test>\", 2:10-2:11\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  {a: int}\n\
        Received:\n\
       \  <int>")
    (render
       "{% match a with <a: 1> %} {% with _ %} {% /match %}\n\
        {% match a with {a: 1} %} {% with _ %} {% /match %}");
  check_raises "?int <> int"
    (E
       "File \"<test>\", 2:10-2:11\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  int\n\
        Received:\n\
       \  ?int")
    (render
       "{% match a with !1 %} {% with _ %} {% /match %}\n\
        {% match a with 1 %} {% with _ %} {% /match %}");
  check_raises "2-tuple <> 3-tuple"
    (E
       "File \"<test>\", 2:10-2:11\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  (int, int, int)\n\
        Received:\n\
       \  (int, int)")
    (render
       "{% match a with (1, 2) %} {% with _ %} {% /match %}\n\
        {% match a with (1, 2, 3) %} {% with _ %} {% /match %}")

let type_error_record () =
  let open Alcotest in
  check_raises "Records with missing fields (1)"
    (E
       "File \"<test>\", 1:26-1:32\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  {a: int}\n\
        Received:\n\
       \  {b: _}")
    (render
       "{% map [(1, {a: 1}), (2, {b: 2})] with (i, {a}) %}\n\
       \  {{ %i i }} {{ %i a }} {% /map %}");
  check_raises "Records with missing fields (2)"
    (E
       "File \"<test>\", 1:10-1:16\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  {a: _, b: _}\n\
        Received:\n\
       \  {a: _}")
    (render "{% match {a: 1} with {a: _, b: _} %} {% /match %}");
  check_raises "Record field access type errors fail correctly."
    (E
       "File \"<test>\", 2:19-2:20\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  {b: {c: int}}\n\
        Received:\n\
       \  {b: {c: false | true}}")
    (render
       {|{% match a.b.c with true %}{% with false %}{% /match %}
         {% match a.b.c with 1 %}{% with _ %}{% /match %}|});
  let comps =
    Compile.Components.(
      make [ parse_string ~fname:"a.acutis" ~name:"A" "{{ a }}" ])
  in
  check_raises "Records with missing fields (Component)"
    (E
       "File \"<test>\", 1:4-1:7\n\
        Type error.\n\
        This is missing key 'a' of type:\n\
       \  string") (fun () ->
      ignore @@ Compile.from_string ~fname:"<test>" comps "{% A / %}")

let type_error_enum () =
  let open Alcotest in
  check_raises "Closed enum <> open enum"
    (E
       "File \"<test>\", 2:10-2:11\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \    @1\n\
        Received:\n\
       \    @1 | @2 | ...")
    (render
       "{% match a with @1 %} {% with @2 %} {% with _ %} {% /match %}\n\
        {% match a with @1 %} {% /match %}");
  check_raises "Enum vars with no subset are reported."
    (E
       "File \"<test>\", 3:12-3:13\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \    @1 | @2\n\
        Received:\n\
       \    @3 | @4")
    (render
       "{% match a with @1 %} {% with @2 %} {% /match %}\n\
        {% match b with @3 %} {% with @4 %} {% /match %}\n\
        {% map [a, b] with _ %} {% /map %}");
  check_raises "Enum vars + literals with no subset are reported."
    (E
       "File \"<test>\", 2:12-2:14\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \    @1 | @2\n\
        Received:\n\
       \    @3 | ...")
    (render
       "{% match a with @1 %} {% with @2 %} {% /match %}\n\
        {% map [a, @3] with _ %} {% /map %}");
  check_raises "Enum literals with no subset are reported (1)."
    (E
       "File \"<test>\", 1:10-1:12\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \    @1\n\
        Received:\n\
       \    @0 | ...")
    (render "{% match @0 with @1 %} {% /match %}");
  check_raises "Enum literals with no subset are reported (2)."
    (E
       "File \"<test>\", 1:10-1:12\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \    @\"a\"\n\
        Received:\n\
       \    @0 | ...")
    (render "{% match @0 with @\"a\" %} {% /match %}");
  check_raises "| false | true <> only false"
    (E
       "File \"<test>\", 1:10-1:14\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \    false\n\
        Received:\n\
       \    false | true")
    (render "{% match true with false %} {% /match %}")

let type_error_union () =
  let open Alcotest in
  check_raises "Int tag <> string tag"
    (E
       "File \"<test>\", 1:38-1:52\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \    {@tag: 0}\n\
        Received:\n\
       \    {@tag: \"a\", b: _}")
    (render
       "{% match a with {@tag: 0} %} {% with {@tag: \"a\", b} %} {{ b }}\n\
        {% /match %}");
  check_raises "Random other tags don't compile."
    (E
       "File \"<test>\", 1:24-1:25\n\
        Parse error.\n\
        Only literal integer, string, and boolean values may be union tags.\n")
    (render "{% match a with {@tag: []} %} {% /match %}");
  check_raises "Tag names must be coherent."
    (E
       "File \"<test>\", 1:36-1:46\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \    {@a: 0}\n\
        Received:\n\
       \    {@b: 1, c: _}")
    (render
       "{% match a with {@a: 0} %} {% with {@b: 1, c} %} {{ c }} {% /match %}")

let vars_with_clauses () =
  let open Alcotest in
  check_raises "Variable names must be coherent."
    (E
       "File \"<test>\", 2:16-2:17\n\
        Type error.\n\
        Variable 'b' must occur in each 'with' pattern.")
    (render
       "{% match a, b\n\
       \   with null, !b\n\
       \   with !a, null %}\n\
        {% with !_, !_ with null, null %}\n\
        {% /match %}");
  check_raises "Variable types must be coherent."
    (E
       "File \"<test>\", 1:38-1:39\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  int\n\
        Received:\n\
       \  string")
    (render
       "{% match a, b with 1, \"a\" %} {% with a, _ with _, a %} {% /match %}")

let component_typechecker () =
  let open Alcotest in
  check_raises "Component names match"
    (E
       "File \"<test>\", 1:4-1:14\n\
        Type error.\n\
        Component name mismatch.\n\
        Expected:\n\
       \  A.\n\
        Received:\n\
       \  B.")
    (render "{% A %} {% /B %}")

let unused_bindings () =
  let open Alcotest in
  check_raises "Basic unused bindings are reported."
    (E
       "File \"<test>\", 1:18-1:19\n\
        Type error.\n\
        This variable is bound but never used:\n\
       \  x")
    (render "{% match a with {x} %} {% /match %}");
  check_raises "Unused bindings are reported in the order they appear."
    (E
       "File \"<test>\", 1:21-1:22\n\
        Type error.\n\
        This variable is bound but never used:\n\
       \  y")
    (render
       "{% match a with {x, y} %}\n\
       \  {% match x with {z} %} {% /match %}\n\
        {% /match %}");
  check_raises "Shadowing bindings can report them as unused."
    (E
       "File \"<test>\", 1:26-1:27\n\
        Type error.\n\
        This variable is bound but never used:\n\
       \  y")
    (render
       "{% match a, b with {x}, {y} %}\n\
       \  {% match x with {y} %} {{ y }} {%/ match %}\n\
        {% /match %}")

let matching_unused () =
  let open Alcotest in
  check_raises "Basic pattern (1)."
    (E "File \"<test>\", 4:4-4:19\nMatching error.\nThis match case is unused.")
    (render
       "{% match a, b, c\n\
       \   with 10, 11, 12 %}\n\
        {% with _x, 21, 22 %}\n\
        {% with 10, 11, 12 %}\n\
        {% /match %}");
  check_raises "Basic pattern (2)."
    (E "File \"<test>\", 6:4-6:19\nMatching error.\nThis match case is unused.")
    (render
       "{% match a, b, c\n\
       \   with 10, 11, 12 %}\n\
        {% with _x, 21, 22 %}\n\
        {% with 30, 31, 32 %}\n\
        {% with 30, _y, 42 %}\n\
        {% with 30, 31, 42 %}\n\
        {% /match %}");
  check_raises "Nest patterns merge into wildcard patterns correctly (1)."
    (E "File \"<test>\", 1:33-1:48\nMatching error.\nThis match case is unused.")
    (render "{% match a, b with _x, _y %} {% with (_, _), 40 %} {% /match %}");
  check_raises "Nest patterns merge into wildcard patterns correctly (2)."
    (E "File \"<test>\", 4:4-4:22\nMatching error.\nThis match case is unused.")
    (render
       "{% match a, b\n\
       \   with _x, 1 %}\n\
        {% with (\"a\", \"b\"), 10 %}\n\
        {% with (\"a\", \"b\"), 1 %}\n\
        {% /match %}")

let parmatch () =
  let open Alcotest in
  check_raises "Partial matching with integers."
    (E
       "File \"<test>\", 1:4-1:55\n\
        Matching error.\n\
        This pattern-matching is not exhaustive.\n\
        Here's an example of a pattern which is not matched:\n\
       \  _")
    (render "{% match a with 0 with 10 with 20 with 30 %} {% /match %}");
  check_raises "Partial matching with lists (1)."
    (E
       "File \"<test>\", 1:4-1:41\n\
        Matching error.\n\
        This pattern-matching is not exhaustive.\n\
        Here's an example of a pattern which is not matched:\n\
       \  [_, ..._]")
    (render "{% match a with [] with [_] %} {% /match %}");
  check_raises "Partial matching with lists (2)."
    (E
       "File \"<test>\", 1:4-1:33\n\
        Matching error.\n\
        This pattern-matching is not exhaustive.\n\
        Here's an example of a pattern which is not matched:\n\
       \  []")
    (render "{% match a with [_] %} {% /match %}");
  check_raises "Partial matching with Nullables (1)."
    (E
       "File \"<test>\", 1:4-1:34\n\
        Matching error.\n\
        This pattern-matching is not exhaustive.\n\
        Here's an example of a pattern which is not matched:\n\
       \  !_")
    (render "{% match a with null %} {% /match %}");
  check_raises "Partial matching with Nullables (2)."
    (E
       "File \"<test>\", 1:4-1:32\n\
        Matching error.\n\
        This pattern-matching is not exhaustive.\n\
        Here's an example of a pattern which is not matched:\n\
       \  null")
    (render "{% match a with !_ %} {% /match %}");
  check_raises "Partial matching with Nullables (3)."
    (E
       "File \"<test>\", 1:4-1:48\n\
        Matching error.\n\
        This pattern-matching is not exhaustive.\n\
        Here's an example of a pattern which is not matched:\n\
       \  !_")
    (render "{% match a with !1 %} {% with null %} {% /match %}");
  check_raises "Partial matching with enums nested in nullables."
    (E
       "File \"<test>\", 1:4-1:58\n\
        Matching error.\n\
        This pattern-matching is not exhaustive.\n\
        Here's an example of a pattern which is not matched:\n\
       \  !@1, _")
    (render "{% match a, b with !@1, 2 %} {% with null, _ %} {% /match %}");
  check_raises "Partial matching with records."
    (E
       "File \"<test>\", 1:4-1:56\n\
        Matching error.\n\
        This pattern-matching is not exhaustive.\n\
        Here's an example of a pattern which is not matched:\n\
       \  {a: _, b: _}")
    (render "{% match a with {b: 10} %} {% with {a: 20} %} {% /match %}");
  check_raises "Partial matching with dictionaries."
    (E
       "File \"<test>\", 1:4-1:61\n\
        Matching error.\n\
        This pattern-matching is not exhaustive.\n\
        Here's an example of a pattern which is not matched:\n\
       \  _")
    (render "{% match a with <a: true> %} {% with <a: false> %} {% /match %}");
  check_raises "Partial matching with unions (1)."
    (E
       "File \"<test>\", 1:4-2:10\n\
        Matching error.\n\
        This pattern-matching is not exhaustive.\n\
        Here's an example of a pattern which is not matched:\n\
       \  {@tag: 0, a: _}")
    (render
       "{% match a with {@tag: 0, a: 10} %} {% with {@tag: 1, b: 20} %}\n\
        {% /match %}");
  check_raises "Partial matching with unions (2)."
    (E
       "File \"<test>\", 1:4-2:10\n\
        Matching error.\n\
        This pattern-matching is not exhaustive.\n\
        Here's an example of a pattern which is not matched:\n\
       \  {@tag: false, b: _}")
    (render
       "{% match a with {@tag: true, a: 10} %}{% with {@tag: false, b: 20} %}\n\
        {% /match %}");
  check_raises "Partial matching with unions (3)."
    (E
       "File \"<test>\", 1:4-2:10\n\
        Matching error.\n\
        This pattern-matching is not exhaustive.\n\
        Here's an example of a pattern which is not matched:\n\
       \  {@tag: true, a: _}")
    (render
       "{% match a with {@tag: true, a: 10} %} {% with {@tag: false, b: _} %}\n\
        {% /match %}");
  check_raises "Partial matching with records."
    (E
       "File \"<test>\", 1:4-3:10\n\
        Matching error.\n\
        This pattern-matching is not exhaustive.\n\
        Here's an example of a pattern which is not matched:\n\
       \  {favoriteColor: _, firstName: _}")
    (render
       "{% match a with {firstName: name, favoriteColor: \"green\"} %}\n\
        {{ name }}'s favorite color is green.\n\
        {% /match %}");
  check_raises "Partial lists print correctly."
    (E
       "File \"<test>\", 1:5-5:10\n\
        Matching error.\n\
        This pattern-matching is not exhaustive.\n\
        Here's an example of a pattern which is not matched:\n\
       \  {@kind: \"anonymous\", books: _}")
    (render
       "{%~ match author\n\
       \    with {@kind: \"person\", name: _, books: [_newest, ..._older]} %}\n\
        {%~  with {@kind: \"anonymous\", books: []} %}\n\
       \  This author hasn't published any books.\n\
        {% /match %}")

let merge_expanded_trees () =
  let open Alcotest in
  check_raises "Both nil and cons paths fail to merge into a wildcard."
    (E "File \"<test>\", 6:9-6:19\nMatching error.\nThis match case is unused.")
    (render
       {|
    {% match a, b
        with 1, _ %}
    {%  with _, !1 %}
    {%  with _, null %}
    {%  with 1, !2 %}
    {%  with _, _ %}
    {% /match %}|})

let bad_destructure () =
  let open Alcotest in
  check_raises "Template blocks are not allowed in destructure patterns."
    (E
       "File \"<test>\", 1:21-1:28\n\
        Type error.\n\
        Template blocks are not allowed in a destructure pattern.")
    (render "{% match a with {b: #%} {%#} %} {% /match %}");
  check_raises "Record accessors are not allowed in destructure patterns."
    (E
       "File \"<test>\", 1:21-1:24\n\
        Type error.\n\
        Record '.' access is not allowed in a destructure pattern.")
    (render "{% match a with {b: x.z} %} {% /match %}")

let component_graph () =
  let open Alcotest in
  let a =
    Compile.Components.parse_string ~fname:"a.acutis" ~name:"A" "{% B /%}"
  in
  let b =
    Compile.Components.parse_string ~fname:"b.acutis" ~name:"B" "{% C /%}"
  in
  let c =
    Compile.Components.parse_string ~fname:"c.acutis" ~name:"C" "{% D /%}"
  in
  let d =
    Compile.Components.parse_string ~fname:"d.acutis" ~name:"D" "{% B /%}"
  in
  check_raises "Cyclic dependencies are reported."
    (E "Compile error.\nDependency cycle detected.\nA -> B -> C -> D -> B")
    (fun () -> ignore @@ Compile.Components.make [ a; b; c; d ]);
  check_raises "Missing components are reported."
    (E "Compile error.\nMissing template:\n  D\nRequired by:\n  C") (fun () ->
      ignore @@ Compile.Components.make [ a; b; c ]);
  check_raises "Missing components are reported (by root)."
    (E "Compile error.\nMissing template:\n  A\nRequired by:\n  <test>")
    (render "{% A /%}");
  check_raises "Duplicate names are reported."
    (E "Compile error.\nThere are multiple components with the name 'A'.")
    (fun () -> ignore @@ Compile.Components.make [ a; b; a ])

let known_broken () =
  let open Alcotest in
  check_raises
    "This error is incorrect. The compiler fails to detect the first unused \
     wildcard case. I'm not fixing this error yet, but I'm keeping this test \
     to track when it changes."
    (E "File \"<test>\", 1:43-1:49\nMatching error.\nThis match case is unused.")
    (render "{% match a with (_, _) %} {% with _ %} {% with _ %} {% /match %}")

let decode_mismatch () =
  let open Alcotest in
  check_raises "Basic type mismatch."
    (E
       "File \"<test>\"\n\
        Render error.\n\
        The data supplied does not match this template's interface.\n\
        Path:\n\
       \  input -> \"a\" -> \"b\".\n\
        Expected type:\n\
       \  string\n\
        Received value:\n\
       \  []")
    (render "{% match a with {b} %} {{ b }} {% /match %}"
       ~json:{|{"a": {"b": []}}|});
  let json = {|{"a": "a", "b": true, "c": []}|} in
  check_raises "Map type mismatch (1)."
    (E
       "File \"<test>\"\n\
        Render error.\n\
        The data supplied does not match this template's interface.\n\
        Path:\n\
       \  input -> \"a\".\n\
        Expected type:\n\
       \  [{a: string}]\n\
        Received value:\n\
       \  \"a\"")
    (render "{% map a with {a} %}{{ a }}{% /map %}" ~json);
  check_raises "Map type mismatch (2)."
    (E
       "File \"<test>\"\n\
        Render error.\n\
        The data supplied does not match this template's interface.\n\
        Path:\n\
       \  input -> \"a\".\n\
        Expected type:\n\
       \  [int]\n\
        Received value:\n\
       \  \"a\"")
    (render "{% map [1, 2, ...a] with a %}{{ %i a }}{% /map %}" ~json);
  check_raises "Missing bindings are reported"
    (E
       "File \"<test>\"\n\
        Render error.\n\
        The data supplied does not match this template's interface.\n\
        Path:\n\
       \  input.\n\
        Expected type:\n\
       \  {z: string}\n\
        Input is missing key:\n\
       \  z")
    (render "{{ z }}" ~json);
  check_raises "Bad enums are reported: boolean."
    (E
       "File \"<test>\"\n\
        Render error.\n\
        The data supplied does not match this template's interface.\n\
        Path:\n\
       \  input -> \"b\".\n\
        Expected type:\n\
       \    false\n\
        This type does not allow the given value:\n\
       \  true")
    (render "{% match b with false %} {% /match %}" ~json);
  check_raises "Bad enums are reported: int."
    (E
       "File \"<test>\"\n\
        Render error.\n\
        The data supplied does not match this template's interface.\n\
        Path:\n\
       \  input -> \"a\".\n\
        Expected type:\n\
       \    @1 | @2\n\
        This type does not allow the given value:\n\
       \  3")
    (render "{% match a with @1 %} {% with @2 %} {% /match %}"
       ~json:{|{"a": 3}|});
  check_raises "Bad enums are reported: int."
    (E
       "File \"<test>\"\n\
        Render error.\n\
        The data supplied does not match this template's interface.\n\
        Path:\n\
       \  input -> \"a\".\n\
        Expected type:\n\
       \    @\"a\" | @\"b\"\n\
        This type does not allow the given value:\n\
       \  \"c\"")
    (render "{% match a with @\"a\" %} {% with @\"b\" %} {% /match %}"
       ~json:{|{"a": "c"}|});
  check_raises "Tuple size mismatch."
    (E
       "File \"<test>\"\n\
        Render error.\n\
        The data supplied does not match this template's interface.\n\
        Path:\n\
       \  input -> \"a\".\n\
        Expected type:\n\
       \  (string, string)\n\
        Received value:\n\
       \  [ \"a\" ]")
    (render "{% match a with (a, b) %} {{ a }} {{ b }} {% /match %}"
       ~json:{|{"a": ["a"]}|});
  check_raises "Bad unions are reported (1)."
    (E
       "File \"<test>\"\n\
        Render error.\n\
        The data supplied does not match this template's interface.\n\
        Path:\n\
       \  input -> \"a\".\n\
        Expected type:\n\
       \    {@tag: 1, a: string}\n\
        Received value:\n\
       \  { \"tag\": \"a\", \"a\": \"a\" }")
    (render "{% match a with {@tag: 1, a} %} {{ a }} {% /match %}"
       ~json:{|{"a": {"tag": "a", "a": "a"}}|});
  check_raises "Bad unions are reported (2)."
    (E
       "File \"<test>\"\n\
        Render error.\n\
        The data supplied does not match this template's interface.\n\
        Path:\n\
       \  input -> \"a\".\n\
        Expected type:\n\
       \    {@tag: 1, a: string}\n\
        Received value:\n\
       \  { \"tag\": 2, \"a\": \"a\" }")
    (render "{% match a with {@tag: 1, a} %} {{ a }} {% /match %}"
       ~json:{|{"a": {"tag": 2, "a": "a"}}|});
  check_raises "Looong paths format correctly."
    (E
       "File \"<test>\"\n\
        Render error.\n\
        The data supplied does not match this template's interface.\n\
        Path:\n\
       \  input -> \"abc\" -> \"def\" -> \"ghi\" -> \"jkl\" -> \"mno\" -> \
        \"pqr\" -> \"stu\"\n\
       \  -> \"vwx\" -> \"yz\".\n\
        Expected type:\n\
       \  int\n\
        Received value:\n\
       \  \"a\"")
    (render
       "{% match abc\n\
        with {def: {ghi: {jkl: {mno: {pqr: {stu: {vwx: {yz: 1}}}}}}}} %}\n\
        {% with _ %} {% /match %}"
       ~json:
         {|{"abc":
              {"def": {"ghi": {"jkl": {"mno": {"pqr": {"stu": {"vwx":
                {"yz": "a"}}}}}}}}
            }|})

let interface_parse () =
  let open Alcotest in
  check_raises "Invalid field names."
    (E
       "File \"<test>\", 1:27-1:31\n\
        Parse error.\n\
        This is not a valid field name.\n")
    (render "{% interface a = {a: int, with: string } / %}");
  check_raises "Missing , or }."
    (E "File \"<test>\", 1:26-1:27\nParse error.\nExpected a ',' or '}'.\n")
    (render "{% interface a = {a: int b: string } / %}");

  check_raises "Bad prop name (1)."
    (E
       "File \"<test>\", 1:14-1:18\n\
        Parse error.\n\
        This is not a valid prop name.\n")
    (render "{% interface with = int / %}");
  check_raises "Bad prop name (2)."
    (E
       "File \"<test>\", 1:22-1:26\n\
        Parse error.\n\
        This is not a valid prop name.\n")
    (render "{% interface x = int with = int / %}");

  check_raises "Bad token after a variant (1)."
    (E
       "File \"<test>\", 1:24-1:28\n\
        Parse error.\n\
        This is not a valid prop name. You possibly forgot a '|'.\n")
    (render "{% interface x={@a: 1} with / %}");
  check_raises "Bad token after a variant (2)."
    (E
       "File \"<test>\", 1:21-1:25\n\
        Parse error.\n\
        This is not a valid prop name. You possibly forgot a '|'.\n")
    (render "{% interface x=@\"a\" with / %}");
  check_raises "Bad token after a variant (3)."
    (E
       "File \"<test>\", 1:19-1:23\n\
        Parse error.\n\
        This is not a valid prop name. You possibly forgot a '|'.\n")
    (render "{% interface x=@0 with / %}");
  check_raises "Bad token after a variant (4)."
    (E
       "File \"<test>\", 1:22-1:26\n\
        Parse error.\n\
        This is not a valid prop name. You possibly forgot a '|'.\n")
    (render "{% interface x=false with / %}");

  check_raises "Missing equals."
    (E "File \"<test>\", 1:16-1:19\nParse error.\nExpected an '='.\n")
    (render "{% interface x int / %}");

  check_raises "Invalid type (1)."
    (E "File \"<test>\", 1:16-1:20\nParse error.\nThis is not a valid type.\n")
    (render "{% interface x=with / %}");
  check_raises "Invalid type (2)."
    (E "File \"<test>\", 1:17-1:21\nParse error.\nThis is not a valid type.\n")
    (render "{% interface x=?with / %}");
  check_raises "Invalid type (3)."
    (E "File \"<test>\", 1:17-1:21\nParse error.\nThis is not a valid type.\n")
    (render "{% interface x=[with] / %}");
  check_raises "Invalid type (4)."
    (E "File \"<test>\", 1:17-1:21\nParse error.\nThis is not a valid type.\n")
    (render "{% interface x={with: int} / %}");
  check_raises "Invalid type (5)."
    (E "File \"<test>\", 1:18-1:22\nParse error.\nThis is not a valid type.\n")
    (render "{% interface x={@with: 1} / %}");
  check_raises "Invalid type (6)."
    (E "File \"<test>\", 1:20-1:24\nParse error.\nThis is not a valid type.\n")
    (render "{% interface x={a: with} / %}");
  check_raises "Invalid type (7)."
    (E "File \"<test>\", 1:17-1:21\nParse error.\nThis is not a valid type.\n")
    (render "{% interface x=<with> / %}");
  check_raises "Invalid type (8)."
    (E "File \"<test>\", 1:17-1:21\nParse error.\nThis is not a valid type.\n")
    (render "{% interface x=(with) / %}");
  check_raises "Invalid type (9)."
    (E "File \"<test>\", 1:22-1:26\nParse error.\nThis is not a valid type.\n")
    (render "{% interface x=(int, with) / %}");

  check_raises "Missing colon (1)."
    (E "File \"<test>\", 1:19-1:20\nParse error.\nExpected a ':'.\n")
    (render "{% interface x={a b} / %}");
  check_raises "Missing colon (2)."
    (E "File \"<test>\", 1:20-1:21\nParse error.\nExpected a ':'.\n")
    (render "{% interface x={@a b} / %}");

  check_raises "Bad enum"
    (E
       "File \"<test>\", 1:17-1:18\n\
        Parse error.\n\
        Expected a string or an integer.\n")
    (render "{% interface x=@a / %}");

  check_raises "Bad token after record pipe."
    (E
       "File \"<test>\", 1:26-1:27\n\
        Parse error.\n\
        Expected a record type or an '...'.\n")
    (render "{% interface x={@a: 1} | y / %}");
  check_raises "Bad token after string enum pipe."
    (E
       "File \"<test>\", 1:23-1:24\n\
        Parse error.\n\
        Expected an enum value or an '...'.\n")
    (render "{% interface x=@\"a\" | y / %}");
  check_raises "Bad token after int enum pipe."
    (E
       "File \"<test>\", 1:21-1:22\n\
        Parse error.\n\
        Expected an enum value or an '...'.\n")
    (render "{% interface x=@0 | y / %}");

  check_raises "Missing >"
    (E "File \"<test>\", 1:20-1:21\nParse error.\nExpected a '>'.\n")
    (render "{% interface x=<int} / %}");
  check_raises "Missing ]"
    (E "File \"<test>\", 1:20-1:21\nParse error.\nExpected a ']'.\n")
    (render "{% interface x=[int} / %}");
  check_raises "Missing )"
    (E "File \"<test>\", 1:20-1:21\nParse error.\nExpected a ')'.\n")
    (render "{% interface x=(int} / %}");

  check_raises "Bad string enum"
    (E "File \"<test>\", 1:24-1:25\nParse error.\nExpected a string.\n")
    (render "{% interface x=@\"a\" | @0 / %}");
  check_raises "Bad int enum"
    (E "File \"<test>\", 1:22-1:25\nParse error.\nExpected an integer.\n")
    (render "{% interface x=@0 | @\"a\" / %}");
  check_raises "Bad boolean"
    (E "File \"<test>\", 1:24-1:25\nParse error.\nExpected a boolean.\n")
    (render "{% interface x=false | @0 / %}");

  check_raises "Bad union tag type"
    (E
       "File \"<test>\", 1:21-1:22\n\
        Parse error.\n\
        Only literal integer, string, and boolean values may be union tags.\n")
    (render "{% interface x={@x: [int]} / %}")

let interface_type_parse () =
  let open Alcotest in
  check_raises "Duplicate declarations"
    (E
       "File \"<test>\", 1:20-1:28\n\
        Type error.\n\
        Prop 'x' is already defined in the interface.")
    (render "{% interface x=int x=string / %}");
  check_raises "Non-existent type names."
    (E "File \"<test>\", 1:16-1:20\nType error.\nThere is no type named 'lmao'.")
    (render "{% interface x=lmao / %}");
  check_raises "Untagged unions (1)"
    (E
       "File \"<test>\", 1:18-1:26\n\
        Type error.\n\
        You cannot union records without a '@' tag field.")
    (render "{% interface x = {a: int} | {b: string} / %}");
  check_raises "Untagged unions (2)"
    (E
       "File \"<test>\", 1:38-1:49\n\
        Type error.\n\
        You cannot union records without a '@' tag field.")
    (render "{% interface x = {@tag: 1, a: int} | {b: string} / %}");
  check_raises "Tagged unions with mismatched tag names"
    (E
       "File \"<test>\", 1:30-1:42\n\
        Type error.\n\
        This record has tag field '@badtag' instead of '@tag'.")
    (render "{% interface x = {@tag: 1} | {@badtag: 2} / %}");
  check_raises "Duplicate tags."
    (E
       "File \"<test>\", 1:30-1:39\n\
        Type error.\n\
        Tag value '1' is already used in this union.")
    (render "{% interface x = {@tag: 1} | {@tag: 1} / %}");
  check_raises "Tag type error: int <> string."
    (E
       "File \"<test>\", 1:37-1:40\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  int\n\
        Received:\n\
       \  string")
    (render "{% interface x = {@tag: 1} | {@tag: \"a\"} / %}");
  check_raises "Tag type error: string <> bool."
    (E
       "File \"<test>\", 1:39-1:43\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \  string\n\
        Received:\n\
       \    true")
    (render "{% interface x = {@tag: \"a\"} | {@tag: true} / %}");
  check_raises "Tag type error: bool <> int."
    (E
       "File \"<test>\", 1:41-1:44\n\
        Type error.\n\
        Type mismatch.\n\
        Expected:\n\
       \    false | true\n\
        Received:\n\
       \  int")
    (render "{% interface x = {@tag: false} | {@tag: 100} / %}")

let interface_typecheck () =
  let open Alcotest in
  check_raises "Interface type error: bool <> int."
    (E
       "File \"<test>\", 1:14-1:21\n\
        Type error.\n\
        This interface does not match the implementation.\n\
        Prop name:\n\
       \  x\n\
        Interface:\n\
       \  int\n\
        Implementation:\n\
       \    true")
    (render "{% interface x = int / %}{% match x with true %}{% /match %}");
  check_raises "Interface is missing props."
    (E
       "File \"<test>\", 1:4-1:23\n\
        Type error.\n\
        This interface does not match the implementation.\n\
        Missing prop name:\n\
       \  y\n\
        Of type:\n\
       \  string")
    (render "{% interface x = int / %} {{ y }}");
  check_raises "Interface is missing record fields."
    (E
       "File \"<test>\", 1:14-1:26\n\
        Type error.\n\
        This interface does not match the implementation.\n\
        Prop name:\n\
       \  x\n\
        Interface:\n\
       \  {a: int}\n\
        Implementation:\n\
       \  {a: string, b: string}")
    (render
       "{% interface x = {a: int} / %} \n\
        {% match x with {a, b} %} {{ a }} {{ b }} {% /match %}");
  check_raises "Interface is missing enum cases."
    (E
       "File \"<test>\", 1:14-1:31\n\
        Type error.\n\
        This interface does not match the implementation.\n\
        Prop name:\n\
       \  x\n\
        Interface:\n\
       \    @0 | @1 | ...\n\
        Implementation:\n\
       \    @0 | @1 | @2 | ...")
    (render
       "{% interface x = @0 | @1 | ... / %} \n\
        {% match x with @0 %} {% with @1 %} {% with @2 %} {% with _ %}\n\
        {% /match %}");
  check_raises "Interface is missing union cases."
    (E
       "File \"<test>\", 1:14-1:53\n\
        Type error.\n\
        This interface does not match the implementation.\n\
        Prop name:\n\
       \  x\n\
        Interface:\n\
       \    {@tag: 0} | {@tag: 1, a: int} | ...\n\
        Implementation:\n\
       \    {@tag: 0} | {@tag: 1, a: string} | {@tag: 2, b: string} | ...")
    (render
       "{% interface x = {@tag: 0} | {@tag: 1, a: int} | ... / %} \n\
        {% match x\n\
       \  with {@tag: 0} %}\n\
        {% with {@tag: 1, a} %} {{ a }}\n\
        {% with {@tag: 2, b} %} {{ b }}\n\
        {% with _ %}\n\
        {% /match %}");
  check_raises "Unknown is not equal to any other type."
    (E
       "File \"<test>\", 1:14-1:19\n\
        Type error.\n\
        This interface does not match the implementation.\n\
        Prop name:\n\
       \  x\n\
        Interface:\n\
       \  _\n\
        Implementation:\n\
       \  string")
    (render "{% interface x = _ / %} {{ x }}")

let () =
  let open Alcotest in
  run "Errors"
    [
      ( "Parser & lexer errors",
        [
          test_case "Illegal characters" `Quick illegal_chars;
          test_case "Number parsing" `Quick number_parsing;
          test_case "Illegal names" `Quick illegal_names;
          test_case "Unterminated sections" `Quick unterminated;
          test_case "Illegal escape sequences" `Quick illegal_escape;
          test_case "Duplicate fields" `Quick dup_record_field;
          test_case "Parser errors" `Quick parser_errors;
          test_case "Interface parser errors" `Quick interface_parse;
          test_case "Interface type parser errors" `Quick interface_type_parse;
        ] );
      ( "Type clash",
        [
          test_case "Echoes" `Quick type_error_echo;
          test_case "Constants" `Quick type_error_const;
          test_case "Nested types" `Quick type_error_nest;
          test_case "Records" `Quick type_error_record;
          test_case "Enums" `Quick type_error_enum;
          test_case "Tagged unions" `Quick type_error_union;
          test_case "Interface type errors" `Quick interface_typecheck;
        ] );
      ( "Typechecker errors",
        [
          test_case "Pattern count mismatch" `Quick pat_count_mismatch;
          test_case "Multiple names bound" `Quick dup_name_destructure;
          test_case "Variables in with clauses" `Quick vars_with_clauses;
          test_case "Component errors" `Quick component_typechecker;
          test_case "Unused bindings" `Quick unused_bindings;
        ] );
      ( "Matching errors",
        [
          test_case "Unused patterns" `Quick matching_unused;
          test_case "Partial patterns" `Quick parmatch;
          test_case "Partial patterns with complex trees" `Quick
            merge_expanded_trees;
          test_case "Other errors" `Quick bad_destructure;
        ] );
      ( "Other compile errors",
        [
          test_case "Dependency graphs" `Quick component_graph;
          test_case "Known broken cases" `Quick known_broken;
        ] );
      ("Render errors", [ test_case "Decode errors" `Quick decode_mismatch ]);
    ]
