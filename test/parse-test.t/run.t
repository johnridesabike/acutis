Echoes parse correctly
  $ acutis echoes.acutis --printast
  ((text no_trim "" no_trim)
   (echo fmt_string (echo_var "a") escape)
   (text no_trim " " no_trim)
   (echo fmt_string (echo_string "b") escape)
   (text no_trim " " no_trim)
   (echo fmt_string (echo_string "f\"g") no_escape)
   (text no_trim "\n" no_trim)
   (echo fmt_int (echo_var "i") escape)
   (text no_trim " " no_trim)
   (echo fmt_float (echo_var "f") escape)
   (text no_trim " " no_trim)
   (echo fmt_bool (echo_var "b") escape)
   (text no_trim "\n" no_trim))

Numbers parse correctly
  $ acutis numbers.acutis --printast
  ((text no_trim "" no_trim)
   (match
    ((var "a"))
    (((pats
       (((record
          (("exp1" 150.)
           ("exp2" -1000.)
           ("exp3" 0.2)
           ("frac" 10.55)
           ("int" 1000)
           ("negfrac" -12.34)
           ("negint" -999))))))
      (nodes ((text no_trim "" no_trim))))
     ((pats (((var "_")))) (nodes ((text no_trim "" no_trim))))))
   (text no_trim "\n" no_trim))

Trim parses correctly
  $ acutis trim.acutis --printast
  ((text no_trim "" trim)
   (echo fmt_string (echo_var "a") escape)
   (text no_trim " " trim)
   (echo fmt_string (echo_var "b") escape)
   (text no_trim " " no_trim)
   (echo fmt_string (echo_var "c") escape)
   (text trim " " trim)
   (echo fmt_string (echo_var "d") escape)
   (text trim " " trim)
   (echo fmt_string (echo_var "e") no_escape)
   (text no_trim " " no_trim)
   (echo fmt_string (echo_var "f") no_escape)
   (text trim " " trim)
   (echo fmt_string (echo_var "g") no_escape)
   (text trim "\n" no_trim))

Comments parse correctly
  $ acutis comments.acutis --printast
  ((text no_trim "a b c\n" no_trim))

Matches parse correctly
  $ acutis matches.acutis --printast
  ((text no_trim "Flat match:\n" no_trim)
   (match
    ((var "a"))
    (((pats ((1) (2))) (nodes ((text no_trim "" no_trim))))
     ((pats ((3))) (nodes ((text no_trim " " no_trim))))
     ((pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n\nNested match:\n" no_trim)
   (match
    ((var "b"))
    (((pats (((var "c"))))
      (nodes
       ((text no_trim "" no_trim)
        (match
         ((var "d") (var "e"))
         (((pats (((var "f") (var "g")))) (nodes ((text no_trim " " no_trim))))))
        (text no_trim "" no_trim))))))
   (text no_trim "\n" no_trim))

Map list parses correctly
  $ acutis map_list.acutis --printast
  ((text no_trim "" no_trim)
   (map
    (var "l")
    (((pats ((1) (2))) (nodes ((text no_trim "" no_trim))))
     ((pats ((3 (var "i")))) (nodes ((text no_trim " " no_trim))))
     ((pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n" no_trim))

Map dict parses correctly
  $ acutis map_dict.acutis --printast
  ((text no_trim "" no_trim)
   (map_dict
    (var "d")
    (((pats ((1) (2))) (nodes ((text no_trim "" no_trim))))
     ((pats ((3 (var "k")))) (nodes ((text no_trim " " no_trim))))
     ((pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n" no_trim))

Component with props parses correctly
  $ acutis component.acutis --printast
  ((text no_trim "" no_trim)
   (component
    "Template"
    (("a" (var "b"))
     ("c" (var "c"))
     ("d" (var "e"))
     ("f" (var "f"))
     ("g" (block ((text no_trim " " no_trim))))
     ("h"
      (block
       ((text no_trim "" no_trim)
        (match
         ((var "a"))
         (((pats (((var "b")))) (nodes ((text no_trim " " no_trim))))))
        (text no_trim "" no_trim))))
     ("i" (block ())))
    "Template")
   (text no_trim "\n" no_trim))
Component with implicit children parses correctly
  $ echo "{% Template %} {% /Template %}" > temp.acutis
  $ acutis temp.acutis --printast
  ((text no_trim "" no_trim)
   (component
    "Template"
    (("children" (block ((text no_trim " " no_trim)))))
    "Template")
   (text no_trim "\n" no_trim))

Patterns parse correctly
  $ acutis patterns.acutis --printast
  ((text no_trim "Tuple:\n" no_trim)
   (match
    ((var "tuple"))
    (((pats (((1 2.5 "a")))) (nodes ((text no_trim " " no_trim))))
     ((pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n\nList:\n" no_trim)
   (match
    ((var "list"))
    (((pats (((list ())))) (nodes ((text no_trim " " no_trim))))
     ((pats (((list ((nullable (var "a")) null)))))
      (nodes ((text no_trim " " no_trim))))
     ((pats (((list ((var "z")) (var "tl")))))
      (nodes ((text no_trim "\n" no_trim))))))
   (text no_trim "\n\nRecord:\n" no_trim)
   (match
    ((var "record"))
    (((pats (((record (("!#%@" (var "b")) ("a" (var "a")))))))
      (nodes ((text no_trim " " no_trim))))
     ((pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n\nEnum:\n" no_trim)
   (match
    ((var "enums"))
    (((pats ((((enum_string "a") (enum_int 1) true false))))
      (nodes ((text no_trim " " no_trim))))
     ((pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n\nTagged union:\n" no_trim)
   (match
    ((var "tagged"))
    (((pats (((record (tagged ("tag" true) (("a" (var "a"))))))))
      (nodes ((text no_trim " " no_trim))))
     ((pats (((record (tagged ("tag" false) ())))))
      (nodes ((text no_trim "\n" no_trim))))))
   (text no_trim "\n\nDictionary:\n" no_trim)
   (match
    ((var "dict"))
    (((pats (((dict (("a" 1) ("b" 2)))))) (nodes ((text no_trim " " no_trim))))
     ((pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n" no_trim))

Interfaces parse correctly.
  $ acutis interface.acutis --printast
  ((text no_trim "" no_trim)
   (interface
    (("a"
      (record
       closed
       ((("a" (enum_int closed (0 1))) ("b" (enum_string closed ("a" "b")))))))
     ("b"
      (record
       closed
       ((tagged ("tag" true) (("a" (list (named "int")))))
        (tagged ("tag" false) (("a" (dict (nullable (named "string")))))))))
     ("c"
      (record
       closed
       ((tagged ("tag" 0) ())
        (tagged ("tag" 1) (("a" ((named "float") (enum_bool (true false)))))))))
     ("d"
      (record
       closed
       ((tagged ("tag" "a") (("a" (named "float"))))
        (tagged ("tag" "b") (("a" (enum_int open (0 1))))))))
     ("e"
      (record
       open
       ((tagged ("tag" 0) (("a" (named "_"))))
        (tagged ("tag" 1) (("b" (enum_string open ("a" "b"))))))))
     ("children" (named "string"))
     ("optionalChildren" (nullable (named "string")))))
   (text no_trim "\n" no_trim))

! and . precedence works correctly
  $ acutis precedence.acutis --printast
  ((text no_trim "" no_trim)
   (match
    ((nullable (nullable (field (field (var "a") "b") "c"))))
    (((pats (((nullable (nullable false)))))
      (nodes ((text no_trim "" no_trim))))
     ((pats (((var "_")))) (nodes ((text no_trim "" no_trim))))))
   (text no_trim "\n" no_trim))

Edge cases
  $ acutis edge_cases.acutis --printast
  ((text no_trim "Patterns with }} parse correctly\n" no_trim)
   (match
    ((var "a"))
    (((pats (((record (("a" (record (("b" (var "b"))))))))))
      (nodes
       ((text no_trim " " no_trim)
        (echo fmt_string (echo_var "b") escape)
        (text no_trim " " no_trim))))))
   (text no_trim "\n" no_trim))
