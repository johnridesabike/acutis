Print the untyped AST to make sure parsing works
  $ acutis template.acutis --printast
  ((text no_trim "Echoes\n" no_trim)
   (echo () fmt_string (echo_var "ech_a") escape)
   (text no_trim " " no_trim)
   (echo () fmt_string (echo_string "b") escape)
   (text no_trim " " no_trim)
   (echo
    ((fmt_string (echo_var "ech_d")) (fmt_string (echo_var "ech_e")))
    fmt_string
    (echo_string "f\"g")
    no_escape)
   (text no_trim "\n" no_trim)
   (echo () fmt_int (echo_var "ech_i") escape)
   (text no_trim " " no_trim)
   (echo () fmt_float (echo_var "ech_f") escape)
   (text no_trim " " no_trim)
   (echo () fmt_bool (echo_var "ech_b") escape)
   (text no_trim "\n\nNumbers\n" no_trim)
   (match
    ((var "numbers"))
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
   (text no_trim "\n\nTrim\n" trim)
   (echo () fmt_string (echo_var "trim_a") escape)
   (text no_trim " " trim)
   (echo () fmt_string (echo_var "trim_b") escape)
   (text no_trim " " no_trim)
   (echo () fmt_string (echo_var "trim_c") escape)
   (text trim " " trim)
   (echo () fmt_string (echo_var "trim_d") escape)
   (text trim " " trim)
   (echo () fmt_string (echo_var "trim_e") no_escape)
   (text no_trim "\n" no_trim)
   (echo () fmt_string (echo_var "trim_f") no_escape)
   (text trim " " trim)
   (echo () fmt_string (echo_var "trim_g") no_escape)
   (text trim "\n\nComments\na b c\n\nFlat match\n" no_trim)
   (match
    ((var "match_a"))
    (((pats ((1) (2))) (nodes ((text no_trim "" no_trim))))
     ((pats ((3))) (nodes ((text no_trim " " no_trim))))
     ((pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n\nNested match\n" no_trim)
   (match
    ((var "match_b"))
    (((pats (((var "c"))))
      (nodes
       ((text no_trim "\n  " no_trim)
        (match
         ((var "d") (var "e"))
         (((pats (((var "f") (var "g"))))
           (nodes
            ((text no_trim " " no_trim)
             (echo () fmt_string (echo_var "c") escape)
             (text no_trim " " no_trim)
             (echo () fmt_string (echo_var "f") escape)
             (text no_trim " " no_trim)
             (echo () fmt_string (echo_var "g") escape)
             (text no_trim " " no_trim))))))
        (text no_trim "\n" no_trim))))))
   (text no_trim "\n\nMap list\n" no_trim)
   (map
    (var "map_l")
    (((pats ((1) (2))) (nodes ((text no_trim "" no_trim))))
     ((pats ((3 (var "i"))))
      (nodes
       ((text no_trim " " no_trim)
        (echo () fmt_int (echo_var "i") escape)
        (text no_trim " " no_trim))))
     ((pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n\nMap dict\n" no_trim)
   (map_dict
    (var "map_d")
    (((pats ((1) (2))) (nodes ((text no_trim "" no_trim))))
     ((pats ((3 (var "k"))))
      (nodes
       ((text no_trim " " no_trim)
        (echo () fmt_string (echo_var "k") escape)
        (text no_trim " " no_trim))))
     ((pats (((var "_")))) (nodes ((text no_trim "\n" no_trim))))))
   (text no_trim "\n\nComponent with props\n" no_trim)
   (component
    "Component"
    (("a_prop" (var "b_prop"))
     ("c_prop" (var "c_prop"))
     ("d_prop" (var "e_prop"))
     ("f_prop" (var "f_prop"))
     ("g_prop" (block ((text no_trim " " no_trim))))
     ("h_prop"
      (block
       ((text no_trim "" no_trim)
        (match
         ((var "a_prop"))
         (((pats (((var "b_prop"))))
           (nodes
            ((text no_trim " " no_trim)
             (echo () fmt_string (echo_var "b_prop") escape)
             (text no_trim " " no_trim))))))
        (text no_trim "" no_trim))))
     ("i_prop" (block ())))
    "Component")
   (text no_trim "\n\nComponent with implicit children\n" no_trim)
   (component
    "Component2"
    (("children" (block ((text no_trim " " no_trim)))))
    "Component2")
   (text no_trim "\n\nPatterns\n\nTuple:\n" no_trim)
   (match
    ((var "tuple"))
    (((pats (((1 2.5 "a")))) (nodes ((text no_trim " " no_trim))))
     ((pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n\nList:\n" no_trim)
   (match
    ((var "list"))
    (((pats (((list ())))) (nodes ((text no_trim "\n" no_trim))))
     ((pats (((list ((nullable (var "a")) null)))))
      (nodes
       ((text no_trim " " no_trim)
        (echo () fmt_string (echo_var "a") escape)
        (text no_trim "\n" no_trim))))
     ((pats (((list ((var "_z")) (var "_tl")))))
      (nodes ((text no_trim "\n" no_trim))))))
   (text no_trim "\n\nRecord:\n" no_trim)
   (match
    ((var "record"))
    (((pats (((record (("!#%@" (var "b")) ("a" (var "a")))))))
      (nodes
       ((text no_trim " " no_trim)
        (echo () fmt_string (echo_var "a") escape)
        (text no_trim " " no_trim)
        (echo () fmt_string (echo_var "b") escape)
        (text no_trim " " no_trim))))
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
      (nodes
       ((text no_trim " " no_trim)
        (echo () fmt_string (echo_var "a") escape)
        (text no_trim " " no_trim))))
     ((pats (((record (tagged ("tag" false) ())))))
      (nodes ((text no_trim "\n" no_trim))))))
   (text no_trim "\n\nDictionary:\n" no_trim)
   (match
    ((var "dict"))
    (((pats (((dict (("a" 1) ("b" 2)))))) (nodes ((text no_trim " " no_trim))))
     ((pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n\n! and . precedence works correctly\n" no_trim)
   (match
    ((nullable (nullable (field (field (var "a") "b") "c"))))
    (((pats (((nullable (nullable false)))))
      (nodes ((text no_trim "" no_trim))))
     ((pats (((var "_")))) (nodes ((text no_trim "" no_trim))))))
   (text
    no_trim
    "\n\nEdge cases\n\nPatterns with }} parse correctly\n"
    no_trim)
   (match
    ((var "a"))
    (((pats (((record (("a" (record (("b" (var "b"))))))))))
      (nodes
       ((text no_trim " " no_trim)
        (echo () fmt_string (echo_var "b") escape)
        (text no_trim " " no_trim))))))
   (text no_trim "\n" no_trim))

Interfaces parse correctly. Use a separate file to minimize type conficts.
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

Print the optimized form
  $ acutis template.acutis component.acutis component2.acutis --printopt
  ((text "Echoes\n")
   (echo () fmt_string (var "ech_a") escape)
   (text " ")
   (echo () fmt_string "b" escape)
   (text " ")
   (echo
    ((fmt_string (var "ech_d")) (fmt_string (var "ech_e")))
    fmt_string
    "f\"g"
    no_escape)
   (text "\n")
   (echo () fmt_int (var "ech_i") escape)
   (text " ")
   (echo () fmt_float (var "ech_f") escape)
   (text " ")
   (echo () fmt_bool (var "ech_b") escape)
   (text "\n\nNumbers\n")
   (match
    ((var "numbers"))
    ((tree
      (nest
       (key 0)
       (ids ())
       (child
        (string_keys
         (switch
          (key "exp1")
          (ids ())
          (cases
           ((data 150.)
            (if_match
             (switch
              (key "exp2")
              (ids ())
              (cases
               ((data -1000.)
                (if_match
                 (switch
                  (key "exp3")
                  (ids ())
                  (cases
                   ((data 0.2)
                    (if_match
                     (switch
                      (key "frac")
                      (ids ())
                      (cases
                       ((data 10.55)
                        (if_match
                         (switch
                          (key "int")
                          (ids ())
                          (cases
                           ((data 1000)
                            (if_match
                             (switch
                              (key "negfrac")
                              (ids ())
                              (cases
                               ((data -12.34)
                                (if_match
                                 (switch
                                  (key "negint")
                                  (ids ())
                                  (cases
                                   ((data -999)
                                    (if_match
                                     (end (end (leaf (names ()) (exit 0)))))
                                    (next ())))
                                  (wildcard ())
                                  (debug_row open)))
                                (next ())))
                              (wildcard ())
                              (debug_row open)))
                            (next ())))
                          (wildcard ())
                          (debug_row open)))
                        (next ())))
                      (wildcard ())
                      (debug_row open)))
                    (next ())))
                  (wildcard ())
                  (debug_row open)))
                (next ())))
              (wildcard ())
              (debug_row open)))
            (next ())))
          (wildcard ())
          (debug_row open))))
       (wildcard (end (leaf (names ()) (exit 1))))
       (debug not_dict)))
     (exits (0 ()) (1 ()))))
   (text "\n\nTrim")
   (echo () fmt_string (var "trim_a") escape)
   (echo () fmt_string (var "trim_b") escape)
   (text " ")
   (echo () fmt_string (var "trim_c") escape)
   (echo () fmt_string (var "trim_d") escape)
   (echo () fmt_string (var "trim_e") no_escape)
   (text "\n")
   (echo () fmt_string (var "trim_f") no_escape)
   (echo () fmt_string (var "trim_g") no_escape)
   (text "Comments\na b c\n\nFlat match\n")
   (match
    ((var "match_a"))
    ((tree
      (switch
       (key 0)
       (ids ())
       (cases
        ((data 1)
         (if_match (end (leaf (names ()) (exit 0))))
         (next
          ((data 2)
           (if_match (end (leaf (names ()) (exit 0))))
           (next
            ((data 3) (if_match (end (leaf (names ()) (exit 1)))) (next ())))))))
       (wildcard (end (leaf (names ()) (exit 2))))
       (debug_row open)))
     (exits (0 ()) (1 ((text " "))) (2 ((text " "))))))
   (text "\n\nNested match\n")
   (match
    ((var "match_b"))
    ((tree
      (wildcard
       (key 0)
       (ids (0))
       (child (end (leaf (names (("c" 0))) (exit 0))))))
     (exits
      (0
       ((text "\n  ")
        (match
         ((var "d") (var "e"))
         ((tree
           (wildcard
            (key 0)
            (ids (0))
            (child
             (wildcard
              (key 1)
              (ids (1))
              (child (end (leaf (names (("f" 0) ("g" 1))) (exit 0))))))))
          (exits
           (0
            ((text " ")
             (echo () fmt_string (var "c") escape)
             (text " ")
             (echo () fmt_string (var "f") escape)
             (text " ")
             (echo () fmt_string (var "g") escape)
             (text " "))))))
        (text "\n"))))))
   (text "\n\nMap list\n")
   (map_list
    (var "map_l")
    ((tree
      (switch
       (key 0)
       (ids ())
       (cases
        ((data 1)
         (if_match
          (wildcard (key 1) (ids ()) (child (end (leaf (names ()) (exit 0))))))
         (next
          ((data 2)
           (if_match
            (wildcard
             (key 1)
             (ids ())
             (child (end (leaf (names ()) (exit 0))))))
           (next
            ((data 3)
             (if_match
              (wildcard
               (key 1)
               (ids (0))
               (child (end (leaf (names (("i" 0))) (exit 1))))))
             (next ())))))))
       (wildcard
        (wildcard (key 1) (ids ()) (child (end (leaf (names ()) (exit 2))))))
       (debug_row open)))
     (exits
      (0 ())
      (1 ((text " ") (echo () fmt_int (var "i") escape) (text " ")))
      (2 ((text " "))))))
   (text "\n\nMap dict\n")
   (map_dict
    (var "map_d")
    ((tree
      (switch
       (key 0)
       (ids ())
       (cases
        ((data 1)
         (if_match
          (wildcard (key 1) (ids ()) (child (end (leaf (names ()) (exit 0))))))
         (next
          ((data 2)
           (if_match
            (wildcard
             (key 1)
             (ids ())
             (child (end (leaf (names ()) (exit 0))))))
           (next
            ((data 3)
             (if_match
              (wildcard
               (key 1)
               (ids (0))
               (child (end (leaf (names (("k" 0))) (exit 1))))))
             (next ())))))))
       (wildcard
        (wildcard (key 1) (ids ()) (child (end (leaf (names ()) (exit 2))))))
       (debug_row open)))
     (exits
      (0 ())
      (1 ((text " ") (echo () fmt_string (var "k") escape) (text " ")))
      (2 ((text "\n"))))))
   (text "\n\nComponent with props\n")
   (component
    "Component"
    (("a_prop" (var "b_prop"))
     ("c_prop" (var "c_prop"))
     ("d_prop" (var "e_prop"))
     ("f_prop" (var "f_prop"))
     ("g_prop" (block ((text " "))))
     ("h_prop"
      (block
       ((match
         ((var "a_prop"))
         ((tree
           (wildcard
            (key 0)
            (ids (0))
            (child (end (leaf (names (("b_prop" 0))) (exit 0))))))
          (exits
           (0
            ((text " ") (echo () fmt_string (var "b_prop") escape) (text " ")))))))))
     ("i_prop" (block ()))))
   (text "\n\nComponent with implicit children\n")
   (component "Component2" (("children" (block ((text " "))))))
   (text "\n\nPatterns\n\nTuple:\n")
   (match
    ((var "tuple"))
    ((tree
      (nest
       (key 0)
       (ids ())
       (child
        (int_keys
         (switch
          (key 0)
          (ids ())
          (cases
           ((data 1)
            (if_match
             (switch
              (key 1)
              (ids ())
              (cases
               ((data 2.5)
                (if_match
                 (switch
                  (key 2)
                  (ids ())
                  (cases
                   ((data "a")
                    (if_match (end (end (leaf (names ()) (exit 0)))))
                    (next ())))
                  (wildcard ())
                  (debug_row open)))
                (next ())))
              (wildcard ())
              (debug_row open)))
            (next ())))
          (wildcard ())
          (debug_row open))))
       (wildcard (end (leaf (names ()) (exit 1))))
       (debug not_dict)))
     (exits (0 ((text " "))) (1 ((text " "))))))
   (text "\n\nList:\n")
   (match
    ((var "list"))
    ((tree
      (construct
       (key 0)
       (ids ())
       (nil (end (leaf (names ()) (exit 0))))
       (cons
        (nest
         (key 0)
         (ids ())
         (child
          (int_keys
           (construct
            (key 0)
            (ids (1))
            (nil
             (wildcard
              (key 1)
              (ids (2))
              (child (end (end (leaf (names (("_tl" 2) ("_z" 1))) (exit 2)))))))
            (cons
             (nest
              (key 0)
              (ids (1))
              (child
               (int_keys
                (wildcard
                 (key 0)
                 (ids (0))
                 (child
                  (end
                   (construct
                    (key 1)
                    (ids (2))
                    (nil
                     (end (end (leaf (names (("_tl" 2) ("_z" 1))) (exit 2)))))
                    (cons
                     (nest
                      (key 1)
                      (ids (2))
                      (child
                       (int_keys
                        (construct
                         (key 0)
                         (ids ())
                         (nil
                          (construct
                           (key 1)
                           (ids ())
                           (nil
                            (end (end (end (leaf (names (("a" 0))) (exit 1))))))
                           (cons ())))
                         (cons ()))))
                      (wildcard
                       (end (end (leaf (names (("_tl" 2) ("_z" 1))) (exit 2)))))
                      (debug not_dict)))))))))
              (wildcard
               (wildcard
                (key 1)
                (ids (2))
                (child
                 (end (end (leaf (names (("_tl" 2) ("_z" 1))) (exit 2)))))))
              (debug not_dict))))))
         (wildcard ())
         (debug not_dict)))))
     (exits
      (0 ((text "\n")))
      (1 ((text " ") (echo () fmt_string (var "a") escape) (text "\n")))
      (2 ((text "\n"))))))
   (text "\n\nRecord:\n")
   (match
    ((var "record"))
    ((tree
      (nest
       (key 0)
       (ids ())
       (child
        (string_keys
         (wildcard
          (key "!#%@")
          (ids (0))
          (child
           (wildcard
            (key "a")
            (ids (1))
            (child (end (end (leaf (names (("a" 1) ("b" 0))) (exit 0))))))))))
       (wildcard (end (leaf (names ()) (exit 1))))
       (debug not_dict)))
     (exits
      (0
       ((text " ")
        (echo () fmt_string (var "a") escape)
        (text " ")
        (echo () fmt_string (var "b") escape)
        (text " ")))
      (1 ((text " "))))))
   (text "\n\nEnum:\n")
   (match
    ((var "enums"))
    ((tree
      (nest
       (key 0)
       (ids ())
       (child
        (int_keys
         (switch
          (key 0)
          (ids ())
          (cases
           ((data "a")
            (if_match
             (switch
              (key 1)
              (ids ())
              (cases
               ((data 1)
                (if_match
                 (switch
                  (key 2)
                  (ids ())
                  (cases
                   ((data 1)
                    (if_match
                     (switch
                      (key 3)
                      (ids ())
                      (cases
                       ((data 0)
                        (if_match (end (end (leaf (names ()) (exit 0)))))
                        (next ())))
                      (wildcard ())
                      (debug_row closed)))
                    (next ())))
                  (wildcard ())
                  (debug_row closed)))
                (next ())))
              (wildcard ())
              (debug_row open)))
            (next ())))
          (wildcard ())
          (debug_row open))))
       (wildcard (end (leaf (names ()) (exit 1))))
       (debug not_dict)))
     (exits (0 ((text " "))) (1 ((text " "))))))
   (text "\n\nTagged union:\n")
   (match
    ((var "tagged"))
    ((tree
      (nest
       (key 0)
       (ids ())
       (child
        (string_keys
         (switch
          (key "tag")
          (ids ())
          (cases
           ((data 0)
            (if_match (end (end (leaf (names ()) (exit 1)))))
            (next
             ((data 1)
              (if_match
               (wildcard
                (key "a")
                (ids (0))
                (child (end (end (leaf (names (("a" 0))) (exit 0)))))))
              (next ())))))
          (wildcard ())
          (debug_row closed))))
       (wildcard ())
       (debug not_dict)))
     (exits
      (0 ((text " ") (echo () fmt_string (var "a") escape) (text " ")))
      (1 ((text "\n"))))))
   (text "\n\nDictionary:\n")
   (match
    ((var "dict"))
    ((tree
      (nest
       (key 0)
       (ids ())
       (child
        (string_keys
         (switch
          (key "a")
          (ids ())
          (cases
           ((data 1)
            (if_match
             (switch
              (key "b")
              (ids ())
              (cases
               ((data 2)
                (if_match (end (end (leaf (names ()) (exit 0)))))
                (next ())))
              (wildcard ())
              (debug_row open)))
            (next ())))
          (wildcard ())
          (debug_row open))))
       (wildcard (end (leaf (names ()) (exit 1))))
       (debug dict)))
     (exits (0 ((text " "))) (1 ((text " "))))))
   (text "\n\n! and . precedence works correctly\n")
   (match
    ((array ((array ((field (field (var "a") "b") "c"))))))
    ((tree
      (construct
       (key 0)
       (ids ())
       (nil (end (leaf (names ()) (exit 1))))
       (cons
        (nest
         (key 0)
         (ids ())
         (child
          (int_keys
           (construct
            (key 0)
            (ids ())
            (nil ())
            (cons
             (nest
              (key 0)
              (ids ())
              (child
               (int_keys
                (switch
                 (key 0)
                 (ids ())
                 (cases
                  ((data 0)
                   (if_match (end (end (end (leaf (names ()) (exit 0))))))
                   (next ())))
                 (wildcard ())
                 (debug_row closed))))
              (wildcard ())
              (debug not_dict))))))
         (wildcard (end (leaf (names ()) (exit 1))))
         (debug not_dict)))))
     (exits (0 ()) (1 ()))))
   (text "\n\nEdge cases\n\nPatterns with }} parse correctly\n")
   (match
    ((var "a"))
    ((tree
      (nest
       (key 0)
       (ids ())
       (child
        (string_keys
         (nest
          (key "a")
          (ids ())
          (child
           (string_keys
            (wildcard
             (key "b")
             (ids (0))
             (child (end (end (end (leaf (names (("b" 0))) (exit 0)))))))))
          (wildcard ())
          (debug not_dict))))
       (wildcard ())
       (debug not_dict)))
     (exits (0 ((text " ") (echo () fmt_string (var "b") escape) (text " "))))))
   (text "\n"))
