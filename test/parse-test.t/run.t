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
    ((case
      (pats
       (((record
          (("int" (int 1000))
           ("frac" (float 10.55))
           ("negint" (int -999))
           ("negfrac" (float -12.34))
           ("exp1" (float 150.))
           ("exp2" (float -1000.))
           ("exp3" (float 0.2)))))))
      (nodes ((text no_trim "" no_trim))))
     (case (pats (((var "_")))) (nodes ((text no_trim "" no_trim))))))
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
   (text trim "\n\nComments\na " no_trim)
   (comment "{* {* *} *}")
   (text no_trim "b" no_trim)
   (comment "{*\n *}")
   (text no_trim " c\n\nFlat match\n" no_trim)
   (match
    ((var "match_a"))
    ((case (pats (((int 1)) ((int 2)))) (nodes ((text no_trim "" no_trim))))
     (case (pats (((int 3)))) (nodes ((text no_trim " " no_trim))))
     (case (pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n\nNested match\n" no_trim)
   (match
    ((var "match_b"))
    ((case
      (pats (((var "c"))))
      (nodes
       ((text no_trim "\n  " no_trim)
        (match
         ((var "d") (var "e"))
         ((case
           (pats (((var "f") (var "g"))))
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
    ((case (pats (((int 1)) ((int 2)))) (nodes ((text no_trim "" no_trim))))
     (case
      (pats (((int 3) (var "i"))))
      (nodes
       ((text no_trim " " no_trim)
        (echo () fmt_int (echo_var "i") escape)
        (text no_trim " " no_trim))))
     (case (pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n\nMap dict\n" no_trim)
   (map_dict
    (var "map_d")
    ((case (pats (((int 1)) ((int 2)))) (nodes ((text no_trim "" no_trim))))
     (case
      (pats (((int 3) (var "k"))))
      (nodes
       ((text no_trim " " no_trim)
        (echo () fmt_string (echo_var "k") escape)
        (text no_trim " " no_trim))))
     (case (pats (((var "_")))) (nodes ((text no_trim "\n" no_trim))))))
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
         ((case
           (pats (((var "b_prop"))))
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
    ((case
      (pats (((tuple ((int 1) (float 2.5) (string "a"))))))
      (nodes ((text no_trim " " no_trim))))
     (case (pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n\nList:\n" no_trim)
   (match
    ((var "list"))
    ((case (pats (((list ())))) (nodes ((text no_trim "\n" no_trim))))
     (case
      (pats (((list ((nullable (var "a")) (nullable null))))))
      (nodes
       ((text no_trim " " no_trim)
        (echo () fmt_string (echo_var "a") escape)
        (text no_trim "\n" no_trim))))
     (case
      (pats (((list ((var "_z")) (var "_tl")))))
      (nodes ((text no_trim "\n" no_trim))))))
   (text no_trim "\n\nRecord:\n" no_trim)
   (match
    ((var "record"))
    ((case
      (pats (((record (("a" (var "a")) ("!#%@" (var "b")))))))
      (nodes
       ((text no_trim " " no_trim)
        (echo () fmt_string (echo_var "a") escape)
        (text no_trim " " no_trim)
        (echo () fmt_string (echo_var "b") escape)
        (text no_trim " " no_trim))))
     (case (pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n\nEnum:\n" no_trim)
   (match
    ((var "enums"))
    ((case
      (pats
       (((tuple ((enum_string "a") (enum_int 1) (bool true) (bool false))))))
      (nodes ((text no_trim " " no_trim))))
     (case (pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n\nTagged union:\n" no_trim)
   (match
    ((var "tagged"))
    ((case
      (pats (((record (("tag" (tag true)) ("a" (var "a")))))))
      (nodes
       ((text no_trim " " no_trim)
        (echo () fmt_string (echo_var "a") escape)
        (text no_trim " " no_trim))))
     (case
      (pats (((record (("tag" (tag false)))))))
      (nodes ((text no_trim "\n" no_trim))))))
   (text no_trim "\n\nDictionary:\n" no_trim)
   (match
    ((var "dict"))
    ((case
      (pats (((dict (("a" (int 1)) ("b" (int 2)))))))
      (nodes ((text no_trim " " no_trim))))
     (case (pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n\n! and . precedence works correctly\n" no_trim)
   (match
    ((nullable (nullable (field (field (var "a") "b") "c"))))
    ((case
      (pats (((nullable (nullable (bool false))))))
      (nodes ((text no_trim "" no_trim))))
     (case (pats (((var "_")))) (nodes ((text no_trim "" no_trim))))))
   (text
    no_trim
    "\n\nOther syntax features\n\nPatterns with }} parse correctly:\n"
    no_trim)
   (match
    ((var "a"))
    ((case
      (pats (((record (("a" (record (("b" (var "b"))))))))))
      (nodes
       ((text no_trim " " no_trim)
        (echo () fmt_string (echo_var "b") escape)
        (text no_trim " " no_trim))))))
   (text no_trim "\n\nTrailing commas parse correctly:\n" no_trim)
   (match
    ((record
      (("a" (list ((int 1) (int 2))))
       ("b" (tuple ((int 3) (int 4))))
       ("c" (dict (("k" (int 5))))))))
    ((case (pats (((var "_")))) (nodes ((text no_trim " " no_trim))))))
   (text no_trim "\n\nStrings may contain line breaks:\n" no_trim)
   (echo () fmt_string (echo_string "a\nb") escape)
   (text no_trim "\n" no_trim))

Interfaces parse correctly. Use a separate file to minimize type conficts.
  $ acutis interface.acutis --printast
  ((text no_trim "" no_trim)
   (interface
    ((prop
      "a"
      (record
       ((("a" (enum_int (0 1) closed)) ("b" (enum_string ("a" "b") closed))))
       closed))
     (prop
      "b"
      (record
       ((("tag" (tag true)) ("a" (list (named "int"))))
        (("tag" (tag false)) ("a" (dict (nullable (named "string"))))))
       closed))
     (prop
      "c"
      (record
       ((("tag" (tag 0)))
        (("tag" (tag 1))
         ("a" (tuple ((named "float") (enum_bool (true false)))))))
       closed))
     (prop
      "d"
      (record
       ((("tag" (tag "a")) ("a" (named "float")))
        (("tag" (tag "b")) ("a" (enum_int (0 1) open))))
       closed))
     (prop
      "e"
      (record
       ((("tag" (tag 0)) ("a" (named "_")))
        (("tag" (tag 1)) ("b" (enum_string ("a" "b") open))))
       open))
     (prop
      "trailing_commas"
      (record ((("a" (tuple ((named "int") (named "string")))))) closed))
     (prop "children" (named "string"))
     (prop "optionalChildren" (nullable (named "string")))))
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
    ()
    ((var "numbers"))
    (matching
     (tree
      (nest
       (key 0)
       (ids ())
       (child
        (string_keys
         (switch
          (key "exp1")
          (ids ())
          (cases
           (case
            (data 150.)
            (if_match
             (switch
              (key "exp2")
              (ids ())
              (cases
               (case
                (data -1000.)
                (if_match
                 (switch
                  (key "exp3")
                  (ids ())
                  (cases
                   (case
                    (data 0.2)
                    (if_match
                     (switch
                      (key "frac")
                      (ids ())
                      (cases
                       (case
                        (data 10.55)
                        (if_match
                         (switch
                          (key "int")
                          (ids ())
                          (cases
                           (case
                            (data 1000)
                            (if_match
                             (switch
                              (key "negfrac")
                              (ids ())
                              (cases
                               (case
                                (data -12.34)
                                (if_match
                                 (switch
                                  (key "negint")
                                  (ids ())
                                  (cases
                                   (case
                                    (data -999)
                                    (if_match
                                     (end (end (leaf (names ()) (exit 0)))))
                                    (next none)))
                                  (wildcard none)
                                  (check_cases none)))
                                (next none)))
                              (wildcard none)
                              (check_cases none)))
                            (next none)))
                          (wildcard none)
                          (check_cases none)))
                        (next none)))
                      (wildcard none)
                      (check_cases none)))
                    (next none)))
                  (wildcard none)
                  (check_cases none)))
                (next none)))
              (wildcard none)
              (check_cases none)))
            (next none)))
          (wildcard none)
          (check_cases none))))
       (wildcard (end (leaf (names ()) (exit 1))))))
     (exits ((0 ()) (1 ())))))
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
   (text "Comments\na ")
   (text "b")
   (text " c\n\nFlat match\n")
   (match
    ()
    ((var "match_a"))
    (matching
     (tree
      (switch
       (key 0)
       (ids ())
       (cases
        (case
         (data 1)
         (if_match (end (leaf (names ()) (exit 0))))
         (next
          (case
           (data 2)
           (if_match (end (leaf (names ()) (exit 0))))
           (next
            (case
             (data 3)
             (if_match (end (leaf (names ()) (exit 1))))
             (next none)))))))
       (wildcard (end (leaf (names ()) (exit 2))))
       (check_cases none)))
     (exits ((0 ()) (1 ((text " "))) (2 ((text " ")))))))
   (text "\n\nNested match\n")
   (match
    ()
    ((var "match_b"))
    (matching
     (tree
      (wildcard
       (key 0)
       (ids (0))
       (child (end (leaf (names (("c" 0))) (exit 0))))))
     (exits
      ((0
        ((text "\n  ")
         (match
          ()
          ((var "d") (var "e"))
          (matching
           (tree
            (wildcard
             (key 0)
             (ids (0))
             (child
              (wildcard
               (key 1)
               (ids (1))
               (child (end (leaf (names (("f" 0) ("g" 1))) (exit 0))))))))
           (exits
            ((0
              ((text " ")
               (echo () fmt_string (var "c") escape)
               (text " ")
               (echo () fmt_string (var "f") escape)
               (text " ")
               (echo () fmt_string (var "g") escape)
               (text " ")))))))
         (text "\n")))))))
   (text "\n\nMap list\n")
   (map_list
    ()
    (var "map_l")
    (matching
     (tree
      (switch
       (key 0)
       (ids ())
       (cases
        (case
         (data 1)
         (if_match
          (wildcard (key 1) (ids ()) (child (end (leaf (names ()) (exit 0))))))
         (next
          (case
           (data 2)
           (if_match
            (wildcard
             (key 1)
             (ids ())
             (child (end (leaf (names ()) (exit 0))))))
           (next
            (case
             (data 3)
             (if_match
              (wildcard
               (key 1)
               (ids (0))
               (child (end (leaf (names (("i" 0))) (exit 1))))))
             (next none)))))))
       (wildcard
        (wildcard (key 1) (ids ()) (child (end (leaf (names ()) (exit 2))))))
       (check_cases none)))
     (exits
      ((0 ())
       (1 ((text " ") (echo () fmt_int (var "i") escape) (text " ")))
       (2 ((text " ")))))))
   (text "\n\nMap dict\n")
   (map_dict
    ()
    (var "map_d")
    (matching
     (tree
      (switch
       (key 0)
       (ids ())
       (cases
        (case
         (data 1)
         (if_match
          (wildcard (key 1) (ids ()) (child (end (leaf (names ()) (exit 0))))))
         (next
          (case
           (data 2)
           (if_match
            (wildcard
             (key 1)
             (ids ())
             (child (end (leaf (names ()) (exit 0))))))
           (next
            (case
             (data 3)
             (if_match
              (wildcard
               (key 1)
               (ids (0))
               (child (end (leaf (names (("k" 0))) (exit 1))))))
             (next none)))))))
       (wildcard
        (wildcard (key 1) (ids ()) (child (end (leaf (names ()) (exit 2))))))
       (check_cases none)))
     (exits
      ((0 ())
       (1 ((text " ") (echo () fmt_string (var "k") escape) (text " ")))
       (2 ((text "\n")))))))
   (text "\n\nComponent with props\n")
   (component
    ((0 ((text " ")))
     (1
      ((match
        ()
        ((var "a_prop"))
        (matching
         (tree
          (wildcard
           (key 0)
           (ids (0))
           (child (end (leaf (names (("b_prop" 0))) (exit 0))))))
         (exits
          ((0
            ((text " ") (echo () fmt_string (var "b_prop") escape) (text " ")))))))))
     (2 ()))
    "Component"
    (("a_prop" (var "b_prop"))
     ("c_prop" (var "c_prop"))
     ("d_prop" (var "e_prop"))
     ("f_prop" (var "f_prop"))
     ("g_prop" (block 0))
     ("h_prop" (block 1))
     ("i_prop" (block 2))))
   (text "\n\nComponent with implicit children\n")
   (component ((0 ((text " ")))) "Component2" (("children" (block 0))))
   (text "\n\nPatterns\n\nTuple:\n")
   (match
    ()
    ((var "tuple"))
    (matching
     (tree
      (nest
       (key 0)
       (ids ())
       (child
        (int_keys
         (switch
          (key 0)
          (ids ())
          (cases
           (case
            (data 1)
            (if_match
             (switch
              (key 1)
              (ids ())
              (cases
               (case
                (data 2.5)
                (if_match
                 (switch
                  (key 2)
                  (ids ())
                  (cases
                   (case
                    (data "a")
                    (if_match (end (end (leaf (names ()) (exit 0)))))
                    (next none)))
                  (wildcard none)
                  (check_cases none)))
                (next none)))
              (wildcard none)
              (check_cases none)))
            (next none)))
          (wildcard none)
          (check_cases none))))
       (wildcard (end (leaf (names ()) (exit 1))))))
     (exits ((0 ((text " "))) (1 ((text " ")))))))
   (text "\n\nList:\n")
   (match
    ()
    ((var "list"))
    (matching
     (tree
      (nil_or_cons
       (key 0)
       (ids ())
       (nil (end (leaf (names ()) (exit 0))))
       (cons
        (nest
         (key 0)
         (ids ())
         (child
          (int_keys
           (nil_or_cons
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
                   (nil_or_cons
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
                        (nil
                         (key 0)
                         (ids ())
                         (child
                          (nil
                           (key 1)
                           (ids ())
                           (child
                            (end (end (end (leaf (names (("a" 0))) (exit 1)))))))))))
                      (wildcard
                       (end (end (leaf (names (("_tl" 2) ("_z" 1))) (exit 2)))))))))))))
              (wildcard
               (wildcard
                (key 1)
                (ids (2))
                (child
                 (end (end (leaf (names (("_tl" 2) ("_z" 1))) (exit 2))))))))))))
         (wildcard none)))))
     (exits
      ((0 ((text "\n")))
       (1 ((text " ") (echo () fmt_string (var "a") escape) (text "\n")))
       (2 ((text "\n")))))))
   (text "\n\nRecord:\n")
   (match
    ()
    ((var "record"))
    (matching
     (tree
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
       (wildcard (end (leaf (names ()) (exit 1))))))
     (exits
      ((0
        ((text " ")
         (echo () fmt_string (var "a") escape)
         (text " ")
         (echo () fmt_string (var "b") escape)
         (text " ")))
       (1 ((text " ")))))))
   (text "\n\nEnum:\n")
   (match
    ()
    ((var "enums"))
    (matching
     (tree
      (nest
       (key 0)
       (ids ())
       (child
        (int_keys
         (switch
          (key 0)
          (ids ())
          (cases
           (case
            (data "a")
            (if_match
             (switch
              (key 1)
              (ids ())
              (cases
               (case
                (data 1)
                (if_match
                 (switch
                  (key 2)
                  (ids ())
                  (cases
                   (case
                    (data 1)
                    (if_match
                     (switch
                      (key 3)
                      (ids ())
                      (cases
                       (case
                        (data 0)
                        (if_match (end (end (leaf (names ()) (exit 0)))))
                        (next none)))
                      (wildcard none)
                      (check_cases (0 1))))
                    (next none)))
                  (wildcard none)
                  (check_cases (0 1))))
                (next none)))
              (wildcard none)
              (check_cases none)))
            (next none)))
          (wildcard none)
          (check_cases none))))
       (wildcard (end (leaf (names ()) (exit 1))))))
     (exits ((0 ((text " "))) (1 ((text " ")))))))
   (text "\n\nTagged union:\n")
   (match
    ()
    ((var "tagged"))
    (matching
     (tree
      (nest
       (key 0)
       (ids ())
       (child
        (string_keys
         (switch
          (key "tag")
          (ids ())
          (cases
           (case
            (data 0)
            (if_match (end (end (leaf (names ()) (exit 1)))))
            (next
             (case
              (data 1)
              (if_match
               (wildcard
                (key "a")
                (ids (0))
                (child (end (end (leaf (names (("a" 0))) (exit 0)))))))
              (next none)))))
          (wildcard none)
          (check_cases (0 1)))))
       (wildcard none)))
     (exits
      ((0 ((text " ") (echo () fmt_string (var "a") escape) (text " ")))
       (1 ((text "\n")))))))
   (text "\n\nDictionary:\n")
   (match
    ()
    ((var "dict"))
    (matching
     (tree
      (nest
       (key 0)
       (ids ())
       (child
        (string_keys
         (optional
          (child
           (switch
            (key "a")
            (ids ())
            (cases
             (case
              (data 1)
              (if_match
               (optional
                (child
                 (switch
                  (key "b")
                  (ids ())
                  (cases
                   (case
                    (data 2)
                    (if_match (end (end (leaf (names ()) (exit 0)))))
                    (next none)))
                  (wildcard none)
                  (check_cases none)))
                (next none)))
              (next none)))
            (wildcard none)
            (check_cases none)))
          (next none))))
       (wildcard (end (leaf (names ()) (exit 1))))))
     (exits ((0 ((text " "))) (1 ((text " ")))))))
   (text "\n\n! and . precedence works correctly\n")
   (match
    ()
    ((array ((array ((field (field (var "a") "b") "c"))))))
    (matching
     (tree
      (nil_or_cons
       (key 0)
       (ids ())
       (nil (end (leaf (names ()) (exit 1))))
       (cons
        (nest
         (key 0)
         (ids ())
         (child
          (int_keys
           (cons
            (key 0)
            (ids ())
            (child
             (nest
              (key 0)
              (ids ())
              (child
               (int_keys
                (switch
                 (key 0)
                 (ids ())
                 (cases
                  (case
                   (data 0)
                   (if_match (end (end (end (leaf (names ()) (exit 0))))))
                   (next none)))
                 (wildcard none)
                 (check_cases (0 1)))))
              (wildcard none))))))
         (wildcard (end (leaf (names ()) (exit 1))))))))
     (exits ((0 ()) (1 ())))))
   (text "\n\nOther syntax features\n\nPatterns with }} parse correctly:\n")
   (match
    ()
    ((var "a"))
    (matching
     (tree
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
          (wildcard none))))
       (wildcard none)))
     (exits
      ((0 ((text " ") (echo () fmt_string (var "b") escape) (text " ")))))))
   (text "\n\nTrailing commas parse correctly:\n")
   (match
    ()
    ((assoc
      (("a" (array (1 (array (2 null)))))
       ("b" (array (3 4)))
       ("c" (assoc (("k" 5)))))))
    (matching
     (tree
      (wildcard (key 0) (ids ()) (child (end (leaf (names ()) (exit 0))))))
     (exits ((0 ((text " ")))))))
   (text "\n\nStrings may contain line breaks:\n")
   (echo () fmt_string "a\nb" escape)
   (text "\n"))

Print the runtime instructions
  $ acutis template.acutis component.acutis component2.acutis --printinst
  (let$ acutis_escape/0 = (escape))
  (let$ buffer_contents/0 = (buffer_contents))
  (let$ components/0 = (hashtbl_create))
  (unit)
  (components/0.%{"Component"} <-
   (lambda arg/0
    ((let$ buffer/0 = (buffer_create))
     (buffer_append buffer/0
      (promise (acutis_escape/0 @@ (Data.to_string (arg/0.%{"a_prop"})))))
     (buffer_append buffer/0 (promise "\n"))
     (buffer_append buffer/0
      (promise (acutis_escape/0 @@ (Data.to_string (arg/0.%{"c_prop"})))))
     (buffer_append buffer/0 (promise "\n"))
     (buffer_append buffer/0
      (promise (acutis_escape/0 @@ (Data.to_string (arg/0.%{"d_prop"})))))
     (buffer_append buffer/0 (promise "\n"))
     (buffer_append buffer/0
      (promise (acutis_escape/0 @@ (Data.to_string (arg/0.%{"f_prop"})))))
     (buffer_append buffer/0 (promise "\n"))
     (buffer_append buffer/0
      (promise (acutis_escape/0 @@ (Data.to_string (arg/0.%{"g_prop"})))))
     (buffer_append buffer/0 (promise "\n"))
     (buffer_append buffer/0
      (promise (acutis_escape/0 @@ (Data.to_string (arg/0.%{"h_prop"})))))
     (buffer_append buffer/0 (promise "\n"))
     (buffer_append buffer/0
      (promise (acutis_escape/0 @@ (Data.to_string (arg/0.%{"i_prop"})))))
     (buffer_append buffer/0 (promise "\n"))
     (return (buffer_contents/0 @@ buffer/0)))))
  (components/0.%{"Component2"} <-
   (lambda arg/1
    ((let$ buffer/1 = (buffer_create))
     (buffer_append buffer/1
      (promise (acutis_escape/0 @@ (Data.to_string (arg/1.%{"children"})))))
     (buffer_append buffer/1 (promise "\n"))
     (return (buffer_contents/0 @@ buffer/1)))))
  (export
   (lambda arg/2
    ((let$ stack/0 = (stack_create))
     (let$ errors/0 = (stack_create))
     (let$ decode_error/0 =
      (lambda arg/3
       ((return
         (lambda arg/4
          ((return
            (array_concat
             ["File \"",
              "template.acutis",
              "\"\nRender error.\nThe data supplied does not match this template's interface.\n",
              "Path:\n",
              (stack_concat stack/0 " <- "),
              "\nExpected type:\n",
              arg/3,
              "\nReceived value:\n",
              (External.show arg/4)]
             ""))))))))
     (let$ key_error/0 =
      (lambda arg/5
       ((return
         (lambda arg/6
          ((return
            (array_concat
             ["File: ",
              "template.acutis",
              "\nRender error.\nThe data supplied does not match this template's interface.\n",
              "Path:\n",
              (stack_concat stack/0 " <- "),
              "\nExpected type:\n",
              arg/5,
              "\nInput is missing keys:\n",
              (stack_concat arg/6 ", ")]
             ""))))))))
     (let$ props/0 = (hashtbl_create))
     (stack_push stack/0 "<input>")
     (let$ type/0 =
      "{\n  a: {a: {b: string}, b: {c: false | true}},\n  a_prop: string,\n  b_prop: string,\n  c_prop: string,\n  d: string,\n  dict: <int>,\n  e: string,\n  e_prop: string,\n  ech_a: string,\n  ech_b: false | true,\n  ech_d: ?string,\n  ech_e: ?string,\n  ech_f: float,\n  ech_i: int,\n  enums: (@\"a\" | ..., @1 | ..., false | true, false | true),\n  f_prop: string,\n  list: [?string],\n  map_d: <int>,\n  map_l: [int],\n  match_a: int,\n  match_b: string,\n  numbers:\n    {\n      exp1: float,\n      exp2: float,\n      exp3: float,\n      frac: float,\n      int: int,\n      negfrac: float,\n      negint: int\n    },\n  record: {\"!#%@\": string, a: string},\n  tagged: {@tag: false} | {@tag: true, a: string},\n  trim_a: string,\n  trim_b: string,\n  trim_c: string,\n  trim_d: string,\n  trim_e: string,\n  trim_f: string,\n  trim_g: string,\n  tuple: (int, float, string)\n}")
     (External.classify (assoc) arg/2 classified/0
      (ok
       (let$ missing_keys/0 = (stack_create))
       (if_else (External.Assoc.mem classified/0 "a")
        (then
         (let$ input/0 = (External.Assoc.find classified/0 "a"))
         (stack_push stack/0 "a")
         (let$ type/1 = "{a: {b: string}, b: {c: false | true}}")
         (External.classify (assoc) input/0 classified/1
          (ok
           (let$ decoded/0 = (hashtbl_create))
           (let$ missing_keys/1 = (stack_create))
           (if_else (External.Assoc.mem classified/1 "a")
            (then
             (let$ input/1 = (External.Assoc.find classified/1 "a"))
             (stack_push stack/0 "a")
             (let$ type/2 = "{b: string}")
             (External.classify (assoc) input/1 classified/2
              (ok
               (let$ decoded/1 = (hashtbl_create))
               (let$ missing_keys/2 = (stack_create))
               (if_else (External.Assoc.mem classified/2 "b")
                (then
                 (let$ input/2 = (External.Assoc.find classified/2 "b"))
                 (stack_push stack/0 "b")
                 (let$ type/3 = "string")
                 (External.classify (string) input/2 classified/3
                  (ok (decoded/1.%{"b"} <- (Data.string classified/3)))
                  (error
                   (stack_push errors/0 ((decode_error/0 @@ type/3) @@ input/2))))
                 (stack_drop stack/0))
                (else (stack_push missing_keys/2 "b")))
               (if (not (stack_is_empty missing_keys/2))
                (then
                 (stack_push errors/0
                  ((key_error/0 @@ type/2) @@ missing_keys/2))))
               (decoded/0.%{"a"} <- (Data.hashtbl decoded/1)))
              (error
               (stack_push errors/0 ((decode_error/0 @@ type/2) @@ input/1))))
             (stack_drop stack/0))
            (else (stack_push missing_keys/1 "a")))
           (if_else (External.Assoc.mem classified/1 "b")
            (then
             (let$ input/3 = (External.Assoc.find classified/1 "b"))
             (stack_push stack/0 "b")
             (let$ type/4 = "{c: false | true}")
             (External.classify (assoc) input/3 classified/4
              (ok
               (let$ decoded/2 = (hashtbl_create))
               (let$ missing_keys/3 = (stack_create))
               (if_else (External.Assoc.mem classified/4 "c")
                (then
                 (let$ input/4 = (External.Assoc.find classified/4 "c"))
                 (stack_push stack/0 "c")
                 (let$ type/5 = "false | true")
                 (External.classify (bool) input/4 classified/5
                  (ok
                   (if_else classified/5
                    (then (decoded/2.%{"c"} <- (Data.int 1)))
                    (else (decoded/2.%{"c"} <- (Data.int 0)))))
                  (error
                   (stack_push errors/0 ((decode_error/0 @@ type/5) @@ input/4))))
                 (stack_drop stack/0))
                (else (stack_push missing_keys/3 "c")))
               (if (not (stack_is_empty missing_keys/3))
                (then
                 (stack_push errors/0
                  ((key_error/0 @@ type/4) @@ missing_keys/3))))
               (decoded/0.%{"b"} <- (Data.hashtbl decoded/2)))
              (error
               (stack_push errors/0 ((decode_error/0 @@ type/4) @@ input/3))))
             (stack_drop stack/0))
            (else (stack_push missing_keys/1 "b")))
           (if (not (stack_is_empty missing_keys/1))
            (then
             (stack_push errors/0 ((key_error/0 @@ type/1) @@ missing_keys/1))))
           (props/0.%{"a"} <- (Data.hashtbl decoded/0)))
          (error (stack_push errors/0 ((decode_error/0 @@ type/1) @@ input/0))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "a")))
       (if_else (External.Assoc.mem classified/0 "a_prop")
        (then
         (let$ input/5 = (External.Assoc.find classified/0 "a_prop"))
         (stack_push stack/0 "a_prop")
         (let$ type/6 = "string")
         (External.classify (string) input/5 classified/6
          (ok (props/0.%{"a_prop"} <- (Data.string classified/6)))
          (error (stack_push errors/0 ((decode_error/0 @@ type/6) @@ input/5))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "a_prop")))
       (if_else (External.Assoc.mem classified/0 "b_prop")
        (then
         (let$ input/6 = (External.Assoc.find classified/0 "b_prop"))
         (stack_push stack/0 "b_prop")
         (let$ type/7 = "string")
         (External.classify (string) input/6 classified/7
          (ok (props/0.%{"b_prop"} <- (Data.string classified/7)))
          (error (stack_push errors/0 ((decode_error/0 @@ type/7) @@ input/6))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "b_prop")))
       (if_else (External.Assoc.mem classified/0 "c_prop")
        (then
         (let$ input/7 = (External.Assoc.find classified/0 "c_prop"))
         (stack_push stack/0 "c_prop")
         (let$ type/8 = "string")
         (External.classify (string) input/7 classified/8
          (ok (props/0.%{"c_prop"} <- (Data.string classified/8)))
          (error (stack_push errors/0 ((decode_error/0 @@ type/8) @@ input/7))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "c_prop")))
       (if_else (External.Assoc.mem classified/0 "d")
        (then
         (let$ input/8 = (External.Assoc.find classified/0 "d"))
         (stack_push stack/0 "d")
         (let$ type/9 = "string")
         (External.classify (string) input/8 classified/9
          (ok (props/0.%{"d"} <- (Data.string classified/9)))
          (error (stack_push errors/0 ((decode_error/0 @@ type/9) @@ input/8))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "d")))
       (if_else (External.Assoc.mem classified/0 "dict")
        (then
         (let$ input/9 = (External.Assoc.find classified/0 "dict"))
         (stack_push stack/0 "dict")
         (let$ type/10 = "<int>")
         (External.classify (assoc) input/9 classified/10
          (ok
           (let$ decoded/3 = (hashtbl_create))
           (External.Assoc.iter classified/10 key/0 value/0
            (stack_push stack/0 key/0) (let$ type/11 = "int")
            (External.classify (int) value/0 classified/11
             (ok (decoded/3.%{key/0} <- (Data.int classified/11)))
             (error
              (stack_push errors/0 ((decode_error/0 @@ type/11) @@ value/0))))
            (props/0.%{"dict"} <- (Data.hashtbl decoded/3))
            (stack_drop stack/0)))
          (error (stack_push errors/0 ((decode_error/0 @@ type/10) @@ input/9))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "dict")))
       (if_else (External.Assoc.mem classified/0 "e")
        (then
         (let$ input/10 = (External.Assoc.find classified/0 "e"))
         (stack_push stack/0 "e")
         (let$ type/12 = "string")
         (External.classify (string) input/10 classified/12
          (ok (props/0.%{"e"} <- (Data.string classified/12)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/12) @@ input/10))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "e")))
       (if_else (External.Assoc.mem classified/0 "e_prop")
        (then
         (let$ input/11 = (External.Assoc.find classified/0 "e_prop"))
         (stack_push stack/0 "e_prop")
         (let$ type/13 = "string")
         (External.classify (string) input/11 classified/13
          (ok (props/0.%{"e_prop"} <- (Data.string classified/13)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/13) @@ input/11))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "e_prop")))
       (if_else (External.Assoc.mem classified/0 "ech_a")
        (then
         (let$ input/12 = (External.Assoc.find classified/0 "ech_a"))
         (stack_push stack/0 "ech_a")
         (let$ type/14 = "string")
         (External.classify (string) input/12 classified/14
          (ok (props/0.%{"ech_a"} <- (Data.string classified/14)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/14) @@ input/12))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "ech_a")))
       (if_else (External.Assoc.mem classified/0 "ech_b")
        (then
         (let$ input/13 = (External.Assoc.find classified/0 "ech_b"))
         (stack_push stack/0 "ech_b")
         (let$ type/15 = "false | true")
         (External.classify (bool) input/13 classified/15
          (ok
           (if_else classified/15
            (then (props/0.%{"ech_b"} <- (Data.int 1)))
            (else (props/0.%{"ech_b"} <- (Data.int 0)))))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/15) @@ input/13))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "ech_b")))
       (if_else (External.Assoc.mem classified/0 "ech_d")
        (then
         (let$ input/14 = (External.Assoc.find classified/0 "ech_d"))
         (stack_push stack/0 "ech_d")
         (let$ type/16 = "?string")
         (if_else (External.is_null input/14)
          (then (props/0.%{"ech_d"} <- (Data.int 0)))
          (else
           (let$ decoded/4 = [(Data.int 0)])
           (stack_push stack/0 "<nullable>")
           (let$ type/17 = "string")
           (External.classify (string) input/14 classified/16
            (ok (decoded/4.%(0) <- (Data.string classified/16)))
            (error
             (stack_push errors/0 ((decode_error/0 @@ type/17) @@ input/14))))
           (props/0.%{"ech_d"} <- (Data.array decoded/4))
           (stack_drop stack/0)))
         (stack_drop stack/0))
        (else (props/0.%{"ech_d"} <- (Data.int 0))))
       (if_else (External.Assoc.mem classified/0 "ech_e")
        (then
         (let$ input/15 = (External.Assoc.find classified/0 "ech_e"))
         (stack_push stack/0 "ech_e")
         (let$ type/18 = "?string")
         (if_else (External.is_null input/15)
          (then (props/0.%{"ech_e"} <- (Data.int 0)))
          (else
           (let$ decoded/5 = [(Data.int 0)])
           (stack_push stack/0 "<nullable>")
           (let$ type/19 = "string")
           (External.classify (string) input/15 classified/17
            (ok (decoded/5.%(0) <- (Data.string classified/17)))
            (error
             (stack_push errors/0 ((decode_error/0 @@ type/19) @@ input/15))))
           (props/0.%{"ech_e"} <- (Data.array decoded/5))
           (stack_drop stack/0)))
         (stack_drop stack/0))
        (else (props/0.%{"ech_e"} <- (Data.int 0))))
       (if_else (External.Assoc.mem classified/0 "ech_f")
        (then
         (let$ input/16 = (External.Assoc.find classified/0 "ech_f"))
         (stack_push stack/0 "ech_f")
         (let$ type/20 = "float")
         (External.classify (float) input/16 classified/18
          (ok (props/0.%{"ech_f"} <- (Data.float classified/18)))
          (error
           (External.classify (int) input/16 classified/19
            (ok
             (props/0.%{"ech_f"} <- (Data.float (int_to_float classified/19))))
            (error
             (stack_push errors/0 ((decode_error/0 @@ type/20) @@ input/16))))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "ech_f")))
       (if_else (External.Assoc.mem classified/0 "ech_i")
        (then
         (let$ input/17 = (External.Assoc.find classified/0 "ech_i"))
         (stack_push stack/0 "ech_i")
         (let$ type/21 = "int")
         (External.classify (int) input/17 classified/20
          (ok (props/0.%{"ech_i"} <- (Data.int classified/20)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/21) @@ input/17))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "ech_i")))
       (if_else (External.Assoc.mem classified/0 "enums")
        (then
         (let$ input/18 = (External.Assoc.find classified/0 "enums"))
         (stack_push stack/0 "enums")
         (let$ type/22 = "(@\"a\" | ..., @1 | ..., false | true, false | true)")
         (External.classify (linear) input/18 classified/21
          (ok
           (if_else (equal_int (External.Linear.length classified/21) 4)
            (then
             (let$ decoded/6 = (array_init 4 (Data.int 0)))
             (External.Linear.iteri classified/21 key/1 value/1
              (stack_push stack/0 (int_to_string key/1))
              (if_else (equal_int key/1 0)
               (then
                (let$ type/26 = "@\"a\" | ...")
                (External.classify (string) value/1 classified/25
                 (ok (decoded/6.%(key/1) <- (Data.string classified/25)))
                 (error
                  (stack_push errors/0 ((decode_error/0 @@ type/26) @@ value/1)))))
               (else
                (if_else (equal_int key/1 1)
                 (then
                  (let$ type/25 = "@1 | ...")
                  (External.classify (int) value/1 classified/24
                   (ok (decoded/6.%(key/1) <- (Data.int classified/24)))
                   (error
                    (stack_push errors/0
                     ((decode_error/0 @@ type/25) @@ value/1)))))
                 (else
                  (if_else (equal_int key/1 2)
                   (then
                    (let$ type/24 = "false | true")
                    (External.classify (bool) value/1 classified/23
                     (ok
                      (if_else classified/23
                       (then (decoded/6.%(key/1) <- (Data.int 1)))
                       (else (decoded/6.%(key/1) <- (Data.int 0)))))
                     (error
                      (stack_push errors/0
                       ((decode_error/0 @@ type/24) @@ value/1)))))
                   (else
                    (if_else (equal_int key/1 3)
                     (then
                      (let$ type/23 = "false | true")
                      (External.classify (bool) value/1 classified/22
                       (ok
                        (if_else classified/22
                         (then (decoded/6.%(key/1) <- (Data.int 1)))
                         (else (decoded/6.%(key/1) <- (Data.int 0)))))
                       (error
                        (stack_push errors/0
                         ((decode_error/0 @@ type/23) @@ value/1)))))
                     (else
                      (stack_push errors/0
                       ((decode_error/0 @@ type/22) @@ value/1))))))))))
              (props/0.%{"enums"} <- (Data.array decoded/6))
              (stack_drop stack/0)))
            (else
             (stack_push errors/0 ((decode_error/0 @@ type/22) @@ input/18)))))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/22) @@ input/18))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "enums")))
       (if_else (External.Assoc.mem classified/0 "f_prop")
        (then
         (let$ input/19 = (External.Assoc.find classified/0 "f_prop"))
         (stack_push stack/0 "f_prop")
         (let$ type/27 = "string")
         (External.classify (string) input/19 classified/26
          (ok (props/0.%{"f_prop"} <- (Data.string classified/26)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/27) @@ input/19))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "f_prop")))
       (if_else (External.Assoc.mem classified/0 "list")
        (then
         (let$ input/20 = (External.Assoc.find classified/0 "list"))
         (stack_push stack/0 "list")
         (let$ type/28 = "[?string]")
         (External.classify (linear) input/20 classified/27
          (ok
           (let$ decoded/7 = [(Data.int 0), (Data.int 0)])
           (let& decode_dst/0 = decoded/7)
           (External.Linear.iteri classified/27 key/2 value/2
            (let$ decode_dst_new/0 = [(Data.int 0), (Data.int 0)])
            (stack_push stack/0 (int_to_string key/2))
            (let$ type/29 = "?string")
            (if_else (External.is_null value/2)
             (then (decode_dst_new/0.%(0) <- (Data.int 0)))
             (else
              (let$ decoded/8 = [(Data.int 0)])
              (stack_push stack/0 "<nullable>")
              (let$ type/30 = "string")
              (External.classify (string) value/2 classified/28
               (ok (decoded/8.%(0) <- (Data.string classified/28)))
               (error
                (stack_push errors/0 ((decode_error/0 @@ type/30) @@ value/2))))
              (decode_dst_new/0.%(0) <- (Data.array decoded/8))
              (stack_drop stack/0)))
            ((deref decode_dst/0).%(1) <- (Data.array decode_dst_new/0))
            (decode_dst/0 := decode_dst_new/0) (stack_drop stack/0))
           (props/0.%{"list"} <- (decoded/7.%(1))))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/28) @@ input/20))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "list")))
       (if_else (External.Assoc.mem classified/0 "map_d")
        (then
         (let$ input/21 = (External.Assoc.find classified/0 "map_d"))
         (stack_push stack/0 "map_d")
         (let$ type/31 = "<int>")
         (External.classify (assoc) input/21 classified/29
          (ok
           (let$ decoded/9 = (hashtbl_create))
           (External.Assoc.iter classified/29 key/3 value/3
            (stack_push stack/0 key/3) (let$ type/32 = "int")
            (External.classify (int) value/3 classified/30
             (ok (decoded/9.%{key/3} <- (Data.int classified/30)))
             (error
              (stack_push errors/0 ((decode_error/0 @@ type/32) @@ value/3))))
            (props/0.%{"map_d"} <- (Data.hashtbl decoded/9))
            (stack_drop stack/0)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/31) @@ input/21))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "map_d")))
       (if_else (External.Assoc.mem classified/0 "map_l")
        (then
         (let$ input/22 = (External.Assoc.find classified/0 "map_l"))
         (stack_push stack/0 "map_l")
         (let$ type/33 = "[int]")
         (External.classify (linear) input/22 classified/31
          (ok
           (let$ decoded/10 = [(Data.int 0), (Data.int 0)])
           (let& decode_dst/1 = decoded/10)
           (External.Linear.iteri classified/31 key/4 value/4
            (let$ decode_dst_new/1 = [(Data.int 0), (Data.int 0)])
            (stack_push stack/0 (int_to_string key/4)) (let$ type/34 = "int")
            (External.classify (int) value/4 classified/32
             (ok (decode_dst_new/1.%(0) <- (Data.int classified/32)))
             (error
              (stack_push errors/0 ((decode_error/0 @@ type/34) @@ value/4))))
            ((deref decode_dst/1).%(1) <- (Data.array decode_dst_new/1))
            (decode_dst/1 := decode_dst_new/1) (stack_drop stack/0))
           (props/0.%{"map_l"} <- (decoded/10.%(1))))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/33) @@ input/22))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "map_l")))
       (if_else (External.Assoc.mem classified/0 "match_a")
        (then
         (let$ input/23 = (External.Assoc.find classified/0 "match_a"))
         (stack_push stack/0 "match_a")
         (let$ type/35 = "int")
         (External.classify (int) input/23 classified/33
          (ok (props/0.%{"match_a"} <- (Data.int classified/33)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/35) @@ input/23))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "match_a")))
       (if_else (External.Assoc.mem classified/0 "match_b")
        (then
         (let$ input/24 = (External.Assoc.find classified/0 "match_b"))
         (stack_push stack/0 "match_b")
         (let$ type/36 = "string")
         (External.classify (string) input/24 classified/34
          (ok (props/0.%{"match_b"} <- (Data.string classified/34)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/36) @@ input/24))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "match_b")))
       (if_else (External.Assoc.mem classified/0 "numbers")
        (then
         (let$ input/25 = (External.Assoc.find classified/0 "numbers"))
         (stack_push stack/0 "numbers")
         (let$ type/37 =
          "{\n  exp1: float,\n  exp2: float,\n  exp3: float,\n  frac: float,\n  int: int,\n  negfrac: float,\n  negint: int\n}")
         (External.classify (assoc) input/25 classified/35
          (ok
           (let$ decoded/11 = (hashtbl_create))
           (let$ missing_keys/4 = (stack_create))
           (if_else (External.Assoc.mem classified/35 "exp1")
            (then
             (let$ input/26 = (External.Assoc.find classified/35 "exp1"))
             (stack_push stack/0 "exp1")
             (let$ type/38 = "float")
             (External.classify (float) input/26 classified/36
              (ok (decoded/11.%{"exp1"} <- (Data.float classified/36)))
              (error
               (External.classify (int) input/26 classified/37
                (ok
                 (decoded/11.%{"exp1"} <-
                  (Data.float (int_to_float classified/37))))
                (error
                 (stack_push errors/0 ((decode_error/0 @@ type/38) @@ input/26))))))
             (stack_drop stack/0))
            (else (stack_push missing_keys/4 "exp1")))
           (if_else (External.Assoc.mem classified/35 "exp2")
            (then
             (let$ input/27 = (External.Assoc.find classified/35 "exp2"))
             (stack_push stack/0 "exp2")
             (let$ type/39 = "float")
             (External.classify (float) input/27 classified/38
              (ok (decoded/11.%{"exp2"} <- (Data.float classified/38)))
              (error
               (External.classify (int) input/27 classified/39
                (ok
                 (decoded/11.%{"exp2"} <-
                  (Data.float (int_to_float classified/39))))
                (error
                 (stack_push errors/0 ((decode_error/0 @@ type/39) @@ input/27))))))
             (stack_drop stack/0))
            (else (stack_push missing_keys/4 "exp2")))
           (if_else (External.Assoc.mem classified/35 "exp3")
            (then
             (let$ input/28 = (External.Assoc.find classified/35 "exp3"))
             (stack_push stack/0 "exp3")
             (let$ type/40 = "float")
             (External.classify (float) input/28 classified/40
              (ok (decoded/11.%{"exp3"} <- (Data.float classified/40)))
              (error
               (External.classify (int) input/28 classified/41
                (ok
                 (decoded/11.%{"exp3"} <-
                  (Data.float (int_to_float classified/41))))
                (error
                 (stack_push errors/0 ((decode_error/0 @@ type/40) @@ input/28))))))
             (stack_drop stack/0))
            (else (stack_push missing_keys/4 "exp3")))
           (if_else (External.Assoc.mem classified/35 "frac")
            (then
             (let$ input/29 = (External.Assoc.find classified/35 "frac"))
             (stack_push stack/0 "frac")
             (let$ type/41 = "float")
             (External.classify (float) input/29 classified/42
              (ok (decoded/11.%{"frac"} <- (Data.float classified/42)))
              (error
               (External.classify (int) input/29 classified/43
                (ok
                 (decoded/11.%{"frac"} <-
                  (Data.float (int_to_float classified/43))))
                (error
                 (stack_push errors/0 ((decode_error/0 @@ type/41) @@ input/29))))))
             (stack_drop stack/0))
            (else (stack_push missing_keys/4 "frac")))
           (if_else (External.Assoc.mem classified/35 "int")
            (then
             (let$ input/30 = (External.Assoc.find classified/35 "int"))
             (stack_push stack/0 "int")
             (let$ type/42 = "int")
             (External.classify (int) input/30 classified/44
              (ok (decoded/11.%{"int"} <- (Data.int classified/44)))
              (error
               (stack_push errors/0 ((decode_error/0 @@ type/42) @@ input/30))))
             (stack_drop stack/0))
            (else (stack_push missing_keys/4 "int")))
           (if_else (External.Assoc.mem classified/35 "negfrac")
            (then
             (let$ input/31 = (External.Assoc.find classified/35 "negfrac"))
             (stack_push stack/0 "negfrac")
             (let$ type/43 = "float")
             (External.classify (float) input/31 classified/45
              (ok (decoded/11.%{"negfrac"} <- (Data.float classified/45)))
              (error
               (External.classify (int) input/31 classified/46
                (ok
                 (decoded/11.%{"negfrac"} <-
                  (Data.float (int_to_float classified/46))))
                (error
                 (stack_push errors/0 ((decode_error/0 @@ type/43) @@ input/31))))))
             (stack_drop stack/0))
            (else (stack_push missing_keys/4 "negfrac")))
           (if_else (External.Assoc.mem classified/35 "negint")
            (then
             (let$ input/32 = (External.Assoc.find classified/35 "negint"))
             (stack_push stack/0 "negint")
             (let$ type/44 = "int")
             (External.classify (int) input/32 classified/47
              (ok (decoded/11.%{"negint"} <- (Data.int classified/47)))
              (error
               (stack_push errors/0 ((decode_error/0 @@ type/44) @@ input/32))))
             (stack_drop stack/0))
            (else (stack_push missing_keys/4 "negint")))
           (if (not (stack_is_empty missing_keys/4))
            (then
             (stack_push errors/0 ((key_error/0 @@ type/37) @@ missing_keys/4))))
           (props/0.%{"numbers"} <- (Data.hashtbl decoded/11)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/37) @@ input/25))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "numbers")))
       (if_else (External.Assoc.mem classified/0 "record")
        (then
         (let$ input/33 = (External.Assoc.find classified/0 "record"))
         (stack_push stack/0 "record")
         (let$ type/45 = "{\"!#%@\": string, a: string}")
         (External.classify (assoc) input/33 classified/48
          (ok
           (let$ decoded/12 = (hashtbl_create))
           (let$ missing_keys/5 = (stack_create))
           (if_else (External.Assoc.mem classified/48 "!#%@")
            (then
             (let$ input/34 = (External.Assoc.find classified/48 "!#%@"))
             (stack_push stack/0 "!#%@")
             (let$ type/46 = "string")
             (External.classify (string) input/34 classified/49
              (ok (decoded/12.%{"!#%@"} <- (Data.string classified/49)))
              (error
               (stack_push errors/0 ((decode_error/0 @@ type/46) @@ input/34))))
             (stack_drop stack/0))
            (else (stack_push missing_keys/5 "!#%@")))
           (if_else (External.Assoc.mem classified/48 "a")
            (then
             (let$ input/35 = (External.Assoc.find classified/48 "a"))
             (stack_push stack/0 "a")
             (let$ type/47 = "string")
             (External.classify (string) input/35 classified/50
              (ok (decoded/12.%{"a"} <- (Data.string classified/50)))
              (error
               (stack_push errors/0 ((decode_error/0 @@ type/47) @@ input/35))))
             (stack_drop stack/0))
            (else (stack_push missing_keys/5 "a")))
           (if (not (stack_is_empty missing_keys/5))
            (then
             (stack_push errors/0 ((key_error/0 @@ type/45) @@ missing_keys/5))))
           (props/0.%{"record"} <- (Data.hashtbl decoded/12)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/45) @@ input/33))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "record")))
       (if_else (External.Assoc.mem classified/0 "tagged")
        (then
         (let$ input/36 = (External.Assoc.find classified/0 "tagged"))
         (stack_push stack/0 "tagged")
         (let$ type/48 = "{@tag: false} | {@tag: true, a: string}")
         (External.classify (assoc) input/36 classified/51
          (ok
           (if_else (External.Assoc.mem classified/51 "tag")
            (then
             (External.classify (bool)
              (External.Assoc.find classified/51 "tag") classified/52
              (ok
               (if_else classified/52
                (then
                 (let$ decoded/14 = (hashtbl_create))
                 (decoded/14.%{"tag"} <- (Data.int 1))
                 (let$ missing_keys/7 = (stack_create))
                 (if_else (External.Assoc.mem classified/51 "a")
                  (then
                   (let$ input/37 = (External.Assoc.find classified/51 "a"))
                   (stack_push stack/0 "a")
                   (let$ type/49 = "string")
                   (External.classify (string) input/37 classified/53
                    (ok (decoded/14.%{"a"} <- (Data.string classified/53)))
                    (error
                     (stack_push errors/0
                      ((decode_error/0 @@ type/49) @@ input/37))))
                   (stack_drop stack/0))
                  (else (stack_push missing_keys/7 "a")))
                 (if (not (stack_is_empty missing_keys/7))
                  (then
                   (stack_push errors/0
                    ((key_error/0 @@ type/48) @@ missing_keys/7))))
                 (props/0.%{"tagged"} <- (Data.hashtbl decoded/14)))
                (else
                 (let$ decoded/13 = (hashtbl_create))
                 (decoded/13.%{"tag"} <- (Data.int 0))
                 (let$ missing_keys/6 = (stack_create))
                 (unit)
                 (if (not (stack_is_empty missing_keys/6))
                  (then
                   (stack_push errors/0
                    ((key_error/0 @@ type/48) @@ missing_keys/6))))
                 (props/0.%{"tagged"} <- (Data.hashtbl decoded/13)))))
              (error
               (stack_push errors/0 ((decode_error/0 @@ type/48) @@ input/36)))))
            (else
             (stack_push errors/0 ((decode_error/0 @@ type/48) @@ input/36)))))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/48) @@ input/36))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "tagged")))
       (if_else (External.Assoc.mem classified/0 "trim_a")
        (then
         (let$ input/38 = (External.Assoc.find classified/0 "trim_a"))
         (stack_push stack/0 "trim_a")
         (let$ type/50 = "string")
         (External.classify (string) input/38 classified/54
          (ok (props/0.%{"trim_a"} <- (Data.string classified/54)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/50) @@ input/38))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "trim_a")))
       (if_else (External.Assoc.mem classified/0 "trim_b")
        (then
         (let$ input/39 = (External.Assoc.find classified/0 "trim_b"))
         (stack_push stack/0 "trim_b")
         (let$ type/51 = "string")
         (External.classify (string) input/39 classified/55
          (ok (props/0.%{"trim_b"} <- (Data.string classified/55)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/51) @@ input/39))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "trim_b")))
       (if_else (External.Assoc.mem classified/0 "trim_c")
        (then
         (let$ input/40 = (External.Assoc.find classified/0 "trim_c"))
         (stack_push stack/0 "trim_c")
         (let$ type/52 = "string")
         (External.classify (string) input/40 classified/56
          (ok (props/0.%{"trim_c"} <- (Data.string classified/56)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/52) @@ input/40))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "trim_c")))
       (if_else (External.Assoc.mem classified/0 "trim_d")
        (then
         (let$ input/41 = (External.Assoc.find classified/0 "trim_d"))
         (stack_push stack/0 "trim_d")
         (let$ type/53 = "string")
         (External.classify (string) input/41 classified/57
          (ok (props/0.%{"trim_d"} <- (Data.string classified/57)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/53) @@ input/41))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "trim_d")))
       (if_else (External.Assoc.mem classified/0 "trim_e")
        (then
         (let$ input/42 = (External.Assoc.find classified/0 "trim_e"))
         (stack_push stack/0 "trim_e")
         (let$ type/54 = "string")
         (External.classify (string) input/42 classified/58
          (ok (props/0.%{"trim_e"} <- (Data.string classified/58)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/54) @@ input/42))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "trim_e")))
       (if_else (External.Assoc.mem classified/0 "trim_f")
        (then
         (let$ input/43 = (External.Assoc.find classified/0 "trim_f"))
         (stack_push stack/0 "trim_f")
         (let$ type/55 = "string")
         (External.classify (string) input/43 classified/59
          (ok (props/0.%{"trim_f"} <- (Data.string classified/59)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/55) @@ input/43))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "trim_f")))
       (if_else (External.Assoc.mem classified/0 "trim_g")
        (then
         (let$ input/44 = (External.Assoc.find classified/0 "trim_g"))
         (stack_push stack/0 "trim_g")
         (let$ type/56 = "string")
         (External.classify (string) input/44 classified/60
          (ok (props/0.%{"trim_g"} <- (Data.string classified/60)))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/56) @@ input/44))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "trim_g")))
       (if_else (External.Assoc.mem classified/0 "tuple")
        (then
         (let$ input/45 = (External.Assoc.find classified/0 "tuple"))
         (stack_push stack/0 "tuple")
         (let$ type/57 = "(int, float, string)")
         (External.classify (linear) input/45 classified/61
          (ok
           (if_else (equal_int (External.Linear.length classified/61) 3)
            (then
             (let$ decoded/15 = (array_init 3 (Data.int 0)))
             (External.Linear.iteri classified/61 key/5 value/5
              (stack_push stack/0 (int_to_string key/5))
              (if_else (equal_int key/5 0)
               (then
                (let$ type/60 = "int")
                (External.classify (int) value/5 classified/65
                 (ok (decoded/15.%(key/5) <- (Data.int classified/65)))
                 (error
                  (stack_push errors/0 ((decode_error/0 @@ type/60) @@ value/5)))))
               (else
                (if_else (equal_int key/5 1)
                 (then
                  (let$ type/59 = "float")
                  (External.classify (float) value/5 classified/63
                   (ok (decoded/15.%(key/5) <- (Data.float classified/63)))
                   (error
                    (External.classify (int) value/5 classified/64
                     (ok
                      (decoded/15.%(key/5) <-
                       (Data.float (int_to_float classified/64))))
                     (error
                      (stack_push errors/0
                       ((decode_error/0 @@ type/59) @@ value/5)))))))
                 (else
                  (if_else (equal_int key/5 2)
                   (then
                    (let$ type/58 = "string")
                    (External.classify (string) value/5 classified/62
                     (ok (decoded/15.%(key/5) <- (Data.string classified/62)))
                     (error
                      (stack_push errors/0
                       ((decode_error/0 @@ type/58) @@ value/5)))))
                   (else
                    (stack_push errors/0
                     ((decode_error/0 @@ type/57) @@ value/5))))))))
              (props/0.%{"tuple"} <- (Data.array decoded/15))
              (stack_drop stack/0)))
            (else
             (stack_push errors/0 ((decode_error/0 @@ type/57) @@ input/45)))))
          (error
           (stack_push errors/0 ((decode_error/0 @@ type/57) @@ input/45))))
         (stack_drop stack/0))
        (else (stack_push missing_keys/0 "tuple")))
       (if (not (stack_is_empty missing_keys/0))
        (then (stack_push errors/0 ((key_error/0 @@ type/0) @@ missing_keys/0)))))
      (error (stack_push errors/0 ((decode_error/0 @@ type/0) @@ arg/2))))
     (if_else (stack_is_empty errors/0)
      (then
       (let$ buffer/2 = (buffer_create))
       (buffer_append buffer/2 (promise "Echoes\n"))
       (buffer_append buffer/2
        (promise (acutis_escape/0 @@ (Data.to_string (props/0.%{"ech_a"})))))
       (buffer_append buffer/2 (promise " "))
       (buffer_append buffer/2
        (promise (acutis_escape/0 @@ (Data.to_string (Data.string "b")))))
       (buffer_append buffer/2 (promise " "))
       (let$ nullable/0 = (props/0.%{"ech_d"}))
       (if_else (not (Data.equal nullable/0 (Data.int 0)))
        (then
         (buffer_append buffer/2
          (promise (Data.to_string ((Data.to_array nullable/0).%(0))))))
        (else
         (let$ nullable/1 = (props/0.%{"ech_e"}))
         (if_else (not (Data.equal nullable/1 (Data.int 0)))
          (then
           (buffer_append buffer/2
            (promise (Data.to_string ((Data.to_array nullable/1).%(0))))))
          (else
           (buffer_append buffer/2
            (promise (Data.to_string (Data.string "f\"g"))))))))
       (buffer_append buffer/2 (promise "\n"))
       (buffer_append buffer/2
        (promise
         (acutis_escape/0 @@ (int_to_string (Data.to_int (props/0.%{"ech_i"}))))))
       (buffer_append buffer/2 (promise " "))
       (buffer_append buffer/2
        (promise
         (acutis_escape/0
          @@ (float_to_string (Data.to_float (props/0.%{"ech_f"}))))))
       (buffer_append buffer/2 (promise " "))
       (buffer_append buffer/2
        (promise
         (acutis_escape/0
          @@ (bool_to_string (Data.to_int (props/0.%{"ech_b"}))))))
       (buffer_append buffer/2 (promise "\n\nNumbers\n"))
       (let$ arg_match/0 = [(props/0.%{"numbers"})])
       (let$ props/1 = (hashtbl_copy props/0 ))
       (let& exit/0 = -1)
       (let$ match_arg/0 = (arg_match/0.%(0)))
       (let$ match_arg/1 = ((Data.to_hashtbl match_arg/0).%{"exp1"}))
       (if_else (Data.equal match_arg/1 (Data.float 150))
        (then
         (let$ match_arg/2 = ((Data.to_hashtbl match_arg/0).%{"exp2"}))
         (if_else (Data.equal match_arg/2 (Data.float -1000))
          (then
           (let$ match_arg/3 = ((Data.to_hashtbl match_arg/0).%{"exp3"}))
           (if_else (Data.equal match_arg/3 (Data.float 0.2))
            (then
             (let$ match_arg/4 = ((Data.to_hashtbl match_arg/0).%{"frac"}))
             (if_else (Data.equal match_arg/4 (Data.float 10.55))
              (then
               (let$ match_arg/5 = ((Data.to_hashtbl match_arg/0).%{"int"}))
               (if_else (Data.equal match_arg/5 (Data.int 1000))
                (then
                 (let$ match_arg/6 =
                  ((Data.to_hashtbl match_arg/0).%{"negfrac"}))
                 (if_else (Data.equal match_arg/6 (Data.float -12.34))
                  (then
                   (let$ match_arg/7 =
                    ((Data.to_hashtbl match_arg/0).%{"negint"}))
                   (if_else (Data.equal match_arg/7 (Data.int -999))
                    (then (unit) (exit/0 := 0))
                    (else (unit))))
                  (else (unit))))
                (else (unit))))
              (else (unit))))
            (else (unit))))
          (else (unit))))
        (else (unit)))
       (if (equal_int (deref exit/0) -1) (then (unit) (exit/0 := 1)))
       (if_else (equal_int (deref exit/0) 0) (then (unit)) (else (unit)))
       (buffer_append buffer/2 (promise "\n\nTrim"))
       (buffer_append buffer/2
        (promise (acutis_escape/0 @@ (Data.to_string (props/0.%{"trim_a"})))))
       (buffer_append buffer/2
        (promise (acutis_escape/0 @@ (Data.to_string (props/0.%{"trim_b"})))))
       (buffer_append buffer/2 (promise " "))
       (buffer_append buffer/2
        (promise (acutis_escape/0 @@ (Data.to_string (props/0.%{"trim_c"})))))
       (buffer_append buffer/2
        (promise (acutis_escape/0 @@ (Data.to_string (props/0.%{"trim_d"})))))
       (buffer_append buffer/2 (promise (Data.to_string (props/0.%{"trim_e"}))))
       (buffer_append buffer/2 (promise "\n"))
       (buffer_append buffer/2 (promise (Data.to_string (props/0.%{"trim_f"}))))
       (buffer_append buffer/2 (promise (Data.to_string (props/0.%{"trim_g"}))))
       (buffer_append buffer/2 (promise "Comments\na "))
       (buffer_append buffer/2 (promise "b"))
       (buffer_append buffer/2 (promise " c\n\nFlat match\n"))
       (let$ arg_match/1 = [(props/0.%{"match_a"})])
       (let$ props/2 = (hashtbl_copy props/0 ))
       (let& exit/1 = -1)
       (let$ match_arg/8 = (arg_match/1.%(0)))
       (if_else (Data.equal match_arg/8 (Data.int 1))
        (then (unit) (exit/1 := 0))
        (else
         (if_else (Data.equal match_arg/8 (Data.int 2))
          (then (unit) (exit/1 := 0))
          (else
           (if_else (Data.equal match_arg/8 (Data.int 3))
            (then (unit) (exit/1 := 1))
            (else (unit) (exit/1 := 2)))))))
       (if_else (equal_int (deref exit/1) 0)
        (then (unit))
        (else
         (if_else (equal_int (deref exit/1) 1)
          (then (buffer_append buffer/2 (promise " ")))
          (else (buffer_append buffer/2 (promise " "))))))
       (buffer_append buffer/2 (promise "\n\nNested match\n"))
       (let$ arg_match/2 = [(props/0.%{"match_b"})])
       (let$ props/3 = (hashtbl_copy props/0 ))
       (let& exit/2 = -1)
       (let$ match_arg/9 = (arg_match/2.%(0)))
       (props/3.%{"c"} <- match_arg/9)
       (exit/2 := 0)
       (buffer_append buffer/2 (promise "\n  "))
       (let$ arg_match/3 = [(props/3.%{"d"}), (props/3.%{"e"})])
       (let$ props/4 = (hashtbl_copy props/3 ))
       (let& exit/3 = -1)
       (let$ match_arg/10 = (arg_match/3.%(0)))
       (let$ match_arg/11 = (arg_match/3.%(1)))
       (props/4.%{"f"} <- match_arg/10)
       (props/4.%{"g"} <- match_arg/11)
       (exit/3 := 0)
       (buffer_append buffer/2 (promise " "))
       (buffer_append buffer/2
        (promise (acutis_escape/0 @@ (Data.to_string (props/4.%{"c"})))))
       (buffer_append buffer/2 (promise " "))
       (buffer_append buffer/2
        (promise (acutis_escape/0 @@ (Data.to_string (props/4.%{"f"})))))
       (buffer_append buffer/2 (promise " "))
       (buffer_append buffer/2
        (promise (acutis_escape/0 @@ (Data.to_string (props/4.%{"g"})))))
       (buffer_append buffer/2 (promise " "))
       (buffer_append buffer/2 (promise "\n"))
       (buffer_append buffer/2 (promise "\n\nMap list\n"))
       (let& index/0 = 0)
       (let& cell/0 = (props/0.%{"map_l"}))
       (while (not (Data.equal (deref cell/0) (Data.int 0)))
        ((let$ props/5 = (hashtbl_copy props/0 ))
         (let$ list/0 = (Data.to_array (deref cell/0)))
         (let$ head/0 = (list/0.%(0)))
         (let& exit/4 = -1)
         (if_else (Data.equal head/0 (Data.int 1))
          (then (unit) (exit/4 := 0))
          (else
           (if_else (Data.equal head/0 (Data.int 2))
            (then (unit) (exit/4 := 0))
            (else
             (if_else (Data.equal head/0 (Data.int 3))
              (then
               (props/5.%{"i"} <- (Data.int (deref index/0)))
               (exit/4 := 1))
              (else (unit) (exit/4 := 2)))))))
         (if_else (equal_int (deref exit/4) 0)
          (then (unit))
          (else
           (if_else (equal_int (deref exit/4) 1)
            (then
             (buffer_append buffer/2 (promise " "))
             (buffer_append buffer/2
              (promise
               (acutis_escape/0
                @@ (int_to_string (Data.to_int (props/5.%{"i"}))))))
             (buffer_append buffer/2 (promise " ")))
            (else (buffer_append buffer/2 (promise " "))))))
         (incr index/0)
         (cell/0 := (list/0.%(1)))))
       (buffer_append buffer/2 (promise "\n\nMap dict\n"))
       (let$ match_arg/12 = (props/0.%{"map_d"}))
       (hashtbl_iter (Data.to_hashtbl match_arg/12) key/6 value/6
        (let$ props/6 = (hashtbl_copy props/0 )) (let& exit/5 = -1)
        (if_else (Data.equal value/6 (Data.int 1))
         (then (unit) (exit/5 := 0))
         (else
          (if_else (Data.equal value/6 (Data.int 2))
           (then (unit) (exit/5 := 0))
           (else
            (if_else (Data.equal value/6 (Data.int 3))
             (then (props/6.%{"k"} <- (Data.string key/6)) (exit/5 := 1))
             (else (unit) (exit/5 := 2)))))))
        (if_else (equal_int (deref exit/5) 0)
         (then (unit))
         (else
          (if_else (equal_int (deref exit/5) 1)
           (then
            (buffer_append buffer/2 (promise " "))
            (buffer_append buffer/2
             (promise (acutis_escape/0 @@ (Data.to_string (props/6.%{"k"})))))
            (buffer_append buffer/2 (promise " ")))
           (else (buffer_append buffer/2 (promise "\n")))))))
       (buffer_append buffer/2 (promise "\n\nComponent with props\n"))
       (let$ blocks/0 = (array_init 3 (promise "")))
       (let$ buffer/3 = (buffer_create))
       (buffer_append buffer/3 (promise " "))
       (blocks/0.%(0) <- (buffer_contents/0 @@ buffer/3))
       (let$ buffer/4 = (buffer_create))
       (let$ arg_match/4 = [(props/0.%{"a_prop"})])
       (let$ props/7 = (hashtbl_copy props/0 ))
       (let& exit/6 = -1)
       (let$ match_arg/13 = (arg_match/4.%(0)))
       (props/7.%{"b_prop"} <- match_arg/13)
       (exit/6 := 0)
       (buffer_append buffer/4 (promise " "))
       (buffer_append buffer/4
        (promise (acutis_escape/0 @@ (Data.to_string (props/7.%{"b_prop"})))))
       (buffer_append buffer/4 (promise " "))
       (blocks/0.%(1) <- (buffer_contents/0 @@ buffer/4))
       (let$ buffer/5 = (buffer_create))
       (unit)
       (blocks/0.%(2) <- (buffer_contents/0 @@ buffer/5))
       (buffer_append buffer/2
        (bind (promise_array blocks/0)
         (lambda arg/7
          ((let$ buffer/6 = (buffer_create))
           (buffer_append buffer/6
            ((components/0.%{"Component"})
             @@ (hashtbl
                 [("a_prop", (props/0.%{"b_prop"})),
                  ("c_prop", (props/0.%{"c_prop"})),
                  ("d_prop", (props/0.%{"e_prop"})),
                  ("f_prop", (props/0.%{"f_prop"})),
                  ("g_prop", (Data.string (arg/7.%(0)))),
                  ("h_prop", (Data.string (arg/7.%(1)))),
                  ("i_prop", (Data.string (arg/7.%(2))))])))
           (return (buffer_contents/0 @@ buffer/6))))))
       (buffer_append buffer/2
        (promise "\n\nComponent with implicit children\n"))
       (let$ blocks/1 = (array_init 1 (promise "")))
       (let$ buffer/7 = (buffer_create))
       (buffer_append buffer/7 (promise " "))
       (blocks/1.%(0) <- (buffer_contents/0 @@ buffer/7))
       (buffer_append buffer/2
        (bind (promise_array blocks/1)
         (lambda arg/8
          ((let$ buffer/8 = (buffer_create))
           (buffer_append buffer/8
            ((components/0.%{"Component2"})
             @@ (hashtbl [("children", (Data.string (arg/8.%(0))))])))
           (return (buffer_contents/0 @@ buffer/8))))))
       (buffer_append buffer/2 (promise "\n\nPatterns\n\nTuple:\n"))
       (let$ arg_match/5 = [(props/0.%{"tuple"})])
       (let$ props/8 = (hashtbl_copy props/0 ))
       (let& exit/7 = -1)
       (let$ match_arg/14 = (arg_match/5.%(0)))
       (let$ match_arg/15 = ((Data.to_array match_arg/14).%(0)))
       (if_else (Data.equal match_arg/15 (Data.int 1))
        (then
         (let$ match_arg/16 = ((Data.to_array match_arg/14).%(1)))
         (if_else (Data.equal match_arg/16 (Data.float 2.5))
          (then
           (let$ match_arg/17 = ((Data.to_array match_arg/14).%(2)))
           (if_else (Data.equal match_arg/17 (Data.string "a"))
            (then (unit) (exit/7 := 0))
            (else (unit))))
          (else (unit))))
        (else (unit)))
       (if (equal_int (deref exit/7) -1) (then (unit) (exit/7 := 1)))
       (if_else (equal_int (deref exit/7) 0)
        (then (buffer_append buffer/2 (promise " ")))
        (else (buffer_append buffer/2 (promise " "))))
       (buffer_append buffer/2 (promise "\n\nList:\n"))
       (let$ arg_match/6 = [(props/0.%{"list"})])
       (let$ props/9 = (hashtbl_copy props/0 ))
       (let& exit/8 = -1)
       (let$ match_arg/18 = (arg_match/6.%(0)))
       (if_else (Data.equal match_arg/18 (Data.int 0))
        (then (unit) (exit/8 := 0))
        (else
         (let$ match_arg/19 = (arg_match/6.%(0)))
         (let$ match_arg/20 = ((Data.to_array match_arg/19).%(0)))
         (if_else (Data.equal match_arg/20 (Data.int 0))
          (then
           (let$ match_arg/28 = ((Data.to_array match_arg/19).%(1)))
           (props/9.%{"_tl"} <- match_arg/28)
           (props/9.%{"_z"} <- match_arg/20)
           (exit/8 := 2))
          (else
           (let$ match_arg/21 = ((Data.to_array match_arg/19).%(0)))
           (let$ match_arg/22 = ((Data.to_array match_arg/21).%(0)))
           (let$ match_arg/23 = ((Data.to_array match_arg/19).%(1)))
           (if_else (Data.equal match_arg/23 (Data.int 0))
            (then
             (props/9.%{"_tl"} <- match_arg/23)
             (props/9.%{"_z"} <- match_arg/21)
             (exit/8 := 2))
            (else
             (let$ match_arg/24 = ((Data.to_array match_arg/19).%(1)))
             (let$ match_arg/25 = ((Data.to_array match_arg/24).%(0)))
             (if (Data.equal match_arg/25 (Data.int 0))
              (then
               (let$ match_arg/26 = ((Data.to_array match_arg/24).%(1)))
               (if (Data.equal match_arg/26 (Data.int 0))
                (then (props/9.%{"a"} <- match_arg/22) (exit/8 := 1)))))
             (if (equal_int (deref exit/8) -1)
              (then
               (props/9.%{"_tl"} <- match_arg/24)
               (props/9.%{"_z"} <- match_arg/21)
               (exit/8 := 2)))))
           (if (equal_int (deref exit/8) -1)
            (then
             (let$ match_arg/27 = ((Data.to_array match_arg/19).%(1)))
             (props/9.%{"_tl"} <- match_arg/27)
             (props/9.%{"_z"} <- match_arg/21)
             (exit/8 := 2)))))))
       (if_else (equal_int (deref exit/8) 0)
        (then (buffer_append buffer/2 (promise "\n")))
        (else
         (if_else (equal_int (deref exit/8) 1)
          (then
           (buffer_append buffer/2 (promise " "))
           (buffer_append buffer/2
            (promise (acutis_escape/0 @@ (Data.to_string (props/9.%{"a"})))))
           (buffer_append buffer/2 (promise "\n")))
          (else (buffer_append buffer/2 (promise "\n"))))))
       (buffer_append buffer/2 (promise "\n\nRecord:\n"))
       (let$ arg_match/7 = [(props/0.%{"record"})])
       (let$ props/10 = (hashtbl_copy props/0 ))
       (let& exit/9 = -1)
       (let$ match_arg/29 = (arg_match/7.%(0)))
       (let$ match_arg/30 = ((Data.to_hashtbl match_arg/29).%{"!#%@"}))
       (let$ match_arg/31 = ((Data.to_hashtbl match_arg/29).%{"a"}))
       (props/10.%{"a"} <- match_arg/31)
       (props/10.%{"b"} <- match_arg/30)
       (exit/9 := 0)
       (if (equal_int (deref exit/9) -1) (then (unit) (exit/9 := 1)))
       (if_else (equal_int (deref exit/9) 0)
        (then
         (buffer_append buffer/2 (promise " "))
         (buffer_append buffer/2
          (promise (acutis_escape/0 @@ (Data.to_string (props/10.%{"a"})))))
         (buffer_append buffer/2 (promise " "))
         (buffer_append buffer/2
          (promise (acutis_escape/0 @@ (Data.to_string (props/10.%{"b"})))))
         (buffer_append buffer/2 (promise " ")))
        (else (buffer_append buffer/2 (promise " "))))
       (buffer_append buffer/2 (promise "\n\nEnum:\n"))
       (let$ arg_match/8 = [(props/0.%{"enums"})])
       (let$ props/11 = (hashtbl_copy props/0 ))
       (let& exit/10 = -1)
       (let$ match_arg/32 = (arg_match/8.%(0)))
       (let$ match_arg/33 = ((Data.to_array match_arg/32).%(0)))
       (if_else (Data.equal match_arg/33 (Data.string "a"))
        (then
         (let$ match_arg/34 = ((Data.to_array match_arg/32).%(1)))
         (if_else (Data.equal match_arg/34 (Data.int 1))
          (then
           (let$ match_arg/35 = ((Data.to_array match_arg/32).%(2)))
           (if_else (Data.equal match_arg/35 (Data.int 1))
            (then
             (let$ match_arg/36 = ((Data.to_array match_arg/32).%(3)))
             (if_else (Data.equal match_arg/36 (Data.int 0))
              (then (unit) (exit/10 := 0))
              (else (unit))))
            (else (unit))))
          (else (unit))))
        (else (unit)))
       (if (equal_int (deref exit/10) -1) (then (unit) (exit/10 := 1)))
       (if_else (equal_int (deref exit/10) 0)
        (then (buffer_append buffer/2 (promise " ")))
        (else (buffer_append buffer/2 (promise " "))))
       (buffer_append buffer/2 (promise "\n\nTagged union:\n"))
       (let$ arg_match/9 = [(props/0.%{"tagged"})])
       (let$ props/12 = (hashtbl_copy props/0 ))
       (let& exit/11 = -1)
       (let$ match_arg/37 = (arg_match/9.%(0)))
       (let$ match_arg/38 = ((Data.to_hashtbl match_arg/37).%{"tag"}))
       (if_else (Data.equal match_arg/38 (Data.int 0))
        (then (unit) (exit/11 := 1))
        (else
         (if_else (Data.equal match_arg/38 (Data.int 1))
          (then
           (let$ match_arg/39 = ((Data.to_hashtbl match_arg/37).%{"a"}))
           (props/12.%{"a"} <- match_arg/39)
           (exit/11 := 0))
          (else (unit)))))
       (if_else (equal_int (deref exit/11) 0)
        (then
         (buffer_append buffer/2 (promise " "))
         (buffer_append buffer/2
          (promise (acutis_escape/0 @@ (Data.to_string (props/12.%{"a"})))))
         (buffer_append buffer/2 (promise " ")))
        (else (buffer_append buffer/2 (promise "\n"))))
       (buffer_append buffer/2 (promise "\n\nDictionary:\n"))
       (let$ arg_match/10 = [(props/0.%{"dict"})])
       (let$ props/13 = (hashtbl_copy props/0 ))
       (let& exit/12 = -1)
       (let$ match_arg/40 = (arg_match/10.%(0)))
       (if (hashtbl_mem (Data.to_hashtbl match_arg/40) "a")
        (then
         (let$ match_arg/41 = ((Data.to_hashtbl match_arg/40).%{"a"}))
         (if_else (Data.equal match_arg/41 (Data.int 1))
          (then
           (if (hashtbl_mem (Data.to_hashtbl match_arg/40) "b")
            (then
             (let$ match_arg/42 = ((Data.to_hashtbl match_arg/40).%{"b"}))
             (if_else (Data.equal match_arg/42 (Data.int 2))
              (then (unit) (exit/12 := 0))
              (else (unit))))))
          (else (unit)))))
       (if (equal_int (deref exit/12) -1) (then (unit) (exit/12 := 1)))
       (if_else (equal_int (deref exit/12) 0)
        (then (buffer_append buffer/2 (promise " ")))
        (else (buffer_append buffer/2 (promise " "))))
       (buffer_append buffer/2
        (promise "\n\n! and . precedence works correctly\n"))
       (let$ arg_match/11 =
        [(Data.array
          [(Data.array
            [((Data.to_hashtbl ((Data.to_hashtbl (props/0.%{"a"})).%{"b"}))
              .%{"c"})])])])
       (let$ props/14 = (hashtbl_copy props/0 ))
       (let& exit/13 = -1)
       (let$ match_arg/43 = (arg_match/11.%(0)))
       (if_else (Data.equal match_arg/43 (Data.int 0))
        (then (unit) (exit/13 := 1))
        (else
         (let$ match_arg/44 = (arg_match/11.%(0)))
         (let$ match_arg/45 = ((Data.to_array match_arg/44).%(0)))
         (if (not (Data.equal match_arg/45 (Data.int 0)))
          (then
           (let$ match_arg/46 = ((Data.to_array match_arg/44).%(0)))
           (let$ match_arg/47 = ((Data.to_array match_arg/46).%(0)))
           (if_else (Data.equal match_arg/47 (Data.int 0))
            (then (unit) (exit/13 := 0))
            (else (unit)))))
         (if (equal_int (deref exit/13) -1) (then (unit) (exit/13 := 1)))))
       (if_else (equal_int (deref exit/13) 0) (then (unit)) (else (unit)))
       (buffer_append buffer/2
        (promise
         "\n\nOther syntax features\n\nPatterns with }} parse correctly:\n"))
       (let$ arg_match/12 = [(props/0.%{"a"})])
       (let$ props/15 = (hashtbl_copy props/0 ))
       (let& exit/14 = -1)
       (let$ match_arg/48 = (arg_match/12.%(0)))
       (let$ match_arg/49 = ((Data.to_hashtbl match_arg/48).%{"a"}))
       (let$ match_arg/50 = ((Data.to_hashtbl match_arg/49).%{"b"}))
       (props/15.%{"b"} <- match_arg/50)
       (exit/14 := 0)
       (buffer_append buffer/2 (promise " "))
       (buffer_append buffer/2
        (promise (acutis_escape/0 @@ (Data.to_string (props/15.%{"b"})))))
       (buffer_append buffer/2 (promise " "))
       (buffer_append buffer/2
        (promise "\n\nTrailing commas parse correctly:\n"))
       (let$ arg_match/13 =
        [(Data.hashtbl
          (hashtbl
           [("a",
             (Data.array
              [(Data.int 1), (Data.array [(Data.int 2), (Data.int 0)])])),
            ("b", (Data.array [(Data.int 3), (Data.int 4)])),
            ("c", (Data.hashtbl (hashtbl [("k", (Data.int 5))])))]))])
       (let$ props/16 = (hashtbl_copy props/0 ))
       (let& exit/15 = -1)
       (let$ match_arg/51 = (arg_match/13.%(0)))
       (unit)
       (exit/15 := 0)
       (buffer_append buffer/2 (promise " "))
       (buffer_append buffer/2
        (promise "\n\nStrings may contain line breaks:\n"))
       (buffer_append buffer/2
        (promise (acutis_escape/0 @@ (Data.to_string (Data.string "a\nb")))))
       (buffer_append buffer/2 (promise "\n"))
       (return (buffer_contents/0 @@ buffer/2)))
      (else (return (error (stack_concat errors/0 "\n\n"))))))))
