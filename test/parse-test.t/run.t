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
        (text no_trim " " no_trim))))))
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
    "\n\nOther syntax features\n\nTrailing commas parse correctly:\n"
    no_trim)
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
     (exits
      ((exit (id 0) (bindings ()) (nodes ()))
       (exit (id 1) (bindings ()) (nodes ()))))))
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
     (exits
      ((exit (id 0) (bindings ()) (nodes ()))
       (exit (id 1) (bindings ()) (nodes ((text " "))))
       (exit (id 2) (bindings ()) (nodes ((text " "))))))))
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
      ((exit
        (id 0)
        (bindings ("c"))
        (nodes
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
             ((exit
               (id 0)
               (bindings ("f" "g"))
               (nodes
                ((text " ")
                 (echo () fmt_string (var "c") escape)
                 (text " ")
                 (echo () fmt_string (var "f") escape)
                 (text " ")
                 (echo () fmt_string (var "g") escape)
                 (text " "))))))))
          (text "\n"))))))))
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
      ((exit (id 0) (bindings ()) (nodes ()))
       (exit
        (id 1)
        (bindings ("i"))
        (nodes ((text " ") (echo () fmt_int (var "i") escape) (text " "))))
       (exit (id 2) (bindings ()) (nodes ((text " "))))))))
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
      ((exit (id 0) (bindings ()) (nodes ()))
       (exit
        (id 1)
        (bindings ("k"))
        (nodes ((text " ") (echo () fmt_string (var "k") escape) (text " "))))
       (exit (id 2) (bindings ()) (nodes ((text "\n"))))))))
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
          ((exit
            (id 0)
            (bindings ("b_prop"))
            (nodes
             ((text " ") (echo () fmt_string (var "b_prop") escape) (text " "))))))))))
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
     (exits
      ((exit (id 0) (bindings ()) (nodes ((text " "))))
       (exit (id 1) (bindings ()) (nodes ((text " "))))))))
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
              (wildcard none))))))
         (wildcard none)))))
     (exits
      ((exit (id 0) (bindings ()) (nodes ((text "\n"))))
       (exit
        (id 1)
        (bindings ("a"))
        (nodes ((text " ") (echo () fmt_string (var "a") escape) (text "\n"))))
       (exit (id 2) (bindings ("_tl" "_z")) (nodes ((text "\n"))))))))
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
       (wildcard none)))
     (exits
      ((exit
        (id 0)
        (bindings ("a" "b"))
        (nodes
         ((text " ")
          (echo () fmt_string (var "a") escape)
          (text " ")
          (echo () fmt_string (var "b") escape)
          (text " "))))))))
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
     (exits
      ((exit (id 0) (bindings ()) (nodes ((text " "))))
       (exit (id 1) (bindings ()) (nodes ((text " "))))))))
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
      ((exit
        (id 0)
        (bindings ("a"))
        (nodes ((text " ") (echo () fmt_string (var "a") escape) (text " "))))
       (exit (id 1) (bindings ()) (nodes ((text "\n"))))))))
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
     (exits
      ((exit (id 0) (bindings ()) (nodes ((text " "))))
       (exit (id 1) (bindings ()) (nodes ((text " "))))))))
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
     (exits
      ((exit (id 0) (bindings ()) (nodes ()))
       (exit (id 1) (bindings ()) (nodes ()))))))
   (text "\n\nOther syntax features\n\nTrailing commas parse correctly:\n")
   (match
    ()
    ((assoc
      (("a" (array (1 (array (2 null)))))
       ("b" (array (3 4)))
       ("c" (assoc (("k" 5)))))))
    (matching
     (tree
      (wildcard (key 0) (ids ()) (child (end (leaf (names ()) (exit 0))))))
     (exits ((exit (id 0) (bindings ()) (nodes ((text " "))))))))
   (text "\n\nStrings may contain line breaks:\n")
   (echo () fmt_string "a\nb" escape)
   (text "\n"))

Print the runtime instructions
  $ acutis template.acutis component.acutis component2.acutis --printinst
  (let$ buffer_add_escape/0 =
   (lambda arg/0
    ((return
      (lambda arg/1
       ((iter (string_to_seq arg/1)
         (match_char arg/2
          (('&' ((buffer_add_string arg/0 "&amp;")))
           ('"' ((buffer_add_string arg/0 "&quot;")))
           ('\'' ((buffer_add_string arg/0 "&apos;")))
           ('>' ((buffer_add_string arg/0 "&gt;")))
           ('<' ((buffer_add_string arg/0 "&lt;")))
           ('/' ((buffer_add_string arg/0 "&sol;")))
           ('`' ((buffer_add_string arg/0 "&grave;")))
           ('=' ((buffer_add_string arg/0 "&equals;")))
           (_ ((buffer_add_char arg/0 arg/2))))))))))))
  (let$ buffer_add_sep/0 =
   (lambda arg/3
    ((return
      (lambda arg/4
       ((return
         (lambda arg/5
          ((if (not ((buffer_length arg/3) = 0))
            (then (buffer_add_string arg/3 arg/4)))
           (buffer_add_string arg/3 arg/5))))))))))
  (let$ stack_empty/0 = (lambda arg/6 ((unit))))
  (let$ stack_add/0 =
   (lambda arg/7
    ((return
      (lambda arg/8
       ((return
         (lambda arg/9 ((stmt (arg/8 @@ arg/9)) (return (arg/9 @@ arg/7)))))))))))
  (let$ components/0 = (hashtbl_create))
  (components/0.%{"Component"} <-
   (async_lambda arg/10
    ((let$ buf/0 = (buffer_create))
     (stmt
      ((buffer_add_escape/0 @@ buf/0) @@ (Data.to_string (arg/10.%{"a_prop"}))))
     (buffer_add_string buf/0 "\n")
     (stmt
      ((buffer_add_escape/0 @@ buf/0) @@ (Data.to_string (arg/10.%{"c_prop"}))))
     (buffer_add_string buf/0 "\n")
     (stmt
      ((buffer_add_escape/0 @@ buf/0) @@ (Data.to_string (arg/10.%{"d_prop"}))))
     (buffer_add_string buf/0 "\n")
     (stmt
      ((buffer_add_escape/0 @@ buf/0) @@ (Data.to_string (arg/10.%{"f_prop"}))))
     (buffer_add_string buf/0 "\n")
     (stmt
      ((buffer_add_escape/0 @@ buf/0) @@ (Data.to_string (arg/10.%{"g_prop"}))))
     (buffer_add_string buf/0 "\n")
     (stmt
      ((buffer_add_escape/0 @@ buf/0) @@ (Data.to_string (arg/10.%{"h_prop"}))))
     (buffer_add_string buf/0 "\n")
     (stmt
      ((buffer_add_escape/0 @@ buf/0) @@ (Data.to_string (arg/10.%{"i_prop"}))))
     (buffer_add_string buf/0 "\n")
     (return (promise (buffer_contents buf/0))))))
  (components/0.%{"Component2"} <-
   (async_lambda arg/11
    ((let$ buf/1 = (buffer_create))
     (stmt
      ((buffer_add_escape/0 @@ buf/1)
       @@ (Data.to_string (arg/11.%{"children"}))))
     (buffer_add_string buf/1 "\n")
     (return (promise (buffer_contents buf/1))))))
  (export
   (async_lambda arg/12
    ((let$ errors/0 = (buffer_create))
     (let$ error_aux/0 =
      (lambda arg/13
       ((return
         (lambda arg/14
          ((return
            (lambda arg/15
             ((return
               (lambda arg/16
                ((if (not ((buffer_length errors/0) = 0))
                  (then (buffer_add_string errors/0 "\n\n")))
                 (buffer_add_string errors/0 "File \"")
                 (buffer_add_string errors/0 "template.acutis")
                 (buffer_add_string errors/0
                  "\"\nRender error.\nThe data supplied does not match this template's interface.\n")
                 (buffer_add_string errors/0 "Path:\n<input>")
                 (stmt (arg/15 @@ ((buffer_add_sep/0 @@ errors/0) @@ " -> ")))
                 (buffer_add_string errors/0 "\nExpected type:\n")
                 (buffer_add_string errors/0 arg/16)
                 (buffer_add_string errors/0 arg/13)
                 (buffer_add_string errors/0 arg/14)))))))))))))
     (let$ decode_error/0 =
      (lambda arg/17
       ((return
         ((error_aux/0 @@ "\nReceived value:\n") @@ (External.to_string arg/17))))))
     (let$ key_error/0 =
      (lambda arg/18
       ((return
         ((error_aux/0 @@ "\nInput is missing keys:\n")
          @@ (buffer_contents arg/18))))))
     (let$ props/0 = (hashtbl_create))
     (let$ type/0 =
      "{\n  a: {b: {c: false | true}},\n  a_prop: string,\n  b_prop: string,\n  c_prop: string,\n  d: string,\n  dict: <int>,\n  e: string,\n  e_prop: string,\n  ech_a: string,\n  ech_b: false | true,\n  ech_d: ?string,\n  ech_e: ?string,\n  ech_f: float,\n  ech_i: int,\n  enums: (@\"a\" | ..., @1 | ..., false | true, false | true),\n  f_prop: string,\n  list: [?string],\n  map_d: <int>,\n  map_l: [int],\n  match_a: int,\n  match_b: string,\n  numbers:\n    {\n      exp1: float,\n      exp2: float,\n      exp3: float,\n      frac: float,\n      int: int,\n      negfrac: float,\n      negint: int\n    },\n  record: {\"!#%@\": string, a: string},\n  tagged: {@tag: false} | {@tag: true, a: string},\n  trim_a: string,\n  trim_b: string,\n  trim_c: string,\n  trim_d: string,\n  trim_e: string,\n  trim_f: string,\n  trim_g: string,\n  tuple: (int, float, string)\n}")
     (External.decode (assoc) arg/12
      (ok decoded/0
       (let$ missing_keys/0 = (buffer_create))
       (if_else (External.assoc_mem "a" decoded/0)
        (then
         (let$ input/0 = (External.assoc_find "a" decoded/0))
         (let$ stack/0 = ((stack_add/0 @@ "a") @@ stack_empty/0))
         (let$ type/1 = "{b: {c: false | true}}")
         (External.decode (assoc) input/0
          (ok decoded/1
           (let$ decoded/2 = (hashtbl_create))
           (let$ missing_keys/1 = (buffer_create))
           (if_else (External.assoc_mem "b" decoded/1)
            (then
             (let$ input/1 = (External.assoc_find "b" decoded/1))
             (let$ stack/1 = ((stack_add/0 @@ "b") @@ stack/0))
             (let$ type/2 = "{c: false | true}")
             (External.decode (assoc) input/1
              (ok decoded/3
               (let$ decoded/4 = (hashtbl_create))
               (let$ missing_keys/2 = (buffer_create))
               (if_else (External.assoc_mem "c" decoded/3)
                (then
                 (let$ input/2 = (External.assoc_find "c" decoded/3))
                 (let$ stack/2 = ((stack_add/0 @@ "c") @@ stack/1))
                 (let$ type/3 = "false | true")
                 (External.decode (bool) input/2
                  (ok decoded/5
                   (if_else decoded/5
                    (then (decoded/4.%{"c"} <- (Data.int 1)))
                    (else (decoded/4.%{"c"} <- (Data.int 0)))))
                  (error
                   (stmt (((decode_error/0 @@ input/2) @@ stack/2) @@ type/3)))))
                (else
                 (stmt (((buffer_add_sep/0 @@ missing_keys/2) @@ ", ") @@ "c"))))
               (if (not ((buffer_length missing_keys/2) = 0))
                (then
                 (stmt (((key_error/0 @@ missing_keys/2) @@ stack/1) @@ type/2))))
               (decoded/2.%{"b"} <- (Data.hashtbl decoded/4)))
              (error
               (stmt (((decode_error/0 @@ input/1) @@ stack/1) @@ type/2)))))
            (else
             (stmt (((buffer_add_sep/0 @@ missing_keys/1) @@ ", ") @@ "b"))))
           (if (not ((buffer_length missing_keys/1) = 0))
            (then
             (stmt (((key_error/0 @@ missing_keys/1) @@ stack/0) @@ type/1))))
           (props/0.%{"a"} <- (Data.hashtbl decoded/2)))
          (error (stmt (((decode_error/0 @@ input/0) @@ stack/0) @@ type/1)))))
        (else (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "a"))))
       (if_else (External.assoc_mem "a_prop" decoded/0)
        (then
         (let$ input/3 = (External.assoc_find "a_prop" decoded/0))
         (let$ stack/3 = ((stack_add/0 @@ "a_prop") @@ stack_empty/0))
         (let$ type/4 = "string")
         (External.decode (string) input/3
          (ok decoded/6 (props/0.%{"a_prop"} <- (Data.string decoded/6)))
          (error (stmt (((decode_error/0 @@ input/3) @@ stack/3) @@ type/4)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "a_prop"))))
       (if_else (External.assoc_mem "b_prop" decoded/0)
        (then
         (let$ input/4 = (External.assoc_find "b_prop" decoded/0))
         (let$ stack/4 = ((stack_add/0 @@ "b_prop") @@ stack_empty/0))
         (let$ type/5 = "string")
         (External.decode (string) input/4
          (ok decoded/7 (props/0.%{"b_prop"} <- (Data.string decoded/7)))
          (error (stmt (((decode_error/0 @@ input/4) @@ stack/4) @@ type/5)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "b_prop"))))
       (if_else (External.assoc_mem "c_prop" decoded/0)
        (then
         (let$ input/5 = (External.assoc_find "c_prop" decoded/0))
         (let$ stack/5 = ((stack_add/0 @@ "c_prop") @@ stack_empty/0))
         (let$ type/6 = "string")
         (External.decode (string) input/5
          (ok decoded/8 (props/0.%{"c_prop"} <- (Data.string decoded/8)))
          (error (stmt (((decode_error/0 @@ input/5) @@ stack/5) @@ type/6)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "c_prop"))))
       (if_else (External.assoc_mem "d" decoded/0)
        (then
         (let$ input/6 = (External.assoc_find "d" decoded/0))
         (let$ stack/6 = ((stack_add/0 @@ "d") @@ stack_empty/0))
         (let$ type/7 = "string")
         (External.decode (string) input/6
          (ok decoded/9 (props/0.%{"d"} <- (Data.string decoded/9)))
          (error (stmt (((decode_error/0 @@ input/6) @@ stack/6) @@ type/7)))))
        (else (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "d"))))
       (if_else (External.assoc_mem "dict" decoded/0)
        (then
         (let$ input/7 = (External.assoc_find "dict" decoded/0))
         (let$ stack/7 = ((stack_add/0 @@ "dict") @@ stack_empty/0))
         (let$ type/8 = "<int>")
         (External.decode (assoc) input/7
          (ok decoded/10
           (let$ decoded/11 = (hashtbl_create))
           (iter (External.assoc_to_seq decoded/10)
            (let$ stack/8 = ((stack_add/0 @@ (fst arg/19)) @@ stack/7))
            (let$ type/9 = "int")
            (External.decode (int) (snd arg/19)
             (ok decoded/12
              (decoded/11.%{(fst arg/19)} <- (Data.int decoded/12)))
             (error
              (stmt (((decode_error/0 @@ (snd arg/19)) @@ stack/8) @@ type/9))))
            (props/0.%{"dict"} <- (Data.hashtbl decoded/11))))
          (error (stmt (((decode_error/0 @@ input/7) @@ stack/7) @@ type/8)))))
        (else (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "dict"))))
       (if_else (External.assoc_mem "e" decoded/0)
        (then
         (let$ input/8 = (External.assoc_find "e" decoded/0))
         (let$ stack/9 = ((stack_add/0 @@ "e") @@ stack_empty/0))
         (let$ type/10 = "string")
         (External.decode (string) input/8
          (ok decoded/13 (props/0.%{"e"} <- (Data.string decoded/13)))
          (error (stmt (((decode_error/0 @@ input/8) @@ stack/9) @@ type/10)))))
        (else (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "e"))))
       (if_else (External.assoc_mem "e_prop" decoded/0)
        (then
         (let$ input/9 = (External.assoc_find "e_prop" decoded/0))
         (let$ stack/10 = ((stack_add/0 @@ "e_prop") @@ stack_empty/0))
         (let$ type/11 = "string")
         (External.decode (string) input/9
          (ok decoded/14 (props/0.%{"e_prop"} <- (Data.string decoded/14)))
          (error (stmt (((decode_error/0 @@ input/9) @@ stack/10) @@ type/11)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "e_prop"))))
       (if_else (External.assoc_mem "ech_a" decoded/0)
        (then
         (let$ input/10 = (External.assoc_find "ech_a" decoded/0))
         (let$ stack/11 = ((stack_add/0 @@ "ech_a") @@ stack_empty/0))
         (let$ type/12 = "string")
         (External.decode (string) input/10
          (ok decoded/15 (props/0.%{"ech_a"} <- (Data.string decoded/15)))
          (error (stmt (((decode_error/0 @@ input/10) @@ stack/11) @@ type/12)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "ech_a"))))
       (if_else (External.assoc_mem "ech_b" decoded/0)
        (then
         (let$ input/11 = (External.assoc_find "ech_b" decoded/0))
         (let$ stack/12 = ((stack_add/0 @@ "ech_b") @@ stack_empty/0))
         (let$ type/13 = "false | true")
         (External.decode (bool) input/11
          (ok decoded/16
           (if_else decoded/16
            (then (props/0.%{"ech_b"} <- (Data.int 1)))
            (else (props/0.%{"ech_b"} <- (Data.int 0)))))
          (error (stmt (((decode_error/0 @@ input/11) @@ stack/12) @@ type/13)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "ech_b"))))
       (if_else (External.assoc_mem "ech_d" decoded/0)
        (then
         (let$ input/12 = (External.assoc_find "ech_d" decoded/0))
         (let$ stack/13 = ((stack_add/0 @@ "ech_d") @@ stack_empty/0))
         (let$ type/14 = "?string")
         (External.decode (some) input/12
          (ok decoded/17
           (let$ decoded/18 = [(Data.int 0)])
           (let$ stack/14 = ((stack_add/0 @@ "<nullable>") @@ stack/13))
           (let$ type/15 = "string")
           (External.decode (string) decoded/17
            (ok decoded/19 (decoded/18.%(0) <- (Data.string decoded/19)))
            (error
             (stmt (((decode_error/0 @@ decoded/17) @@ stack/14) @@ type/15))))
           (props/0.%{"ech_d"} <- (Data.array decoded/18)))
          (error (props/0.%{"ech_d"} <- (Data.int 0)))))
        (else (props/0.%{"ech_d"} <- (Data.int 0))))
       (if_else (External.assoc_mem "ech_e" decoded/0)
        (then
         (let$ input/13 = (External.assoc_find "ech_e" decoded/0))
         (let$ stack/15 = ((stack_add/0 @@ "ech_e") @@ stack_empty/0))
         (let$ type/16 = "?string")
         (External.decode (some) input/13
          (ok decoded/20
           (let$ decoded/21 = [(Data.int 0)])
           (let$ stack/16 = ((stack_add/0 @@ "<nullable>") @@ stack/15))
           (let$ type/17 = "string")
           (External.decode (string) decoded/20
            (ok decoded/22 (decoded/21.%(0) <- (Data.string decoded/22)))
            (error
             (stmt (((decode_error/0 @@ decoded/20) @@ stack/16) @@ type/17))))
           (props/0.%{"ech_e"} <- (Data.array decoded/21)))
          (error (props/0.%{"ech_e"} <- (Data.int 0)))))
        (else (props/0.%{"ech_e"} <- (Data.int 0))))
       (if_else (External.assoc_mem "ech_f" decoded/0)
        (then
         (let$ input/14 = (External.assoc_find "ech_f" decoded/0))
         (let$ stack/17 = ((stack_add/0 @@ "ech_f") @@ stack_empty/0))
         (let$ type/18 = "float")
         (External.decode (float) input/14
          (ok decoded/23 (props/0.%{"ech_f"} <- (Data.float decoded/23)))
          (error (stmt (((decode_error/0 @@ input/14) @@ stack/17) @@ type/18)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "ech_f"))))
       (if_else (External.assoc_mem "ech_i" decoded/0)
        (then
         (let$ input/15 = (External.assoc_find "ech_i" decoded/0))
         (let$ stack/18 = ((stack_add/0 @@ "ech_i") @@ stack_empty/0))
         (let$ type/19 = "int")
         (External.decode (int) input/15
          (ok decoded/24 (props/0.%{"ech_i"} <- (Data.int decoded/24)))
          (error (stmt (((decode_error/0 @@ input/15) @@ stack/18) @@ type/19)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "ech_i"))))
       (if_else (External.assoc_mem "enums" decoded/0)
        (then
         (let$ input/16 = (External.assoc_find "enums" decoded/0))
         (let$ stack/19 = ((stack_add/0 @@ "enums") @@ stack_empty/0))
         (let$ type/20 = "(@\"a\" | ..., @1 | ..., false | true, false | true)")
         (External.decode (seq) input/16
          (ok decoded/25
           (let$ decoded/26 = (array_make 4 (Data.int 0)))
           (uncons decoded/25
            (nil (stmt (((decode_error/0 @@ input/16) @@ stack/19) @@ type/20)))
            (cons hd/0 seq/0
             (let$ stack/20 = ((stack_add/0 @@ (string_of_int 0)) @@ stack/19))
             (let$ type/21 = "@\"a\" | ...")
             (External.decode (string) hd/0
              (ok decoded/27 (decoded/26.%(0) <- (Data.string decoded/27)))
              (error (stmt (((decode_error/0 @@ hd/0) @@ stack/20) @@ type/21))))
             (uncons seq/0
              (nil
               (stmt (((decode_error/0 @@ input/16) @@ stack/19) @@ type/20)))
              (cons hd/1 seq/1
               (let$ stack/21 =
                ((stack_add/0 @@ (string_of_int 1)) @@ stack/19))
               (let$ type/22 = "@1 | ...")
               (External.decode (int) hd/1
                (ok decoded/28 (decoded/26.%(1) <- (Data.int decoded/28)))
                (error
                 (stmt (((decode_error/0 @@ hd/1) @@ stack/21) @@ type/22))))
               (uncons seq/1
                (nil
                 (stmt (((decode_error/0 @@ input/16) @@ stack/19) @@ type/20)))
                (cons hd/2 seq/2
                 (let$ stack/22 =
                  ((stack_add/0 @@ (string_of_int 2)) @@ stack/19))
                 (let$ type/23 = "false | true")
                 (External.decode (bool) hd/2
                  (ok decoded/29
                   (if_else decoded/29
                    (then (decoded/26.%(2) <- (Data.int 1)))
                    (else (decoded/26.%(2) <- (Data.int 0)))))
                  (error
                   (stmt (((decode_error/0 @@ hd/2) @@ stack/22) @@ type/23))))
                 (uncons seq/2
                  (nil
                   (stmt
                    (((decode_error/0 @@ input/16) @@ stack/19) @@ type/20)))
                  (cons hd/3 seq/3
                   (let$ stack/23 =
                    ((stack_add/0 @@ (string_of_int 3)) @@ stack/19))
                   (let$ type/24 = "false | true")
                   (External.decode (bool) hd/3
                    (ok decoded/30
                     (if_else decoded/30
                      (then (decoded/26.%(3) <- (Data.int 1)))
                      (else (decoded/26.%(3) <- (Data.int 0)))))
                    (error
                     (stmt (((decode_error/0 @@ hd/3) @@ stack/23) @@ type/24))))
                   (unit)))))))))
           (props/0.%{"enums"} <- (Data.array decoded/26)))
          (error (stmt (((decode_error/0 @@ input/16) @@ stack/19) @@ type/20)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "enums"))))
       (if_else (External.assoc_mem "f_prop" decoded/0)
        (then
         (let$ input/17 = (External.assoc_find "f_prop" decoded/0))
         (let$ stack/24 = ((stack_add/0 @@ "f_prop") @@ stack_empty/0))
         (let$ type/25 = "string")
         (External.decode (string) input/17
          (ok decoded/31 (props/0.%{"f_prop"} <- (Data.string decoded/31)))
          (error (stmt (((decode_error/0 @@ input/17) @@ stack/24) @@ type/25)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "f_prop"))))
       (if_else (External.assoc_mem "list" decoded/0)
        (then
         (let$ input/18 = (External.assoc_find "list" decoded/0))
         (let$ stack/25 = ((stack_add/0 @@ "list") @@ stack_empty/0))
         (let$ type/26 = "[?string]")
         (External.decode (seq) input/18
          (ok decoded/32
           (let& index/0 = 0)
           (let$ decoded/33 = [(Data.int 0), (Data.int 0)])
           (let& decode_dst/0 = decoded/33)
           (iter decoded/32
            (let$ decode_dst_new/0 = [(Data.int 0), (Data.int 0)])
            (let$ stack/26 =
             ((stack_add/0 @@ (string_of_int !index/0)) @@ stack/25))
            (let$ type/27 = "?string")
            (External.decode (some) arg/20
             (ok decoded/34
              (let$ decoded/35 = [(Data.int 0)])
              (let$ stack/27 = ((stack_add/0 @@ "<nullable>") @@ stack/26))
              (let$ type/28 = "string")
              (External.decode (string) decoded/34
               (ok decoded/36 (decoded/35.%(0) <- (Data.string decoded/36)))
               (error
                (stmt (((decode_error/0 @@ decoded/34) @@ stack/27) @@ type/28))))
              (decode_dst_new/0.%(0) <- (Data.array decoded/35)))
             (error (decode_dst_new/0.%(0) <- (Data.int 0))))
            (!decode_dst/0.%(1) <- (Data.array decode_dst_new/0))
            (incr index/0) (decode_dst/0 := decode_dst_new/0))
           (props/0.%{"list"} <- (decoded/33.%(1))))
          (error (stmt (((decode_error/0 @@ input/18) @@ stack/25) @@ type/26)))))
        (else (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "list"))))
       (if_else (External.assoc_mem "map_d" decoded/0)
        (then
         (let$ input/19 = (External.assoc_find "map_d" decoded/0))
         (let$ stack/28 = ((stack_add/0 @@ "map_d") @@ stack_empty/0))
         (let$ type/29 = "<int>")
         (External.decode (assoc) input/19
          (ok decoded/37
           (let$ decoded/38 = (hashtbl_create))
           (iter (External.assoc_to_seq decoded/37)
            (let$ stack/29 = ((stack_add/0 @@ (fst arg/21)) @@ stack/28))
            (let$ type/30 = "int")
            (External.decode (int) (snd arg/21)
             (ok decoded/39
              (decoded/38.%{(fst arg/21)} <- (Data.int decoded/39)))
             (error
              (stmt (((decode_error/0 @@ (snd arg/21)) @@ stack/29) @@ type/30))))
            (props/0.%{"map_d"} <- (Data.hashtbl decoded/38))))
          (error (stmt (((decode_error/0 @@ input/19) @@ stack/28) @@ type/29)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "map_d"))))
       (if_else (External.assoc_mem "map_l" decoded/0)
        (then
         (let$ input/20 = (External.assoc_find "map_l" decoded/0))
         (let$ stack/30 = ((stack_add/0 @@ "map_l") @@ stack_empty/0))
         (let$ type/31 = "[int]")
         (External.decode (seq) input/20
          (ok decoded/40
           (let& index/1 = 0)
           (let$ decoded/41 = [(Data.int 0), (Data.int 0)])
           (let& decode_dst/1 = decoded/41)
           (iter decoded/40
            (let$ decode_dst_new/1 = [(Data.int 0), (Data.int 0)])
            (let$ stack/31 =
             ((stack_add/0 @@ (string_of_int !index/1)) @@ stack/30))
            (let$ type/32 = "int")
            (External.decode (int) arg/22
             (ok decoded/42 (decode_dst_new/1.%(0) <- (Data.int decoded/42)))
             (error
              (stmt (((decode_error/0 @@ arg/22) @@ stack/31) @@ type/32))))
            (!decode_dst/1.%(1) <- (Data.array decode_dst_new/1))
            (incr index/1) (decode_dst/1 := decode_dst_new/1))
           (props/0.%{"map_l"} <- (decoded/41.%(1))))
          (error (stmt (((decode_error/0 @@ input/20) @@ stack/30) @@ type/31)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "map_l"))))
       (if_else (External.assoc_mem "match_a" decoded/0)
        (then
         (let$ input/21 = (External.assoc_find "match_a" decoded/0))
         (let$ stack/32 = ((stack_add/0 @@ "match_a") @@ stack_empty/0))
         (let$ type/33 = "int")
         (External.decode (int) input/21
          (ok decoded/43 (props/0.%{"match_a"} <- (Data.int decoded/43)))
          (error (stmt (((decode_error/0 @@ input/21) @@ stack/32) @@ type/33)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "match_a"))))
       (if_else (External.assoc_mem "match_b" decoded/0)
        (then
         (let$ input/22 = (External.assoc_find "match_b" decoded/0))
         (let$ stack/33 = ((stack_add/0 @@ "match_b") @@ stack_empty/0))
         (let$ type/34 = "string")
         (External.decode (string) input/22
          (ok decoded/44 (props/0.%{"match_b"} <- (Data.string decoded/44)))
          (error (stmt (((decode_error/0 @@ input/22) @@ stack/33) @@ type/34)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "match_b"))))
       (if_else (External.assoc_mem "numbers" decoded/0)
        (then
         (let$ input/23 = (External.assoc_find "numbers" decoded/0))
         (let$ stack/34 = ((stack_add/0 @@ "numbers") @@ stack_empty/0))
         (let$ type/35 =
          "{\n  exp1: float,\n  exp2: float,\n  exp3: float,\n  frac: float,\n  int: int,\n  negfrac: float,\n  negint: int\n}")
         (External.decode (assoc) input/23
          (ok decoded/45
           (let$ decoded/46 = (hashtbl_create))
           (let$ missing_keys/3 = (buffer_create))
           (if_else (External.assoc_mem "exp1" decoded/45)
            (then
             (let$ input/24 = (External.assoc_find "exp1" decoded/45))
             (let$ stack/35 = ((stack_add/0 @@ "exp1") @@ stack/34))
             (let$ type/36 = "float")
             (External.decode (float) input/24
              (ok decoded/47 (decoded/46.%{"exp1"} <- (Data.float decoded/47)))
              (error
               (stmt (((decode_error/0 @@ input/24) @@ stack/35) @@ type/36)))))
            (else
             (stmt (((buffer_add_sep/0 @@ missing_keys/3) @@ ", ") @@ "exp1"))))
           (if_else (External.assoc_mem "exp2" decoded/45)
            (then
             (let$ input/25 = (External.assoc_find "exp2" decoded/45))
             (let$ stack/36 = ((stack_add/0 @@ "exp2") @@ stack/34))
             (let$ type/37 = "float")
             (External.decode (float) input/25
              (ok decoded/48 (decoded/46.%{"exp2"} <- (Data.float decoded/48)))
              (error
               (stmt (((decode_error/0 @@ input/25) @@ stack/36) @@ type/37)))))
            (else
             (stmt (((buffer_add_sep/0 @@ missing_keys/3) @@ ", ") @@ "exp2"))))
           (if_else (External.assoc_mem "exp3" decoded/45)
            (then
             (let$ input/26 = (External.assoc_find "exp3" decoded/45))
             (let$ stack/37 = ((stack_add/0 @@ "exp3") @@ stack/34))
             (let$ type/38 = "float")
             (External.decode (float) input/26
              (ok decoded/49 (decoded/46.%{"exp3"} <- (Data.float decoded/49)))
              (error
               (stmt (((decode_error/0 @@ input/26) @@ stack/37) @@ type/38)))))
            (else
             (stmt (((buffer_add_sep/0 @@ missing_keys/3) @@ ", ") @@ "exp3"))))
           (if_else (External.assoc_mem "frac" decoded/45)
            (then
             (let$ input/27 = (External.assoc_find "frac" decoded/45))
             (let$ stack/38 = ((stack_add/0 @@ "frac") @@ stack/34))
             (let$ type/39 = "float")
             (External.decode (float) input/27
              (ok decoded/50 (decoded/46.%{"frac"} <- (Data.float decoded/50)))
              (error
               (stmt (((decode_error/0 @@ input/27) @@ stack/38) @@ type/39)))))
            (else
             (stmt (((buffer_add_sep/0 @@ missing_keys/3) @@ ", ") @@ "frac"))))
           (if_else (External.assoc_mem "int" decoded/45)
            (then
             (let$ input/28 = (External.assoc_find "int" decoded/45))
             (let$ stack/39 = ((stack_add/0 @@ "int") @@ stack/34))
             (let$ type/40 = "int")
             (External.decode (int) input/28
              (ok decoded/51 (decoded/46.%{"int"} <- (Data.int decoded/51)))
              (error
               (stmt (((decode_error/0 @@ input/28) @@ stack/39) @@ type/40)))))
            (else
             (stmt (((buffer_add_sep/0 @@ missing_keys/3) @@ ", ") @@ "int"))))
           (if_else (External.assoc_mem "negfrac" decoded/45)
            (then
             (let$ input/29 = (External.assoc_find "negfrac" decoded/45))
             (let$ stack/40 = ((stack_add/0 @@ "negfrac") @@ stack/34))
             (let$ type/41 = "float")
             (External.decode (float) input/29
              (ok decoded/52
               (decoded/46.%{"negfrac"} <- (Data.float decoded/52)))
              (error
               (stmt (((decode_error/0 @@ input/29) @@ stack/40) @@ type/41)))))
            (else
             (stmt
              (((buffer_add_sep/0 @@ missing_keys/3) @@ ", ") @@ "negfrac"))))
           (if_else (External.assoc_mem "negint" decoded/45)
            (then
             (let$ input/30 = (External.assoc_find "negint" decoded/45))
             (let$ stack/41 = ((stack_add/0 @@ "negint") @@ stack/34))
             (let$ type/42 = "int")
             (External.decode (int) input/30
              (ok decoded/53 (decoded/46.%{"negint"} <- (Data.int decoded/53)))
              (error
               (stmt (((decode_error/0 @@ input/30) @@ stack/41) @@ type/42)))))
            (else
             (stmt (((buffer_add_sep/0 @@ missing_keys/3) @@ ", ") @@ "negint"))))
           (if (not ((buffer_length missing_keys/3) = 0))
            (then
             (stmt (((key_error/0 @@ missing_keys/3) @@ stack/34) @@ type/35))))
           (props/0.%{"numbers"} <- (Data.hashtbl decoded/46)))
          (error (stmt (((decode_error/0 @@ input/23) @@ stack/34) @@ type/35)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "numbers"))))
       (if_else (External.assoc_mem "record" decoded/0)
        (then
         (let$ input/31 = (External.assoc_find "record" decoded/0))
         (let$ stack/42 = ((stack_add/0 @@ "record") @@ stack_empty/0))
         (let$ type/43 = "{\"!#%@\": string, a: string}")
         (External.decode (assoc) input/31
          (ok decoded/54
           (let$ decoded/55 = (hashtbl_create))
           (let$ missing_keys/4 = (buffer_create))
           (if_else (External.assoc_mem "!#%@" decoded/54)
            (then
             (let$ input/32 = (External.assoc_find "!#%@" decoded/54))
             (let$ stack/43 = ((stack_add/0 @@ "!#%@") @@ stack/42))
             (let$ type/44 = "string")
             (External.decode (string) input/32
              (ok decoded/56 (decoded/55.%{"!#%@"} <- (Data.string decoded/56)))
              (error
               (stmt (((decode_error/0 @@ input/32) @@ stack/43) @@ type/44)))))
            (else
             (stmt (((buffer_add_sep/0 @@ missing_keys/4) @@ ", ") @@ "!#%@"))))
           (if_else (External.assoc_mem "a" decoded/54)
            (then
             (let$ input/33 = (External.assoc_find "a" decoded/54))
             (let$ stack/44 = ((stack_add/0 @@ "a") @@ stack/42))
             (let$ type/45 = "string")
             (External.decode (string) input/33
              (ok decoded/57 (decoded/55.%{"a"} <- (Data.string decoded/57)))
              (error
               (stmt (((decode_error/0 @@ input/33) @@ stack/44) @@ type/45)))))
            (else
             (stmt (((buffer_add_sep/0 @@ missing_keys/4) @@ ", ") @@ "a"))))
           (if (not ((buffer_length missing_keys/4) = 0))
            (then
             (stmt (((key_error/0 @@ missing_keys/4) @@ stack/42) @@ type/43))))
           (props/0.%{"record"} <- (Data.hashtbl decoded/55)))
          (error (stmt (((decode_error/0 @@ input/31) @@ stack/42) @@ type/43)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "record"))))
       (if_else (External.assoc_mem "tagged" decoded/0)
        (then
         (let$ input/34 = (External.assoc_find "tagged" decoded/0))
         (let$ stack/45 = ((stack_add/0 @@ "tagged") @@ stack_empty/0))
         (let$ type/46 = "{@tag: false} | {@tag: true, a: string}")
         (External.decode (assoc) input/34
          (ok decoded/58
           (if_else (External.assoc_mem "tag" decoded/58)
            (then
             (External.decode (bool) (External.assoc_find "tag" decoded/58)
              (ok decoded/59
               (let$ decoded/60 = (hashtbl_create))
               (if_else (not decoded/59)
                (then
                 (decoded/60.%{"tag"} <- (Data.int 0))
                 (let$ missing_keys/6 = (buffer_create))
                 (unit)
                 (if (not ((buffer_length missing_keys/6) = 0))
                  (then
                   (stmt
                    (((key_error/0 @@ missing_keys/6) @@ stack/45) @@ type/46)))))
                (else
                 (if_else decoded/59
                  (then
                   (decoded/60.%{"tag"} <- (Data.int 1))
                   (let$ missing_keys/5 = (buffer_create))
                   (if_else (External.assoc_mem "a" decoded/58)
                    (then
                     (let$ input/35 = (External.assoc_find "a" decoded/58))
                     (let$ stack/46 = ((stack_add/0 @@ "a") @@ stack/45))
                     (let$ type/47 = "string")
                     (External.decode (string) input/35
                      (ok decoded/61
                       (decoded/60.%{"a"} <- (Data.string decoded/61)))
                      (error
                       (stmt
                        (((decode_error/0 @@ input/35) @@ stack/46) @@ type/47)))))
                    (else
                     (stmt
                      (((buffer_add_sep/0 @@ missing_keys/5) @@ ", ") @@ "a"))))
                   (if (not ((buffer_length missing_keys/5) = 0))
                    (then
                     (stmt
                      (((key_error/0 @@ missing_keys/5) @@ stack/45) @@ type/46)))))
                  (else
                   (stmt
                    (((decode_error/0 @@ input/34) @@ stack/45) @@ type/46))))))
               (props/0.%{"tagged"} <- (Data.hashtbl decoded/60)))
              (error
               (stmt (((decode_error/0 @@ input/34) @@ stack/45) @@ type/46)))))
            (else
             (stmt (((decode_error/0 @@ input/34) @@ stack/45) @@ type/46)))))
          (error (stmt (((decode_error/0 @@ input/34) @@ stack/45) @@ type/46)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "tagged"))))
       (if_else (External.assoc_mem "trim_a" decoded/0)
        (then
         (let$ input/36 = (External.assoc_find "trim_a" decoded/0))
         (let$ stack/47 = ((stack_add/0 @@ "trim_a") @@ stack_empty/0))
         (let$ type/48 = "string")
         (External.decode (string) input/36
          (ok decoded/62 (props/0.%{"trim_a"} <- (Data.string decoded/62)))
          (error (stmt (((decode_error/0 @@ input/36) @@ stack/47) @@ type/48)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "trim_a"))))
       (if_else (External.assoc_mem "trim_b" decoded/0)
        (then
         (let$ input/37 = (External.assoc_find "trim_b" decoded/0))
         (let$ stack/48 = ((stack_add/0 @@ "trim_b") @@ stack_empty/0))
         (let$ type/49 = "string")
         (External.decode (string) input/37
          (ok decoded/63 (props/0.%{"trim_b"} <- (Data.string decoded/63)))
          (error (stmt (((decode_error/0 @@ input/37) @@ stack/48) @@ type/49)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "trim_b"))))
       (if_else (External.assoc_mem "trim_c" decoded/0)
        (then
         (let$ input/38 = (External.assoc_find "trim_c" decoded/0))
         (let$ stack/49 = ((stack_add/0 @@ "trim_c") @@ stack_empty/0))
         (let$ type/50 = "string")
         (External.decode (string) input/38
          (ok decoded/64 (props/0.%{"trim_c"} <- (Data.string decoded/64)))
          (error (stmt (((decode_error/0 @@ input/38) @@ stack/49) @@ type/50)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "trim_c"))))
       (if_else (External.assoc_mem "trim_d" decoded/0)
        (then
         (let$ input/39 = (External.assoc_find "trim_d" decoded/0))
         (let$ stack/50 = ((stack_add/0 @@ "trim_d") @@ stack_empty/0))
         (let$ type/51 = "string")
         (External.decode (string) input/39
          (ok decoded/65 (props/0.%{"trim_d"} <- (Data.string decoded/65)))
          (error (stmt (((decode_error/0 @@ input/39) @@ stack/50) @@ type/51)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "trim_d"))))
       (if_else (External.assoc_mem "trim_e" decoded/0)
        (then
         (let$ input/40 = (External.assoc_find "trim_e" decoded/0))
         (let$ stack/51 = ((stack_add/0 @@ "trim_e") @@ stack_empty/0))
         (let$ type/52 = "string")
         (External.decode (string) input/40
          (ok decoded/66 (props/0.%{"trim_e"} <- (Data.string decoded/66)))
          (error (stmt (((decode_error/0 @@ input/40) @@ stack/51) @@ type/52)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "trim_e"))))
       (if_else (External.assoc_mem "trim_f" decoded/0)
        (then
         (let$ input/41 = (External.assoc_find "trim_f" decoded/0))
         (let$ stack/52 = ((stack_add/0 @@ "trim_f") @@ stack_empty/0))
         (let$ type/53 = "string")
         (External.decode (string) input/41
          (ok decoded/67 (props/0.%{"trim_f"} <- (Data.string decoded/67)))
          (error (stmt (((decode_error/0 @@ input/41) @@ stack/52) @@ type/53)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "trim_f"))))
       (if_else (External.assoc_mem "trim_g" decoded/0)
        (then
         (let$ input/42 = (External.assoc_find "trim_g" decoded/0))
         (let$ stack/53 = ((stack_add/0 @@ "trim_g") @@ stack_empty/0))
         (let$ type/54 = "string")
         (External.decode (string) input/42
          (ok decoded/68 (props/0.%{"trim_g"} <- (Data.string decoded/68)))
          (error (stmt (((decode_error/0 @@ input/42) @@ stack/53) @@ type/54)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "trim_g"))))
       (if_else (External.assoc_mem "tuple" decoded/0)
        (then
         (let$ input/43 = (External.assoc_find "tuple" decoded/0))
         (let$ stack/54 = ((stack_add/0 @@ "tuple") @@ stack_empty/0))
         (let$ type/55 = "(int, float, string)")
         (External.decode (seq) input/43
          (ok decoded/69
           (let$ decoded/70 = (array_make 3 (Data.int 0)))
           (uncons decoded/69
            (nil (stmt (((decode_error/0 @@ input/43) @@ stack/54) @@ type/55)))
            (cons hd/4 seq/4
             (let$ stack/55 = ((stack_add/0 @@ (string_of_int 0)) @@ stack/54))
             (let$ type/56 = "int")
             (External.decode (int) hd/4
              (ok decoded/71 (decoded/70.%(0) <- (Data.int decoded/71)))
              (error (stmt (((decode_error/0 @@ hd/4) @@ stack/55) @@ type/56))))
             (uncons seq/4
              (nil
               (stmt (((decode_error/0 @@ input/43) @@ stack/54) @@ type/55)))
              (cons hd/5 seq/5
               (let$ stack/56 =
                ((stack_add/0 @@ (string_of_int 1)) @@ stack/54))
               (let$ type/57 = "float")
               (External.decode (float) hd/5
                (ok decoded/72 (decoded/70.%(1) <- (Data.float decoded/72)))
                (error
                 (stmt (((decode_error/0 @@ hd/5) @@ stack/56) @@ type/57))))
               (uncons seq/5
                (nil
                 (stmt (((decode_error/0 @@ input/43) @@ stack/54) @@ type/55)))
                (cons hd/6 seq/6
                 (let$ stack/57 =
                  ((stack_add/0 @@ (string_of_int 2)) @@ stack/54))
                 (let$ type/58 = "string")
                 (External.decode (string) hd/6
                  (ok decoded/73 (decoded/70.%(2) <- (Data.string decoded/73)))
                  (error
                   (stmt (((decode_error/0 @@ hd/6) @@ stack/57) @@ type/58))))
                 (unit)))))))
           (props/0.%{"tuple"} <- (Data.array decoded/70)))
          (error (stmt (((decode_error/0 @@ input/43) @@ stack/54) @@ type/55)))))
        (else
         (stmt (((buffer_add_sep/0 @@ missing_keys/0) @@ ", ") @@ "tuple"))))
       (if (not ((buffer_length missing_keys/0) = 0))
        (then
         (stmt (((key_error/0 @@ missing_keys/0) @@ stack_empty/0) @@ type/0)))))
      (error (stmt (((decode_error/0 @@ arg/12) @@ stack_empty/0) @@ type/0))))
     (if_else ((buffer_length errors/0) = 0)
      (then
       (let$ buf/2 = (buffer_create))
       (buffer_add_string buf/2 "Echoes\n")
       (stmt
        ((buffer_add_escape/0 @@ buf/2)
         @@ (Data.to_string (props/0.%{"ech_a"}))))
       (buffer_add_string buf/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf/2) @@ (Data.to_string (Data.string "b"))))
       (buffer_add_string buf/2 " ")
       (let$ nullable/0 = (props/0.%{"ech_d"}))
       (if_else (not (Data.equal nullable/0 (Data.int 0)))
        (then
         (buffer_add_string buf/2
          (Data.to_string ((Data.to_array nullable/0).%(0)))))
        (else
         (let$ nullable/1 = (props/0.%{"ech_e"}))
         (if_else (not (Data.equal nullable/1 (Data.int 0)))
          (then
           (buffer_add_string buf/2
            (Data.to_string ((Data.to_array nullable/1).%(0)))))
          (else (buffer_add_string buf/2 (Data.to_string (Data.string "f\"g")))))))
       (buffer_add_string buf/2 "\n")
       (stmt
        ((buffer_add_escape/0 @@ buf/2)
         @@ (string_of_int (Data.to_int (props/0.%{"ech_i"})))))
       (buffer_add_string buf/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf/2)
         @@ (string_of_float (Data.to_float (props/0.%{"ech_f"})))))
       (buffer_add_string buf/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf/2)
         @@ (string_of_bool (not ((Data.to_int (props/0.%{"ech_b"})) = 0)))))
       (buffer_add_string buf/2 "\n\nNumbers\n")
       (let$ arg_match/0 = [(props/0.%{"numbers"})])
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
       (if (!exit/0 = -1) (then (unit) (exit/0 := 1)))
       (if_else (!exit/0 = 0) (then (unit)) (else (unit)))
       (buffer_add_string buf/2 "\n\nTrim")
       (stmt
        ((buffer_add_escape/0 @@ buf/2)
         @@ (Data.to_string (props/0.%{"trim_a"}))))
       (stmt
        ((buffer_add_escape/0 @@ buf/2)
         @@ (Data.to_string (props/0.%{"trim_b"}))))
       (buffer_add_string buf/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf/2)
         @@ (Data.to_string (props/0.%{"trim_c"}))))
       (stmt
        ((buffer_add_escape/0 @@ buf/2)
         @@ (Data.to_string (props/0.%{"trim_d"}))))
       (buffer_add_string buf/2 (Data.to_string (props/0.%{"trim_e"})))
       (buffer_add_string buf/2 "\n")
       (buffer_add_string buf/2 (Data.to_string (props/0.%{"trim_f"})))
       (buffer_add_string buf/2 (Data.to_string (props/0.%{"trim_g"})))
       (buffer_add_string buf/2 "Comments\na ")
       (buffer_add_string buf/2 "b")
       (buffer_add_string buf/2 " c\n\nFlat match\n")
       (let$ arg_match/1 = [(props/0.%{"match_a"})])
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
       (if_else (!exit/1 = 0)
        (then (unit))
        (else
         (if_else (!exit/1 = 1)
          (then (buffer_add_string buf/2 " "))
          (else (buffer_add_string buf/2 " ")))))
       (buffer_add_string buf/2 "\n\nNested match\n")
       (let$ arg_match/2 = [(props/0.%{"match_b"})])
       (let$ match_props/0 = (hashtbl_create))
       (let& exit/2 = -1)
       (let$ match_arg/9 = (arg_match/2.%(0)))
       (match_props/0.%{"c"} <- match_arg/9)
       (exit/2 := 0)
       (buffer_add_string buf/2 "\n  ")
       (let$ arg_match/3 = [(props/0.%{"d"}), (props/0.%{"e"})])
       (let$ match_props/1 = (hashtbl_create))
       (let& exit/3 = -1)
       (let$ match_arg/10 = (arg_match/3.%(0)))
       (let$ match_arg/11 = (arg_match/3.%(1)))
       (match_props/1.%{"f"} <- match_arg/10)
       (match_props/1.%{"g"} <- match_arg/11)
       (exit/3 := 0)
       (buffer_add_string buf/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf/2)
         @@ (Data.to_string (match_props/0.%{"c"}))))
       (buffer_add_string buf/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf/2)
         @@ (Data.to_string (match_props/1.%{"f"}))))
       (buffer_add_string buf/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf/2)
         @@ (Data.to_string (match_props/1.%{"g"}))))
       (buffer_add_string buf/2 " ")
       (buffer_add_string buf/2 "\n")
       (buffer_add_string buf/2 "\n\nMap list\n")
       (let& index/2 = 0)
       (let& cell/0 = (props/0.%{"map_l"}))
       (while (not (Data.equal !cell/0 (Data.int 0)))
        ((let$ match_props/2 = (hashtbl_create))
         (let$ list/0 = (Data.to_array !cell/0))
         (let$ head/0 = (list/0.%(0)))
         (let& exit/4 = -1)
         (if_else (Data.equal head/0 (Data.int 1))
          (then (unit) (exit/4 := 0))
          (else
           (if_else (Data.equal head/0 (Data.int 2))
            (then (unit) (exit/4 := 0))
            (else
             (if_else (Data.equal head/0 (Data.int 3))
              (then (match_props/2.%{"i"} <- (Data.int !index/2)) (exit/4 := 1))
              (else (unit) (exit/4 := 2)))))))
         (if_else (!exit/4 = 0)
          (then (unit))
          (else
           (if_else (!exit/4 = 1)
            (then
             (buffer_add_string buf/2 " ")
             (stmt
              ((buffer_add_escape/0 @@ buf/2)
               @@ (string_of_int (Data.to_int (match_props/2.%{"i"})))))
             (buffer_add_string buf/2 " "))
            (else (buffer_add_string buf/2 " ")))))
         (incr index/2)
         (cell/0 := (list/0.%(1)))))
       (buffer_add_string buf/2 "\n\nMap dict\n")
       (let$ match_arg/12 = (props/0.%{"map_d"}))
       (iter (hashtbl_to_seq (Data.to_hashtbl match_arg/12))
        (let$ match_props/3 = (hashtbl_create)) (let& exit/5 = -1)
        (if_else (Data.equal (snd arg/23) (Data.int 1))
         (then (unit) (exit/5 := 0))
         (else
          (if_else (Data.equal (snd arg/23) (Data.int 2))
           (then (unit) (exit/5 := 0))
           (else
            (if_else (Data.equal (snd arg/23) (Data.int 3))
             (then
              (match_props/3.%{"k"} <- (Data.string (fst arg/23)))
              (exit/5 := 1))
             (else (unit) (exit/5 := 2)))))))
        (if_else (!exit/5 = 0)
         (then (unit))
         (else
          (if_else (!exit/5 = 1)
           (then
            (buffer_add_string buf/2 " ")
            (stmt
             ((buffer_add_escape/0 @@ buf/2)
              @@ (Data.to_string (match_props/3.%{"k"}))))
            (buffer_add_string buf/2 " "))
           (else (buffer_add_string buf/2 "\n"))))))
       (buffer_add_string buf/2 "\n\nComponent with props\n")
       (let$ buf/3 = (buffer_create))
       (buffer_add_string buf/3 " ")
       (let$ buf/4 = (buffer_create))
       (let$ arg_match/4 = [(props/0.%{"a_prop"})])
       (let$ match_props/4 = (hashtbl_create))
       (let& exit/6 = -1)
       (let$ match_arg/13 = (arg_match/4.%(0)))
       (match_props/4.%{"b_prop"} <- match_arg/13)
       (exit/6 := 0)
       (buffer_add_string buf/4 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf/4)
         @@ (Data.to_string (match_props/4.%{"b_prop"}))))
       (buffer_add_string buf/4 " ")
       (let$ buf/5 = (buffer_create))
       (unit)
       (buffer_add_string buf/2
        (await
         ((components/0.%{"Component"})
          @@ (hashtbl
              [("a_prop", (props/0.%{"b_prop"})),
               ("c_prop", (props/0.%{"c_prop"})),
               ("d_prop", (props/0.%{"e_prop"})),
               ("f_prop", (props/0.%{"f_prop"})),
               ("g_prop", (Data.string (buffer_contents buf/3))),
               ("h_prop", (Data.string (buffer_contents buf/4))),
               ("i_prop", (Data.string (buffer_contents buf/5)))]))))
       (buffer_add_string buf/2 "\n\nComponent with implicit children\n")
       (let$ buf/6 = (buffer_create))
       (buffer_add_string buf/6 " ")
       (buffer_add_string buf/2
        (await
         ((components/0.%{"Component2"})
          @@ (hashtbl [("children", (Data.string (buffer_contents buf/6)))]))))
       (buffer_add_string buf/2 "\n\nPatterns\n\nTuple:\n")
       (let$ arg_match/5 = [(props/0.%{"tuple"})])
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
       (if (!exit/7 = -1) (then (unit) (exit/7 := 1)))
       (if_else (!exit/7 = 0)
        (then (buffer_add_string buf/2 " "))
        (else (buffer_add_string buf/2 " ")))
       (buffer_add_string buf/2 "\n\nList:\n")
       (let$ arg_match/6 = [(props/0.%{"list"})])
       (let$ match_props/5 = (hashtbl_create))
       (let& exit/8 = -1)
       (let$ match_arg/18 = (arg_match/6.%(0)))
       (if_else (Data.equal match_arg/18 (Data.int 0))
        (then (unit) (exit/8 := 0))
        (else
         (let$ match_arg/19 = (arg_match/6.%(0)))
         (let$ match_arg/20 = ((Data.to_array match_arg/19).%(0)))
         (if_else (Data.equal match_arg/20 (Data.int 0))
          (then
           (let$ match_arg/27 = ((Data.to_array match_arg/19).%(1)))
           (match_props/5.%{"_tl"} <- match_arg/27)
           (match_props/5.%{"_z"} <- match_arg/20)
           (exit/8 := 2))
          (else
           (let$ match_arg/21 = ((Data.to_array match_arg/19).%(0)))
           (let$ match_arg/22 = ((Data.to_array match_arg/21).%(0)))
           (let$ match_arg/23 = ((Data.to_array match_arg/19).%(1)))
           (if_else (Data.equal match_arg/23 (Data.int 0))
            (then
             (match_props/5.%{"_tl"} <- match_arg/23)
             (match_props/5.%{"_z"} <- match_arg/21)
             (exit/8 := 2))
            (else
             (let$ match_arg/24 = ((Data.to_array match_arg/19).%(1)))
             (let$ match_arg/25 = ((Data.to_array match_arg/24).%(0)))
             (if (Data.equal match_arg/25 (Data.int 0))
              (then
               (let$ match_arg/26 = ((Data.to_array match_arg/24).%(1)))
               (if (Data.equal match_arg/26 (Data.int 0))
                (then (match_props/5.%{"a"} <- match_arg/22) (exit/8 := 1)))))
             (if (!exit/8 = -1)
              (then
               (match_props/5.%{"_tl"} <- match_arg/24)
               (match_props/5.%{"_z"} <- match_arg/21)
               (exit/8 := 2)))))))))
       (if_else (!exit/8 = 0)
        (then (buffer_add_string buf/2 "\n"))
        (else
         (if_else (!exit/8 = 1)
          (then
           (buffer_add_string buf/2 " ")
           (stmt
            ((buffer_add_escape/0 @@ buf/2)
             @@ (Data.to_string (match_props/5.%{"a"}))))
           (buffer_add_string buf/2 "\n"))
          (else (buffer_add_string buf/2 "\n")))))
       (buffer_add_string buf/2 "\n\nRecord:\n")
       (let$ arg_match/7 = [(props/0.%{"record"})])
       (let$ match_props/6 = (hashtbl_create))
       (let& exit/9 = -1)
       (let$ match_arg/28 = (arg_match/7.%(0)))
       (let$ match_arg/29 = ((Data.to_hashtbl match_arg/28).%{"!#%@"}))
       (let$ match_arg/30 = ((Data.to_hashtbl match_arg/28).%{"a"}))
       (match_props/6.%{"a"} <- match_arg/30)
       (match_props/6.%{"b"} <- match_arg/29)
       (exit/9 := 0)
       (buffer_add_string buf/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf/2)
         @@ (Data.to_string (match_props/6.%{"a"}))))
       (buffer_add_string buf/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf/2)
         @@ (Data.to_string (match_props/6.%{"b"}))))
       (buffer_add_string buf/2 " ")
       (buffer_add_string buf/2 "\n\nEnum:\n")
       (let$ arg_match/8 = [(props/0.%{"enums"})])
       (let& exit/10 = -1)
       (let$ match_arg/31 = (arg_match/8.%(0)))
       (let$ match_arg/32 = ((Data.to_array match_arg/31).%(0)))
       (if_else (Data.equal match_arg/32 (Data.string "a"))
        (then
         (let$ match_arg/33 = ((Data.to_array match_arg/31).%(1)))
         (if_else (Data.equal match_arg/33 (Data.int 1))
          (then
           (let$ match_arg/34 = ((Data.to_array match_arg/31).%(2)))
           (if_else (Data.equal match_arg/34 (Data.int 1))
            (then
             (let$ match_arg/35 = ((Data.to_array match_arg/31).%(3)))
             (if_else (Data.equal match_arg/35 (Data.int 0))
              (then (unit) (exit/10 := 0))
              (else (unit))))
            (else (unit))))
          (else (unit))))
        (else (unit)))
       (if (!exit/10 = -1) (then (unit) (exit/10 := 1)))
       (if_else (!exit/10 = 0)
        (then (buffer_add_string buf/2 " "))
        (else (buffer_add_string buf/2 " ")))
       (buffer_add_string buf/2 "\n\nTagged union:\n")
       (let$ arg_match/9 = [(props/0.%{"tagged"})])
       (let$ match_props/7 = (hashtbl_create))
       (let& exit/11 = -1)
       (let$ match_arg/36 = (arg_match/9.%(0)))
       (let$ match_arg/37 = ((Data.to_hashtbl match_arg/36).%{"tag"}))
       (if_else (Data.equal match_arg/37 (Data.int 0))
        (then (unit) (exit/11 := 1))
        (else
         (if_else (Data.equal match_arg/37 (Data.int 1))
          (then
           (let$ match_arg/38 = ((Data.to_hashtbl match_arg/36).%{"a"}))
           (match_props/7.%{"a"} <- match_arg/38)
           (exit/11 := 0))
          (else (unit)))))
       (if_else (!exit/11 = 0)
        (then
         (buffer_add_string buf/2 " ")
         (stmt
          ((buffer_add_escape/0 @@ buf/2)
           @@ (Data.to_string (match_props/7.%{"a"}))))
         (buffer_add_string buf/2 " "))
        (else (buffer_add_string buf/2 "\n")))
       (buffer_add_string buf/2 "\n\nDictionary:\n")
       (let$ arg_match/10 = [(props/0.%{"dict"})])
       (let& exit/12 = -1)
       (let$ match_arg/39 = (arg_match/10.%(0)))
       (if (hashtbl_mem (Data.to_hashtbl match_arg/39) "a")
        (then
         (let$ match_arg/40 = ((Data.to_hashtbl match_arg/39).%{"a"}))
         (if_else (Data.equal match_arg/40 (Data.int 1))
          (then
           (if (hashtbl_mem (Data.to_hashtbl match_arg/39) "b")
            (then
             (let$ match_arg/41 = ((Data.to_hashtbl match_arg/39).%{"b"}))
             (if_else (Data.equal match_arg/41 (Data.int 2))
              (then (unit) (exit/12 := 0))
              (else (unit))))))
          (else (unit)))))
       (if (!exit/12 = -1) (then (unit) (exit/12 := 1)))
       (if_else (!exit/12 = 0)
        (then (buffer_add_string buf/2 " "))
        (else (buffer_add_string buf/2 " ")))
       (buffer_add_string buf/2 "\n\n! and . precedence works correctly\n")
       (let$ arg_match/11 =
        [(Data.array
          [(Data.array
            [((Data.to_hashtbl ((Data.to_hashtbl (props/0.%{"a"})).%{"b"}))
              .%{"c"})])])])
       (let& exit/13 = -1)
       (let$ match_arg/42 = (arg_match/11.%(0)))
       (if_else (Data.equal match_arg/42 (Data.int 0))
        (then (unit) (exit/13 := 1))
        (else
         (let$ match_arg/43 = (arg_match/11.%(0)))
         (let$ match_arg/44 = ((Data.to_array match_arg/43).%(0)))
         (if (not (Data.equal match_arg/44 (Data.int 0)))
          (then
           (let$ match_arg/45 = ((Data.to_array match_arg/43).%(0)))
           (let$ match_arg/46 = ((Data.to_array match_arg/45).%(0)))
           (if_else (Data.equal match_arg/46 (Data.int 0))
            (then (unit) (exit/13 := 0))
            (else (unit)))))
         (if (!exit/13 = -1) (then (unit) (exit/13 := 1)))))
       (if_else (!exit/13 = 0) (then (unit)) (else (unit)))
       (buffer_add_string buf/2
        "\n\nOther syntax features\n\nTrailing commas parse correctly:\n")
       (let$ arg_match/12 =
        [(Data.hashtbl
          (hashtbl
           [("a",
             (Data.array
              [(Data.int 1), (Data.array [(Data.int 2), (Data.int 0)])])),
            ("b", (Data.array [(Data.int 3), (Data.int 4)])),
            ("c", (Data.hashtbl (hashtbl [("k", (Data.int 5))])))]))])
       (let& exit/14 = -1)
       (let$ match_arg/47 = (arg_match/12.%(0)))
       (unit)
       (exit/14 := 0)
       (buffer_add_string buf/2 " ")
       (buffer_add_string buf/2 "\n\nStrings may contain line breaks:\n")
       (stmt
        ((buffer_add_escape/0 @@ buf/2)
         @@ (Data.to_string (Data.string "a\nb"))))
       (buffer_add_string buf/2 "\n")
       (return (promise (buffer_contents buf/2))))
      (else (return (error (buffer_contents errors/0))))))))
