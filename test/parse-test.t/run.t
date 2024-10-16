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
       ((string_iter arg/1
         (match_char char/0
          (('&' ((buffer_add_string arg/0 "&amp;")))
           ('"' ((buffer_add_string arg/0 "&quot;")))
           ('\'' ((buffer_add_string arg/0 "&apos;")))
           ('>' ((buffer_add_string arg/0 "&gt;")))
           ('<' ((buffer_add_string arg/0 "&lt;")))
           ('/' ((buffer_add_string arg/0 "&sol;")))
           ('`' ((buffer_add_string arg/0 "&grave;")))
           ('=' ((buffer_add_string arg/0 "&equals;")))
           (_ ((buffer_add_char arg/0 char/0))))))))))))
  (let$ components/0 = (hashtbl_create))
  (components/0.%{"Component"} <-
   (lambda arg/2
    ((let$ buf_sync/0 = (buffer_create))
     (let& buf_async/0 = (promise (buffer_create)))
     (stmt
      ((buffer_add_escape/0 @@ buf_sync/0)
       @@ (Data.to_string (arg/2.%{"a_prop"}))))
     (buffer_add_string buf_sync/0 "\n")
     (stmt
      ((buffer_add_escape/0 @@ buf_sync/0)
       @@ (Data.to_string (arg/2.%{"c_prop"}))))
     (buffer_add_string buf_sync/0 "\n")
     (stmt
      ((buffer_add_escape/0 @@ buf_sync/0)
       @@ (Data.to_string (arg/2.%{"d_prop"}))))
     (buffer_add_string buf_sync/0 "\n")
     (stmt
      ((buffer_add_escape/0 @@ buf_sync/0)
       @@ (Data.to_string (arg/2.%{"f_prop"}))))
     (buffer_add_string buf_sync/0 "\n")
     (stmt
      ((buffer_add_escape/0 @@ buf_sync/0)
       @@ (Data.to_string (arg/2.%{"g_prop"}))))
     (buffer_add_string buf_sync/0 "\n")
     (stmt
      ((buffer_add_escape/0 @@ buf_sync/0)
       @@ (Data.to_string (arg/2.%{"h_prop"}))))
     (buffer_add_string buf_sync/0 "\n")
     (stmt
      ((buffer_add_escape/0 @@ buf_sync/0)
       @@ (Data.to_string (arg/2.%{"i_prop"}))))
     (buffer_add_string buf_sync/0 "\n")
     (return
      (bind !buf_async/0
       (lambda arg/3
        ((buffer_add_buffer arg/3 buf_sync/0)
         (return (promise (buffer_contents arg/3))))))))))
  (components/0.%{"Component2"} <-
   (lambda arg/4
    ((let$ buf_sync/1 = (buffer_create))
     (let& buf_async/1 = (promise (buffer_create)))
     (stmt
      ((buffer_add_escape/0 @@ buf_sync/1)
       @@ (Data.to_string (arg/4.%{"children"}))))
     (buffer_add_string buf_sync/1 "\n")
     (return
      (bind !buf_async/1
       (lambda arg/5
        ((buffer_add_buffer arg/5 buf_sync/1)
         (return (promise (buffer_contents arg/5))))))))))
  (export
   (lambda arg/6
    ((let$ errors/0 = (buffer_create))
     (let$ decode_error/0 =
      (lambda arg/7
       ((return
         (lambda arg/8
          ((return
            (lambda arg/9
             ((if (not ((buffer_length errors/0) = 0))
               (then (buffer_add_string errors/0 "\n\n")))
              (buffer_add_string errors/0 "File \"")
              (buffer_add_string errors/0 "template.acutis")
              (buffer_add_string errors/0
               "\"\nRender error.\nThe data supplied does not match this template's interface.\n")
              (buffer_add_string errors/0 "Path:\n")
              (let$ tuple/0 = (Data.to_array arg/7))
              (buffer_add_string errors/0 (Data.to_string (tuple/0.%(0))))
              (let& stack/0 = (tuple/0.%(1)))
              (while (not (Data.equal !stack/0 (Data.int 0)))
               ((let$ tuple/1 = (Data.to_array !stack/0))
                (buffer_add_string errors/0 " <- ")
                (buffer_add_string errors/0 (Data.to_string (tuple/1.%(0))))
                (stack/0 := (tuple/1.%(1)))))
              (buffer_add_string errors/0 "\nExpected type:\n")
              (buffer_add_string errors/0 arg/8)
              (buffer_add_string errors/0 "\nReceived value:\n")
              (buffer_add_string errors/0 (External.to_string arg/9)))))))))))
     (let$ key_error/0 =
      (lambda arg/10
       ((return
         (lambda arg/11
          ((return
            (lambda arg/12
             ((if (not ((buffer_length errors/0) = 0))
               (then (buffer_add_string errors/0 "\n\n")))
              (buffer_add_string errors/0 "File: ")
              (buffer_add_string errors/0 "template.acutis")
              (buffer_add_string errors/0
               "\nRender error.\nThe data supplied does not match this template's interface.\n")
              (buffer_add_string errors/0 "Path:\n")
              (let$ tuple/2 = (Data.to_array arg/10))
              (buffer_add_string errors/0 (Data.to_string (tuple/2.%(0))))
              (let& stack/1 = (tuple/2.%(1)))
              (while (not (Data.equal !stack/1 (Data.int 0)))
               ((let$ tuple/3 = (Data.to_array !stack/1))
                (buffer_add_string errors/0 " <- ")
                (buffer_add_string errors/0 (Data.to_string (tuple/3.%(0))))
                (stack/1 := (tuple/3.%(1)))))
              (buffer_add_string errors/0 "\nExpected type:\n")
              (buffer_add_string errors/0 arg/11)
              (buffer_add_string errors/0 "\nInput is missing keys:\n")
              (let$ tuple/4 = (Data.to_array arg/12))
              (buffer_add_string errors/0 (Data.to_string (tuple/4.%(0))))
              (let& stack/2 = (tuple/4.%(1)))
              (while (not (Data.equal !stack/2 (Data.int 0)))
               ((let$ tuple/5 = (Data.to_array !stack/2))
                (buffer_add_string errors/0 ", ")
                (buffer_add_string errors/0 (Data.to_string (tuple/5.%(0))))
                (stack/2 := (tuple/5.%(1))))))))))))))
     (let$ props/0 = (hashtbl_create))
     (let$ stack/3 = (Data.array [(Data.string "<input>"), (Data.int 0)]))
     (let$ type/0 =
      "{\n  a: {b: {c: false | true}},\n  a_prop: string,\n  b_prop: string,\n  c_prop: string,\n  d: string,\n  dict: <int>,\n  e: string,\n  e_prop: string,\n  ech_a: string,\n  ech_b: false | true,\n  ech_d: ?string,\n  ech_e: ?string,\n  ech_f: float,\n  ech_i: int,\n  enums: (@\"a\" | ..., @1 | ..., false | true, false | true),\n  f_prop: string,\n  list: [?string],\n  map_d: <int>,\n  map_l: [int],\n  match_a: int,\n  match_b: string,\n  numbers:\n    {\n      exp1: float,\n      exp2: float,\n      exp3: float,\n      frac: float,\n      int: int,\n      negfrac: float,\n      negint: int\n    },\n  record: {\"!#%@\": string, a: string},\n  tagged: {@tag: false} | {@tag: true, a: string},\n  trim_a: string,\n  trim_b: string,\n  trim_c: string,\n  trim_d: string,\n  trim_e: string,\n  trim_f: string,\n  trim_g: string,\n  tuple: (int, float, string)\n}")
     (External.classify (assoc) arg/6
      (ok classified/0
       (let& missing_keys/0 = (Data.int 0))
       (if_else (External.assoc_mem "a" classified/0)
        (then
         (let$ input/0 = (External.assoc_find "a" classified/0))
         (let$ stack/4 = (Data.array [(Data.string "a"), stack/3]))
         (let$ type/1 = "{b: {c: false | true}}")
         (External.classify (assoc) input/0
          (ok classified/1
           (let$ decoded/0 = (hashtbl_create))
           (let& missing_keys/1 = (Data.int 0))
           (if_else (External.assoc_mem "b" classified/1)
            (then
             (let$ input/1 = (External.assoc_find "b" classified/1))
             (let$ stack/5 = (Data.array [(Data.string "b"), stack/4]))
             (let$ type/2 = "{c: false | true}")
             (External.classify (assoc) input/1
              (ok classified/2
               (let$ decoded/1 = (hashtbl_create))
               (let& missing_keys/2 = (Data.int 0))
               (if_else (External.assoc_mem "c" classified/2)
                (then
                 (let$ input/2 = (External.assoc_find "c" classified/2))
                 (let$ stack/6 = (Data.array [(Data.string "c"), stack/5]))
                 (let$ type/3 = "false | true")
                 (External.classify (bool) input/2
                  (ok classified/3
                   (if_else classified/3
                    (then (decoded/1.%{"c"} <- (Data.int 1)))
                    (else (decoded/1.%{"c"} <- (Data.int 0)))))
                  (error
                   (stmt (((decode_error/0 @@ stack/6) @@ type/3) @@ input/2)))))
                (else
                 (missing_keys/2 :=
                  (Data.array [(Data.string "c"), !missing_keys/2]))))
               (if (not (Data.equal !missing_keys/2 (Data.int 0)))
                (then
                 (stmt
                  (((key_error/0 @@ stack/5) @@ type/2) @@ !missing_keys/2))))
               (decoded/0.%{"b"} <- (Data.hashtbl decoded/1)))
              (error
               (stmt (((decode_error/0 @@ stack/5) @@ type/2) @@ input/1)))))
            (else
             (missing_keys/1 :=
              (Data.array [(Data.string "b"), !missing_keys/1]))))
           (if (not (Data.equal !missing_keys/1 (Data.int 0)))
            (then
             (stmt (((key_error/0 @@ stack/4) @@ type/1) @@ !missing_keys/1))))
           (props/0.%{"a"} <- (Data.hashtbl decoded/0)))
          (error (stmt (((decode_error/0 @@ stack/4) @@ type/1) @@ input/0)))))
        (else
         (missing_keys/0 := (Data.array [(Data.string "a"), !missing_keys/0]))))
       (if_else (External.assoc_mem "a_prop" classified/0)
        (then
         (let$ input/3 = (External.assoc_find "a_prop" classified/0))
         (let$ stack/7 = (Data.array [(Data.string "a_prop"), stack/3]))
         (let$ type/4 = "string")
         (External.classify (string) input/3
          (ok classified/4 (props/0.%{"a_prop"} <- (Data.string classified/4)))
          (error (stmt (((decode_error/0 @@ stack/7) @@ type/4) @@ input/3)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "a_prop"), !missing_keys/0]))))
       (if_else (External.assoc_mem "b_prop" classified/0)
        (then
         (let$ input/4 = (External.assoc_find "b_prop" classified/0))
         (let$ stack/8 = (Data.array [(Data.string "b_prop"), stack/3]))
         (let$ type/5 = "string")
         (External.classify (string) input/4
          (ok classified/5 (props/0.%{"b_prop"} <- (Data.string classified/5)))
          (error (stmt (((decode_error/0 @@ stack/8) @@ type/5) @@ input/4)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "b_prop"), !missing_keys/0]))))
       (if_else (External.assoc_mem "c_prop" classified/0)
        (then
         (let$ input/5 = (External.assoc_find "c_prop" classified/0))
         (let$ stack/9 = (Data.array [(Data.string "c_prop"), stack/3]))
         (let$ type/6 = "string")
         (External.classify (string) input/5
          (ok classified/6 (props/0.%{"c_prop"} <- (Data.string classified/6)))
          (error (stmt (((decode_error/0 @@ stack/9) @@ type/6) @@ input/5)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "c_prop"), !missing_keys/0]))))
       (if_else (External.assoc_mem "d" classified/0)
        (then
         (let$ input/6 = (External.assoc_find "d" classified/0))
         (let$ stack/10 = (Data.array [(Data.string "d"), stack/3]))
         (let$ type/7 = "string")
         (External.classify (string) input/6
          (ok classified/7 (props/0.%{"d"} <- (Data.string classified/7)))
          (error (stmt (((decode_error/0 @@ stack/10) @@ type/7) @@ input/6)))))
        (else
         (missing_keys/0 := (Data.array [(Data.string "d"), !missing_keys/0]))))
       (if_else (External.assoc_mem "dict" classified/0)
        (then
         (let$ input/7 = (External.assoc_find "dict" classified/0))
         (let$ stack/11 = (Data.array [(Data.string "dict"), stack/3]))
         (let$ type/8 = "<int>")
         (External.classify (assoc) input/7
          (ok classified/8
           (let$ decoded/2 = (hashtbl_create))
           (External.assoc_iter classified/8 key/0 value/0
            (let$ stack/12 = (Data.array [(Data.string key/0), stack/11]))
            (let$ type/9 = "int")
            (External.classify (int) value/0
             (ok classified/9 (decoded/2.%{key/0} <- (Data.int classified/9)))
             (error
              (stmt (((decode_error/0 @@ stack/12) @@ type/9) @@ value/0))))
            (props/0.%{"dict"} <- (Data.hashtbl decoded/2))))
          (error (stmt (((decode_error/0 @@ stack/11) @@ type/8) @@ input/7)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "dict"), !missing_keys/0]))))
       (if_else (External.assoc_mem "e" classified/0)
        (then
         (let$ input/8 = (External.assoc_find "e" classified/0))
         (let$ stack/13 = (Data.array [(Data.string "e"), stack/3]))
         (let$ type/10 = "string")
         (External.classify (string) input/8
          (ok classified/10 (props/0.%{"e"} <- (Data.string classified/10)))
          (error (stmt (((decode_error/0 @@ stack/13) @@ type/10) @@ input/8)))))
        (else
         (missing_keys/0 := (Data.array [(Data.string "e"), !missing_keys/0]))))
       (if_else (External.assoc_mem "e_prop" classified/0)
        (then
         (let$ input/9 = (External.assoc_find "e_prop" classified/0))
         (let$ stack/14 = (Data.array [(Data.string "e_prop"), stack/3]))
         (let$ type/11 = "string")
         (External.classify (string) input/9
          (ok classified/11
           (props/0.%{"e_prop"} <- (Data.string classified/11)))
          (error (stmt (((decode_error/0 @@ stack/14) @@ type/11) @@ input/9)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "e_prop"), !missing_keys/0]))))
       (if_else (External.assoc_mem "ech_a" classified/0)
        (then
         (let$ input/10 = (External.assoc_find "ech_a" classified/0))
         (let$ stack/15 = (Data.array [(Data.string "ech_a"), stack/3]))
         (let$ type/12 = "string")
         (External.classify (string) input/10
          (ok classified/12 (props/0.%{"ech_a"} <- (Data.string classified/12)))
          (error (stmt (((decode_error/0 @@ stack/15) @@ type/12) @@ input/10)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "ech_a"), !missing_keys/0]))))
       (if_else (External.assoc_mem "ech_b" classified/0)
        (then
         (let$ input/11 = (External.assoc_find "ech_b" classified/0))
         (let$ stack/16 = (Data.array [(Data.string "ech_b"), stack/3]))
         (let$ type/13 = "false | true")
         (External.classify (bool) input/11
          (ok classified/13
           (if_else classified/13
            (then (props/0.%{"ech_b"} <- (Data.int 1)))
            (else (props/0.%{"ech_b"} <- (Data.int 0)))))
          (error (stmt (((decode_error/0 @@ stack/16) @@ type/13) @@ input/11)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "ech_b"), !missing_keys/0]))))
       (if_else (External.assoc_mem "ech_d" classified/0)
        (then
         (let$ input/12 = (External.assoc_find "ech_d" classified/0))
         (let$ stack/17 = (Data.array [(Data.string "ech_d"), stack/3]))
         (let$ type/14 = "?string")
         (External.classify (not_null) input/12
          (ok classified/14
           (let$ decoded/3 = [(Data.int 0)])
           (let$ stack/18 = (Data.array [(Data.string "<nullable>"), stack/17]))
           (let$ type/15 = "string")
           (External.classify (string) classified/14
            (ok classified/15 (decoded/3.%(0) <- (Data.string classified/15)))
            (error
             (stmt (((decode_error/0 @@ stack/18) @@ type/15) @@ classified/14))))
           (props/0.%{"ech_d"} <- (Data.array decoded/3)))
          (error (props/0.%{"ech_d"} <- (Data.int 0)))))
        (else (props/0.%{"ech_d"} <- (Data.int 0))))
       (if_else (External.assoc_mem "ech_e" classified/0)
        (then
         (let$ input/13 = (External.assoc_find "ech_e" classified/0))
         (let$ stack/19 = (Data.array [(Data.string "ech_e"), stack/3]))
         (let$ type/16 = "?string")
         (External.classify (not_null) input/13
          (ok classified/16
           (let$ decoded/4 = [(Data.int 0)])
           (let$ stack/20 = (Data.array [(Data.string "<nullable>"), stack/19]))
           (let$ type/17 = "string")
           (External.classify (string) classified/16
            (ok classified/17 (decoded/4.%(0) <- (Data.string classified/17)))
            (error
             (stmt (((decode_error/0 @@ stack/20) @@ type/17) @@ classified/16))))
           (props/0.%{"ech_e"} <- (Data.array decoded/4)))
          (error (props/0.%{"ech_e"} <- (Data.int 0)))))
        (else (props/0.%{"ech_e"} <- (Data.int 0))))
       (if_else (External.assoc_mem "ech_f" classified/0)
        (then
         (let$ input/14 = (External.assoc_find "ech_f" classified/0))
         (let$ stack/21 = (Data.array [(Data.string "ech_f"), stack/3]))
         (let$ type/18 = "float")
         (External.classify (float) input/14
          (ok classified/18 (props/0.%{"ech_f"} <- (Data.float classified/18)))
          (error
           (External.classify (int) input/14
            (ok classified/19
             (props/0.%{"ech_f"} <- (Data.float (float_of_int classified/19))))
            (error
             (stmt (((decode_error/0 @@ stack/21) @@ type/18) @@ input/14)))))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "ech_f"), !missing_keys/0]))))
       (if_else (External.assoc_mem "ech_i" classified/0)
        (then
         (let$ input/15 = (External.assoc_find "ech_i" classified/0))
         (let$ stack/22 = (Data.array [(Data.string "ech_i"), stack/3]))
         (let$ type/19 = "int")
         (External.classify (int) input/15
          (ok classified/20 (props/0.%{"ech_i"} <- (Data.int classified/20)))
          (error (stmt (((decode_error/0 @@ stack/22) @@ type/19) @@ input/15)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "ech_i"), !missing_keys/0]))))
       (if_else (External.assoc_mem "enums" classified/0)
        (then
         (let$ input/16 = (External.assoc_find "enums" classified/0))
         (let$ stack/23 = (Data.array [(Data.string "enums"), stack/3]))
         (let$ type/20 = "(@\"a\" | ..., @1 | ..., false | true, false | true)")
         (External.classify (linear) input/16
          (ok classified/21
           (if_else ((External.length classified/21) = 4)
            (then
             (let$ decoded/5 = (array_make 4 (Data.int 0)))
             (External.iteri classified/21 key/1 value/1
              (let$ stack/24 =
               (Data.array [(Data.string (string_of_int key/1)), stack/23]))
              (if_else (key/1 = 0)
               (then
                (let$ type/24 = "@\"a\" | ...")
                (External.classify (string) value/1
                 (ok classified/25
                  (decoded/5.%(key/1) <- (Data.string classified/25)))
                 (error
                  (stmt (((decode_error/0 @@ stack/24) @@ type/24) @@ value/1)))))
               (else
                (if_else (key/1 = 1)
                 (then
                  (let$ type/23 = "@1 | ...")
                  (External.classify (int) value/1
                   (ok classified/24
                    (decoded/5.%(key/1) <- (Data.int classified/24)))
                   (error
                    (stmt
                     (((decode_error/0 @@ stack/24) @@ type/23) @@ value/1)))))
                 (else
                  (if_else (key/1 = 2)
                   (then
                    (let$ type/22 = "false | true")
                    (External.classify (bool) value/1
                     (ok classified/23
                      (if_else classified/23
                       (then (decoded/5.%(key/1) <- (Data.int 1)))
                       (else (decoded/5.%(key/1) <- (Data.int 0)))))
                     (error
                      (stmt
                       (((decode_error/0 @@ stack/24) @@ type/22) @@ value/1)))))
                   (else
                    (if_else (key/1 = 3)
                     (then
                      (let$ type/21 = "false | true")
                      (External.classify (bool) value/1
                       (ok classified/22
                        (if_else classified/22
                         (then (decoded/5.%(key/1) <- (Data.int 1)))
                         (else (decoded/5.%(key/1) <- (Data.int 0)))))
                       (error
                        (stmt
                         (((decode_error/0 @@ stack/24) @@ type/21) @@ value/1)))))
                     (else
                      (stmt
                       (((decode_error/0 @@ stack/24) @@ type/20) @@ value/1))))))))))
              (props/0.%{"enums"} <- (Data.array decoded/5))))
            (else
             (stmt (((decode_error/0 @@ stack/23) @@ type/20) @@ input/16)))))
          (error (stmt (((decode_error/0 @@ stack/23) @@ type/20) @@ input/16)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "enums"), !missing_keys/0]))))
       (if_else (External.assoc_mem "f_prop" classified/0)
        (then
         (let$ input/17 = (External.assoc_find "f_prop" classified/0))
         (let$ stack/25 = (Data.array [(Data.string "f_prop"), stack/3]))
         (let$ type/25 = "string")
         (External.classify (string) input/17
          (ok classified/26
           (props/0.%{"f_prop"} <- (Data.string classified/26)))
          (error (stmt (((decode_error/0 @@ stack/25) @@ type/25) @@ input/17)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "f_prop"), !missing_keys/0]))))
       (if_else (External.assoc_mem "list" classified/0)
        (then
         (let$ input/18 = (External.assoc_find "list" classified/0))
         (let$ stack/26 = (Data.array [(Data.string "list"), stack/3]))
         (let$ type/26 = "[?string]")
         (External.classify (linear) input/18
          (ok classified/27
           (let$ decoded/6 = [(Data.int 0), (Data.int 0)])
           (let& decode_dst/0 = decoded/6)
           (External.iteri classified/27 key/2 value/2
            (let$ decode_dst_new/0 = [(Data.int 0), (Data.int 0)])
            (let$ stack/27 =
             (Data.array [(Data.string (string_of_int key/2)), stack/26]))
            (let$ type/27 = "?string")
            (External.classify (not_null) value/2
             (ok classified/28
              (let$ decoded/7 = [(Data.int 0)])
              (let$ stack/28 =
               (Data.array [(Data.string "<nullable>"), stack/27]))
              (let$ type/28 = "string")
              (External.classify (string) classified/28
               (ok classified/29
                (decoded/7.%(0) <- (Data.string classified/29)))
               (error
                (stmt
                 (((decode_error/0 @@ stack/28) @@ type/28) @@ classified/28))))
              (decode_dst_new/0.%(0) <- (Data.array decoded/7)))
             (error (decode_dst_new/0.%(0) <- (Data.int 0))))
            (!decode_dst/0.%(1) <- (Data.array decode_dst_new/0))
            (decode_dst/0 := decode_dst_new/0))
           (props/0.%{"list"} <- (decoded/6.%(1))))
          (error (stmt (((decode_error/0 @@ stack/26) @@ type/26) @@ input/18)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "list"), !missing_keys/0]))))
       (if_else (External.assoc_mem "map_d" classified/0)
        (then
         (let$ input/19 = (External.assoc_find "map_d" classified/0))
         (let$ stack/29 = (Data.array [(Data.string "map_d"), stack/3]))
         (let$ type/29 = "<int>")
         (External.classify (assoc) input/19
          (ok classified/30
           (let$ decoded/8 = (hashtbl_create))
           (External.assoc_iter classified/30 key/3 value/3
            (let$ stack/30 = (Data.array [(Data.string key/3), stack/29]))
            (let$ type/30 = "int")
            (External.classify (int) value/3
             (ok classified/31 (decoded/8.%{key/3} <- (Data.int classified/31)))
             (error
              (stmt (((decode_error/0 @@ stack/30) @@ type/30) @@ value/3))))
            (props/0.%{"map_d"} <- (Data.hashtbl decoded/8))))
          (error (stmt (((decode_error/0 @@ stack/29) @@ type/29) @@ input/19)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "map_d"), !missing_keys/0]))))
       (if_else (External.assoc_mem "map_l" classified/0)
        (then
         (let$ input/20 = (External.assoc_find "map_l" classified/0))
         (let$ stack/31 = (Data.array [(Data.string "map_l"), stack/3]))
         (let$ type/31 = "[int]")
         (External.classify (linear) input/20
          (ok classified/32
           (let$ decoded/9 = [(Data.int 0), (Data.int 0)])
           (let& decode_dst/1 = decoded/9)
           (External.iteri classified/32 key/4 value/4
            (let$ decode_dst_new/1 = [(Data.int 0), (Data.int 0)])
            (let$ stack/32 =
             (Data.array [(Data.string (string_of_int key/4)), stack/31]))
            (let$ type/32 = "int")
            (External.classify (int) value/4
             (ok classified/33
              (decode_dst_new/1.%(0) <- (Data.int classified/33)))
             (error
              (stmt (((decode_error/0 @@ stack/32) @@ type/32) @@ value/4))))
            (!decode_dst/1.%(1) <- (Data.array decode_dst_new/1))
            (decode_dst/1 := decode_dst_new/1))
           (props/0.%{"map_l"} <- (decoded/9.%(1))))
          (error (stmt (((decode_error/0 @@ stack/31) @@ type/31) @@ input/20)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "map_l"), !missing_keys/0]))))
       (if_else (External.assoc_mem "match_a" classified/0)
        (then
         (let$ input/21 = (External.assoc_find "match_a" classified/0))
         (let$ stack/33 = (Data.array [(Data.string "match_a"), stack/3]))
         (let$ type/33 = "int")
         (External.classify (int) input/21
          (ok classified/34 (props/0.%{"match_a"} <- (Data.int classified/34)))
          (error (stmt (((decode_error/0 @@ stack/33) @@ type/33) @@ input/21)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "match_a"), !missing_keys/0]))))
       (if_else (External.assoc_mem "match_b" classified/0)
        (then
         (let$ input/22 = (External.assoc_find "match_b" classified/0))
         (let$ stack/34 = (Data.array [(Data.string "match_b"), stack/3]))
         (let$ type/34 = "string")
         (External.classify (string) input/22
          (ok classified/35
           (props/0.%{"match_b"} <- (Data.string classified/35)))
          (error (stmt (((decode_error/0 @@ stack/34) @@ type/34) @@ input/22)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "match_b"), !missing_keys/0]))))
       (if_else (External.assoc_mem "numbers" classified/0)
        (then
         (let$ input/23 = (External.assoc_find "numbers" classified/0))
         (let$ stack/35 = (Data.array [(Data.string "numbers"), stack/3]))
         (let$ type/35 =
          "{\n  exp1: float,\n  exp2: float,\n  exp3: float,\n  frac: float,\n  int: int,\n  negfrac: float,\n  negint: int\n}")
         (External.classify (assoc) input/23
          (ok classified/36
           (let$ decoded/10 = (hashtbl_create))
           (let& missing_keys/3 = (Data.int 0))
           (if_else (External.assoc_mem "exp1" classified/36)
            (then
             (let$ input/24 = (External.assoc_find "exp1" classified/36))
             (let$ stack/36 = (Data.array [(Data.string "exp1"), stack/35]))
             (let$ type/36 = "float")
             (External.classify (float) input/24
              (ok classified/37
               (decoded/10.%{"exp1"} <- (Data.float classified/37)))
              (error
               (External.classify (int) input/24
                (ok classified/38
                 (decoded/10.%{"exp1"} <-
                  (Data.float (float_of_int classified/38))))
                (error
                 (stmt (((decode_error/0 @@ stack/36) @@ type/36) @@ input/24)))))))
            (else
             (missing_keys/3 :=
              (Data.array [(Data.string "exp1"), !missing_keys/3]))))
           (if_else (External.assoc_mem "exp2" classified/36)
            (then
             (let$ input/25 = (External.assoc_find "exp2" classified/36))
             (let$ stack/37 = (Data.array [(Data.string "exp2"), stack/35]))
             (let$ type/37 = "float")
             (External.classify (float) input/25
              (ok classified/39
               (decoded/10.%{"exp2"} <- (Data.float classified/39)))
              (error
               (External.classify (int) input/25
                (ok classified/40
                 (decoded/10.%{"exp2"} <-
                  (Data.float (float_of_int classified/40))))
                (error
                 (stmt (((decode_error/0 @@ stack/37) @@ type/37) @@ input/25)))))))
            (else
             (missing_keys/3 :=
              (Data.array [(Data.string "exp2"), !missing_keys/3]))))
           (if_else (External.assoc_mem "exp3" classified/36)
            (then
             (let$ input/26 = (External.assoc_find "exp3" classified/36))
             (let$ stack/38 = (Data.array [(Data.string "exp3"), stack/35]))
             (let$ type/38 = "float")
             (External.classify (float) input/26
              (ok classified/41
               (decoded/10.%{"exp3"} <- (Data.float classified/41)))
              (error
               (External.classify (int) input/26
                (ok classified/42
                 (decoded/10.%{"exp3"} <-
                  (Data.float (float_of_int classified/42))))
                (error
                 (stmt (((decode_error/0 @@ stack/38) @@ type/38) @@ input/26)))))))
            (else
             (missing_keys/3 :=
              (Data.array [(Data.string "exp3"), !missing_keys/3]))))
           (if_else (External.assoc_mem "frac" classified/36)
            (then
             (let$ input/27 = (External.assoc_find "frac" classified/36))
             (let$ stack/39 = (Data.array [(Data.string "frac"), stack/35]))
             (let$ type/39 = "float")
             (External.classify (float) input/27
              (ok classified/43
               (decoded/10.%{"frac"} <- (Data.float classified/43)))
              (error
               (External.classify (int) input/27
                (ok classified/44
                 (decoded/10.%{"frac"} <-
                  (Data.float (float_of_int classified/44))))
                (error
                 (stmt (((decode_error/0 @@ stack/39) @@ type/39) @@ input/27)))))))
            (else
             (missing_keys/3 :=
              (Data.array [(Data.string "frac"), !missing_keys/3]))))
           (if_else (External.assoc_mem "int" classified/36)
            (then
             (let$ input/28 = (External.assoc_find "int" classified/36))
             (let$ stack/40 = (Data.array [(Data.string "int"), stack/35]))
             (let$ type/40 = "int")
             (External.classify (int) input/28
              (ok classified/45
               (decoded/10.%{"int"} <- (Data.int classified/45)))
              (error
               (stmt (((decode_error/0 @@ stack/40) @@ type/40) @@ input/28)))))
            (else
             (missing_keys/3 :=
              (Data.array [(Data.string "int"), !missing_keys/3]))))
           (if_else (External.assoc_mem "negfrac" classified/36)
            (then
             (let$ input/29 = (External.assoc_find "negfrac" classified/36))
             (let$ stack/41 = (Data.array [(Data.string "negfrac"), stack/35]))
             (let$ type/41 = "float")
             (External.classify (float) input/29
              (ok classified/46
               (decoded/10.%{"negfrac"} <- (Data.float classified/46)))
              (error
               (External.classify (int) input/29
                (ok classified/47
                 (decoded/10.%{"negfrac"} <-
                  (Data.float (float_of_int classified/47))))
                (error
                 (stmt (((decode_error/0 @@ stack/41) @@ type/41) @@ input/29)))))))
            (else
             (missing_keys/3 :=
              (Data.array [(Data.string "negfrac"), !missing_keys/3]))))
           (if_else (External.assoc_mem "negint" classified/36)
            (then
             (let$ input/30 = (External.assoc_find "negint" classified/36))
             (let$ stack/42 = (Data.array [(Data.string "negint"), stack/35]))
             (let$ type/42 = "int")
             (External.classify (int) input/30
              (ok classified/48
               (decoded/10.%{"negint"} <- (Data.int classified/48)))
              (error
               (stmt (((decode_error/0 @@ stack/42) @@ type/42) @@ input/30)))))
            (else
             (missing_keys/3 :=
              (Data.array [(Data.string "negint"), !missing_keys/3]))))
           (if (not (Data.equal !missing_keys/3 (Data.int 0)))
            (then
             (stmt (((key_error/0 @@ stack/35) @@ type/35) @@ !missing_keys/3))))
           (props/0.%{"numbers"} <- (Data.hashtbl decoded/10)))
          (error (stmt (((decode_error/0 @@ stack/35) @@ type/35) @@ input/23)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "numbers"), !missing_keys/0]))))
       (if_else (External.assoc_mem "record" classified/0)
        (then
         (let$ input/31 = (External.assoc_find "record" classified/0))
         (let$ stack/43 = (Data.array [(Data.string "record"), stack/3]))
         (let$ type/43 = "{\"!#%@\": string, a: string}")
         (External.classify (assoc) input/31
          (ok classified/49
           (let$ decoded/11 = (hashtbl_create))
           (let& missing_keys/4 = (Data.int 0))
           (if_else (External.assoc_mem "!#%@" classified/49)
            (then
             (let$ input/32 = (External.assoc_find "!#%@" classified/49))
             (let$ stack/44 = (Data.array [(Data.string "!#%@"), stack/43]))
             (let$ type/44 = "string")
             (External.classify (string) input/32
              (ok classified/50
               (decoded/11.%{"!#%@"} <- (Data.string classified/50)))
              (error
               (stmt (((decode_error/0 @@ stack/44) @@ type/44) @@ input/32)))))
            (else
             (missing_keys/4 :=
              (Data.array [(Data.string "!#%@"), !missing_keys/4]))))
           (if_else (External.assoc_mem "a" classified/49)
            (then
             (let$ input/33 = (External.assoc_find "a" classified/49))
             (let$ stack/45 = (Data.array [(Data.string "a"), stack/43]))
             (let$ type/45 = "string")
             (External.classify (string) input/33
              (ok classified/51
               (decoded/11.%{"a"} <- (Data.string classified/51)))
              (error
               (stmt (((decode_error/0 @@ stack/45) @@ type/45) @@ input/33)))))
            (else
             (missing_keys/4 :=
              (Data.array [(Data.string "a"), !missing_keys/4]))))
           (if (not (Data.equal !missing_keys/4 (Data.int 0)))
            (then
             (stmt (((key_error/0 @@ stack/43) @@ type/43) @@ !missing_keys/4))))
           (props/0.%{"record"} <- (Data.hashtbl decoded/11)))
          (error (stmt (((decode_error/0 @@ stack/43) @@ type/43) @@ input/31)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "record"), !missing_keys/0]))))
       (if_else (External.assoc_mem "tagged" classified/0)
        (then
         (let$ input/34 = (External.assoc_find "tagged" classified/0))
         (let$ stack/46 = (Data.array [(Data.string "tagged"), stack/3]))
         (let$ type/46 = "{@tag: false} | {@tag: true, a: string}")
         (External.classify (assoc) input/34
          (ok classified/52
           (if_else (External.assoc_mem "tag" classified/52)
            (then
             (External.classify (bool)
              (External.assoc_find "tag" classified/52)
              (ok classified/53
               (if_else classified/53
                (then
                 (let$ decoded/13 = (hashtbl_create))
                 (decoded/13.%{"tag"} <- (Data.int 1))
                 (let& missing_keys/6 = (Data.int 0))
                 (if_else (External.assoc_mem "a" classified/52)
                  (then
                   (let$ input/35 = (External.assoc_find "a" classified/52))
                   (let$ stack/47 = (Data.array [(Data.string "a"), stack/46]))
                   (let$ type/47 = "string")
                   (External.classify (string) input/35
                    (ok classified/54
                     (decoded/13.%{"a"} <- (Data.string classified/54)))
                    (error
                     (stmt
                      (((decode_error/0 @@ stack/47) @@ type/47) @@ input/35)))))
                  (else
                   (missing_keys/6 :=
                    (Data.array [(Data.string "a"), !missing_keys/6]))))
                 (if (not (Data.equal !missing_keys/6 (Data.int 0)))
                  (then
                   (stmt
                    (((key_error/0 @@ stack/46) @@ type/46) @@ !missing_keys/6))))
                 (props/0.%{"tagged"} <- (Data.hashtbl decoded/13)))
                (else
                 (let$ decoded/12 = (hashtbl_create))
                 (decoded/12.%{"tag"} <- (Data.int 0))
                 (let& missing_keys/5 = (Data.int 0))
                 (unit)
                 (if (not (Data.equal !missing_keys/5 (Data.int 0)))
                  (then
                   (stmt
                    (((key_error/0 @@ stack/46) @@ type/46) @@ !missing_keys/5))))
                 (props/0.%{"tagged"} <- (Data.hashtbl decoded/12)))))
              (error
               (stmt (((decode_error/0 @@ stack/46) @@ type/46) @@ input/34)))))
            (else
             (stmt (((decode_error/0 @@ stack/46) @@ type/46) @@ input/34)))))
          (error (stmt (((decode_error/0 @@ stack/46) @@ type/46) @@ input/34)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "tagged"), !missing_keys/0]))))
       (if_else (External.assoc_mem "trim_a" classified/0)
        (then
         (let$ input/36 = (External.assoc_find "trim_a" classified/0))
         (let$ stack/48 = (Data.array [(Data.string "trim_a"), stack/3]))
         (let$ type/48 = "string")
         (External.classify (string) input/36
          (ok classified/55
           (props/0.%{"trim_a"} <- (Data.string classified/55)))
          (error (stmt (((decode_error/0 @@ stack/48) @@ type/48) @@ input/36)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "trim_a"), !missing_keys/0]))))
       (if_else (External.assoc_mem "trim_b" classified/0)
        (then
         (let$ input/37 = (External.assoc_find "trim_b" classified/0))
         (let$ stack/49 = (Data.array [(Data.string "trim_b"), stack/3]))
         (let$ type/49 = "string")
         (External.classify (string) input/37
          (ok classified/56
           (props/0.%{"trim_b"} <- (Data.string classified/56)))
          (error (stmt (((decode_error/0 @@ stack/49) @@ type/49) @@ input/37)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "trim_b"), !missing_keys/0]))))
       (if_else (External.assoc_mem "trim_c" classified/0)
        (then
         (let$ input/38 = (External.assoc_find "trim_c" classified/0))
         (let$ stack/50 = (Data.array [(Data.string "trim_c"), stack/3]))
         (let$ type/50 = "string")
         (External.classify (string) input/38
          (ok classified/57
           (props/0.%{"trim_c"} <- (Data.string classified/57)))
          (error (stmt (((decode_error/0 @@ stack/50) @@ type/50) @@ input/38)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "trim_c"), !missing_keys/0]))))
       (if_else (External.assoc_mem "trim_d" classified/0)
        (then
         (let$ input/39 = (External.assoc_find "trim_d" classified/0))
         (let$ stack/51 = (Data.array [(Data.string "trim_d"), stack/3]))
         (let$ type/51 = "string")
         (External.classify (string) input/39
          (ok classified/58
           (props/0.%{"trim_d"} <- (Data.string classified/58)))
          (error (stmt (((decode_error/0 @@ stack/51) @@ type/51) @@ input/39)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "trim_d"), !missing_keys/0]))))
       (if_else (External.assoc_mem "trim_e" classified/0)
        (then
         (let$ input/40 = (External.assoc_find "trim_e" classified/0))
         (let$ stack/52 = (Data.array [(Data.string "trim_e"), stack/3]))
         (let$ type/52 = "string")
         (External.classify (string) input/40
          (ok classified/59
           (props/0.%{"trim_e"} <- (Data.string classified/59)))
          (error (stmt (((decode_error/0 @@ stack/52) @@ type/52) @@ input/40)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "trim_e"), !missing_keys/0]))))
       (if_else (External.assoc_mem "trim_f" classified/0)
        (then
         (let$ input/41 = (External.assoc_find "trim_f" classified/0))
         (let$ stack/53 = (Data.array [(Data.string "trim_f"), stack/3]))
         (let$ type/53 = "string")
         (External.classify (string) input/41
          (ok classified/60
           (props/0.%{"trim_f"} <- (Data.string classified/60)))
          (error (stmt (((decode_error/0 @@ stack/53) @@ type/53) @@ input/41)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "trim_f"), !missing_keys/0]))))
       (if_else (External.assoc_mem "trim_g" classified/0)
        (then
         (let$ input/42 = (External.assoc_find "trim_g" classified/0))
         (let$ stack/54 = (Data.array [(Data.string "trim_g"), stack/3]))
         (let$ type/54 = "string")
         (External.classify (string) input/42
          (ok classified/61
           (props/0.%{"trim_g"} <- (Data.string classified/61)))
          (error (stmt (((decode_error/0 @@ stack/54) @@ type/54) @@ input/42)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "trim_g"), !missing_keys/0]))))
       (if_else (External.assoc_mem "tuple" classified/0)
        (then
         (let$ input/43 = (External.assoc_find "tuple" classified/0))
         (let$ stack/55 = (Data.array [(Data.string "tuple"), stack/3]))
         (let$ type/55 = "(int, float, string)")
         (External.classify (linear) input/43
          (ok classified/62
           (if_else ((External.length classified/62) = 3)
            (then
             (let$ decoded/14 = (array_make 3 (Data.int 0)))
             (External.iteri classified/62 key/5 value/5
              (let$ stack/56 =
               (Data.array [(Data.string (string_of_int key/5)), stack/55]))
              (if_else (key/5 = 0)
               (then
                (let$ type/58 = "int")
                (External.classify (int) value/5
                 (ok classified/66
                  (decoded/14.%(key/5) <- (Data.int classified/66)))
                 (error
                  (stmt (((decode_error/0 @@ stack/56) @@ type/58) @@ value/5)))))
               (else
                (if_else (key/5 = 1)
                 (then
                  (let$ type/57 = "float")
                  (External.classify (float) value/5
                   (ok classified/64
                    (decoded/14.%(key/5) <- (Data.float classified/64)))
                   (error
                    (External.classify (int) value/5
                     (ok classified/65
                      (decoded/14.%(key/5) <-
                       (Data.float (float_of_int classified/65))))
                     (error
                      (stmt
                       (((decode_error/0 @@ stack/56) @@ type/57) @@ value/5)))))))
                 (else
                  (if_else (key/5 = 2)
                   (then
                    (let$ type/56 = "string")
                    (External.classify (string) value/5
                     (ok classified/63
                      (decoded/14.%(key/5) <- (Data.string classified/63)))
                     (error
                      (stmt
                       (((decode_error/0 @@ stack/56) @@ type/56) @@ value/5)))))
                   (else
                    (stmt
                     (((decode_error/0 @@ stack/56) @@ type/55) @@ value/5))))))))
              (props/0.%{"tuple"} <- (Data.array decoded/14))))
            (else
             (stmt (((decode_error/0 @@ stack/55) @@ type/55) @@ input/43)))))
          (error (stmt (((decode_error/0 @@ stack/55) @@ type/55) @@ input/43)))))
        (else
         (missing_keys/0 :=
          (Data.array [(Data.string "tuple"), !missing_keys/0]))))
       (if (not (Data.equal !missing_keys/0 (Data.int 0)))
        (then (stmt (((key_error/0 @@ stack/3) @@ type/0) @@ !missing_keys/0)))))
      (error (stmt (((decode_error/0 @@ stack/3) @@ type/0) @@ arg/6))))
     (if_else ((buffer_length errors/0) = 0)
      (then
       (let$ buf_sync/2 = (buffer_create))
       (let& buf_async/2 = (promise (buffer_create)))
       (buffer_add_string buf_sync/2 "Echoes\n")
       (stmt
        ((buffer_add_escape/0 @@ buf_sync/2)
         @@ (Data.to_string (props/0.%{"ech_a"}))))
       (buffer_add_string buf_sync/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf_sync/2)
         @@ (Data.to_string (Data.string "b"))))
       (buffer_add_string buf_sync/2 " ")
       (let$ nullable/0 = (props/0.%{"ech_d"}))
       (if_else (not (Data.equal nullable/0 (Data.int 0)))
        (then
         (buffer_add_string buf_sync/2
          (Data.to_string ((Data.to_array nullable/0).%(0)))))
        (else
         (let$ nullable/1 = (props/0.%{"ech_e"}))
         (if_else (not (Data.equal nullable/1 (Data.int 0)))
          (then
           (buffer_add_string buf_sync/2
            (Data.to_string ((Data.to_array nullable/1).%(0)))))
          (else
           (buffer_add_string buf_sync/2 (Data.to_string (Data.string "f\"g")))))))
       (buffer_add_string buf_sync/2 "\n")
       (stmt
        ((buffer_add_escape/0 @@ buf_sync/2)
         @@ (string_of_int (Data.to_int (props/0.%{"ech_i"})))))
       (buffer_add_string buf_sync/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf_sync/2)
         @@ (string_of_float (Data.to_float (props/0.%{"ech_f"})))))
       (buffer_add_string buf_sync/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf_sync/2)
         @@ (string_of_bool (not ((Data.to_int (props/0.%{"ech_b"})) = 0)))))
       (buffer_add_string buf_sync/2 "\n\nNumbers\n")
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
       (buffer_add_string buf_sync/2 "\n\nTrim")
       (stmt
        ((buffer_add_escape/0 @@ buf_sync/2)
         @@ (Data.to_string (props/0.%{"trim_a"}))))
       (stmt
        ((buffer_add_escape/0 @@ buf_sync/2)
         @@ (Data.to_string (props/0.%{"trim_b"}))))
       (buffer_add_string buf_sync/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf_sync/2)
         @@ (Data.to_string (props/0.%{"trim_c"}))))
       (stmt
        ((buffer_add_escape/0 @@ buf_sync/2)
         @@ (Data.to_string (props/0.%{"trim_d"}))))
       (buffer_add_string buf_sync/2 (Data.to_string (props/0.%{"trim_e"})))
       (buffer_add_string buf_sync/2 "\n")
       (buffer_add_string buf_sync/2 (Data.to_string (props/0.%{"trim_f"})))
       (buffer_add_string buf_sync/2 (Data.to_string (props/0.%{"trim_g"})))
       (buffer_add_string buf_sync/2 "Comments\na ")
       (buffer_add_string buf_sync/2 "b")
       (buffer_add_string buf_sync/2 " c\n\nFlat match\n")
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
          (then (buffer_add_string buf_sync/2 " "))
          (else (buffer_add_string buf_sync/2 " ")))))
       (buffer_add_string buf_sync/2 "\n\nNested match\n")
       (let$ arg_match/2 = [(props/0.%{"match_b"})])
       (let$ match_props/0 = (hashtbl_create))
       (let& exit/2 = -1)
       (let$ match_arg/9 = (arg_match/2.%(0)))
       (match_props/0.%{"c"} <- match_arg/9)
       (exit/2 := 0)
       (buffer_add_string buf_sync/2 "\n  ")
       (let$ arg_match/3 = [(props/0.%{"d"}), (props/0.%{"e"})])
       (let$ match_props/1 = (hashtbl_create))
       (let& exit/3 = -1)
       (let$ match_arg/10 = (arg_match/3.%(0)))
       (let$ match_arg/11 = (arg_match/3.%(1)))
       (match_props/1.%{"f"} <- match_arg/10)
       (match_props/1.%{"g"} <- match_arg/11)
       (exit/3 := 0)
       (buffer_add_string buf_sync/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf_sync/2)
         @@ (Data.to_string (match_props/0.%{"c"}))))
       (buffer_add_string buf_sync/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf_sync/2)
         @@ (Data.to_string (match_props/1.%{"f"}))))
       (buffer_add_string buf_sync/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf_sync/2)
         @@ (Data.to_string (match_props/1.%{"g"}))))
       (buffer_add_string buf_sync/2 " ")
       (buffer_add_string buf_sync/2 "\n")
       (buffer_add_string buf_sync/2 "\n\nMap list\n")
       (let& index/0 = 0)
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
              (then (match_props/2.%{"i"} <- (Data.int !index/0)) (exit/4 := 1))
              (else (unit) (exit/4 := 2)))))))
         (if_else (!exit/4 = 0)
          (then (unit))
          (else
           (if_else (!exit/4 = 1)
            (then
             (buffer_add_string buf_sync/2 " ")
             (stmt
              ((buffer_add_escape/0 @@ buf_sync/2)
               @@ (string_of_int (Data.to_int (match_props/2.%{"i"})))))
             (buffer_add_string buf_sync/2 " "))
            (else (buffer_add_string buf_sync/2 " ")))))
         (incr index/0)
         (cell/0 := (list/0.%(1)))))
       (buffer_add_string buf_sync/2 "\n\nMap dict\n")
       (let$ match_arg/12 = (props/0.%{"map_d"}))
       (hashtbl_iter (Data.to_hashtbl match_arg/12) key/6 value/6
        (let$ match_props/3 = (hashtbl_create)) (let& exit/5 = -1)
        (if_else (Data.equal value/6 (Data.int 1))
         (then (unit) (exit/5 := 0))
         (else
          (if_else (Data.equal value/6 (Data.int 2))
           (then (unit) (exit/5 := 0))
           (else
            (if_else (Data.equal value/6 (Data.int 3))
             (then (match_props/3.%{"k"} <- (Data.string key/6)) (exit/5 := 1))
             (else (unit) (exit/5 := 2)))))))
        (if_else (!exit/5 = 0)
         (then (unit))
         (else
          (if_else (!exit/5 = 1)
           (then
            (buffer_add_string buf_sync/2 " ")
            (stmt
             ((buffer_add_escape/0 @@ buf_sync/2)
              @@ (Data.to_string (match_props/3.%{"k"}))))
            (buffer_add_string buf_sync/2 " "))
           (else (buffer_add_string buf_sync/2 "\n"))))))
       (buffer_add_string buf_sync/2 "\n\nComponent with props\n")
       (let$ buf_sync/3 = (buffer_create))
       (let& buf_async/3 = (promise (buffer_create)))
       (buffer_add_string buf_sync/3 " ")
       (let$ sync_contents/1 = (buffer_contents buf_sync/2))
       (buffer_clear buf_sync/2)
       (buf_async/2 :=
        (bind !buf_async/2
         (lambda arg/22
          ((return
            (bind
             (bind
              (bind !buf_async/3
               (lambda arg/21
                ((buffer_add_buffer arg/21 buf_sync/3)
                 (return (promise (buffer_contents arg/21))))))
              (lambda arg/13
               ((let$ buf_sync/4 = (buffer_create))
                (let& buf_async/4 = (promise (buffer_create)))
                (let$ arg_match/4 = [(props/0.%{"a_prop"})])
                (let$ match_props/4 = (hashtbl_create))
                (let& exit/6 = -1)
                (let$ match_arg/13 = (arg_match/4.%(0)))
                (match_props/4.%{"b_prop"} <- match_arg/13)
                (exit/6 := 0)
                (buffer_add_string buf_sync/4 " ")
                (stmt
                 ((buffer_add_escape/0 @@ buf_sync/4)
                  @@ (Data.to_string (match_props/4.%{"b_prop"}))))
                (buffer_add_string buf_sync/4 " ")
                (return
                 (bind
                  (bind !buf_async/4
                   (lambda arg/20
                    ((buffer_add_buffer arg/20 buf_sync/4)
                     (return (promise (buffer_contents arg/20))))))
                  (lambda arg/14
                   ((let$ buf_sync/5 = (buffer_create))
                    (let& buf_async/5 = (promise (buffer_create)))
                    (unit)
                    (return
                     (bind
                      (bind !buf_async/5
                       (lambda arg/19
                        ((buffer_add_buffer arg/19 buf_sync/5)
                         (return (promise (buffer_contents arg/19))))))
                      (lambda arg/15
                       ((let$ buf_sync/6 = (buffer_create))
                        (let& buf_async/6 = (promise (buffer_create)))
                        (let$ sync_contents/0 = (buffer_contents buf_sync/6))
                        (buffer_clear buf_sync/6)
                        (buf_async/6 :=
                         (bind !buf_async/6
                          (lambda arg/16
                           ((return
                             (bind
                              ((components/0.%{"Component"})
                               @@ (hashtbl
                                   [("a_prop", (props/0.%{"b_prop"})),
                                    ("c_prop", (props/0.%{"c_prop"})),
                                    ("d_prop", (props/0.%{"e_prop"})),
                                    ("f_prop", (props/0.%{"f_prop"})),
                                    ("g_prop", (Data.string arg/13)),
                                    ("h_prop", (Data.string arg/14)),
                                    ("i_prop", (Data.string arg/15))]))
                              (lambda arg/17
                               ((buffer_add_string arg/16 sync_contents/0)
                                (buffer_add_string arg/16 arg/17)
                                (return (promise arg/16))))))))))
                        (return
                         (bind !buf_async/6
                          (lambda arg/18
                           ((buffer_add_buffer arg/18 buf_sync/6)
                            (return (promise (buffer_contents arg/18))))))))))))))))))
             (lambda arg/23
              ((buffer_add_string arg/22 sync_contents/1)
               (buffer_add_string arg/22 arg/23)
               (return (promise arg/22))))))))))
       (buffer_add_string buf_sync/2 "\n\nComponent with implicit children\n")
       (let$ buf_sync/7 = (buffer_create))
       (let& buf_async/7 = (promise (buffer_create)))
       (buffer_add_string buf_sync/7 " ")
       (let$ sync_contents/3 = (buffer_contents buf_sync/2))
       (buffer_clear buf_sync/2)
       (buf_async/2 :=
        (bind !buf_async/2
         (lambda arg/29
          ((return
            (bind
             (bind
              (bind !buf_async/7
               (lambda arg/28
                ((buffer_add_buffer arg/28 buf_sync/7)
                 (return (promise (buffer_contents arg/28))))))
              (lambda arg/24
               ((let$ buf_sync/8 = (buffer_create))
                (let& buf_async/8 = (promise (buffer_create)))
                (let$ sync_contents/2 = (buffer_contents buf_sync/8))
                (buffer_clear buf_sync/8)
                (buf_async/8 :=
                 (bind !buf_async/8
                  (lambda arg/25
                   ((return
                     (bind
                      ((components/0.%{"Component2"})
                       @@ (hashtbl [("children", (Data.string arg/24))]))
                      (lambda arg/26
                       ((buffer_add_string arg/25 sync_contents/2)
                        (buffer_add_string arg/25 arg/26)
                        (return (promise arg/25))))))))))
                (return
                 (bind !buf_async/8
                  (lambda arg/27
                   ((buffer_add_buffer arg/27 buf_sync/8)
                    (return (promise (buffer_contents arg/27))))))))))
             (lambda arg/30
              ((buffer_add_string arg/29 sync_contents/3)
               (buffer_add_string arg/29 arg/30)
               (return (promise arg/29))))))))))
       (buffer_add_string buf_sync/2 "\n\nPatterns\n\nTuple:\n")
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
        (then (buffer_add_string buf_sync/2 " "))
        (else (buffer_add_string buf_sync/2 " ")))
       (buffer_add_string buf_sync/2 "\n\nList:\n")
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
        (then (buffer_add_string buf_sync/2 "\n"))
        (else
         (if_else (!exit/8 = 1)
          (then
           (buffer_add_string buf_sync/2 " ")
           (stmt
            ((buffer_add_escape/0 @@ buf_sync/2)
             @@ (Data.to_string (match_props/5.%{"a"}))))
           (buffer_add_string buf_sync/2 "\n"))
          (else (buffer_add_string buf_sync/2 "\n")))))
       (buffer_add_string buf_sync/2 "\n\nRecord:\n")
       (let$ arg_match/7 = [(props/0.%{"record"})])
       (let$ match_props/6 = (hashtbl_create))
       (let& exit/9 = -1)
       (let$ match_arg/28 = (arg_match/7.%(0)))
       (let$ match_arg/29 = ((Data.to_hashtbl match_arg/28).%{"!#%@"}))
       (let$ match_arg/30 = ((Data.to_hashtbl match_arg/28).%{"a"}))
       (match_props/6.%{"a"} <- match_arg/30)
       (match_props/6.%{"b"} <- match_arg/29)
       (exit/9 := 0)
       (buffer_add_string buf_sync/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf_sync/2)
         @@ (Data.to_string (match_props/6.%{"a"}))))
       (buffer_add_string buf_sync/2 " ")
       (stmt
        ((buffer_add_escape/0 @@ buf_sync/2)
         @@ (Data.to_string (match_props/6.%{"b"}))))
       (buffer_add_string buf_sync/2 " ")
       (buffer_add_string buf_sync/2 "\n\nEnum:\n")
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
        (then (buffer_add_string buf_sync/2 " "))
        (else (buffer_add_string buf_sync/2 " ")))
       (buffer_add_string buf_sync/2 "\n\nTagged union:\n")
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
         (buffer_add_string buf_sync/2 " ")
         (stmt
          ((buffer_add_escape/0 @@ buf_sync/2)
           @@ (Data.to_string (match_props/7.%{"a"}))))
         (buffer_add_string buf_sync/2 " "))
        (else (buffer_add_string buf_sync/2 "\n")))
       (buffer_add_string buf_sync/2 "\n\nDictionary:\n")
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
        (then (buffer_add_string buf_sync/2 " "))
        (else (buffer_add_string buf_sync/2 " ")))
       (buffer_add_string buf_sync/2 "\n\n! and . precedence works correctly\n")
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
       (buffer_add_string buf_sync/2
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
       (buffer_add_string buf_sync/2 " ")
       (buffer_add_string buf_sync/2 "\n\nStrings may contain line breaks:\n")
       (stmt
        ((buffer_add_escape/0 @@ buf_sync/2)
         @@ (Data.to_string (Data.string "a\nb"))))
       (buffer_add_string buf_sync/2 "\n")
       (return
        (bind !buf_async/2
         (lambda arg/31
          ((buffer_add_buffer arg/31 buf_sync/2)
           (return (promise (buffer_contents arg/31))))))))
      (else (return (error (buffer_contents errors/0))))))))
