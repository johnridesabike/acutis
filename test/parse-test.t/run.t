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
   (text
    no_trim
    "\n\nComponents are only bound once in the instructions.\n"
    no_trim)
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
   (text no_trim "\n\nZero-length record fields:\n" no_trim)
   (echo () fmt_string (echo_field "" (echo_var "zero")) escape)
   (text no_trim "\n" no_trim)
   (match
    ((var "zero"))
    ((case
      (pats (((record (("" (var "empty")))))))
      (nodes
       ((text no_trim " " no_trim)
        (echo () fmt_string (echo_var "empty") escape)
        (text no_trim " " no_trim))))))
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
   (text "\n\nComponents are only bound once in the instructions.\n")
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
   (text "\n\nZero-length record fields:\n")
   (echo () fmt_string (field (var "zero") "") escape)
   (text "\n")
   (match
    ()
    ((var "zero"))
    (matching
     (tree
      (nest
       (key 0)
       (ids ())
       (child
        (string_keys
         (wildcard
          (key "")
          (ids (0))
          (child (end (end (leaf (names (("empty" 0))) (exit 0))))))))
       (wildcard none)))
     (exits
      ((exit
        (id 0)
        (bindings ("empty"))
        (nodes
         ((text " ") (echo () fmt_string (var "empty") escape) (text " "))))))))
   (text "\n"))

Print the runtime instructions
  $ acutis template.acutis component.acutis component2.acutis --printinst
  (let$ buffer_add_escape =
   (lambda arg
    ((return
      (lambda arg
       ((iter (string_to_seq arg)
         (match_char arg
          (('&' ((buffer_add_string arg "&amp;")))
           ('"' ((buffer_add_string arg "&quot;")))
           ('\'' ((buffer_add_string arg "&apos;")))
           ('>' ((buffer_add_string arg "&gt;")))
           ('<' ((buffer_add_string arg "&lt;")))
           ('/' ((buffer_add_string arg "&sol;")))
           ('`' ((buffer_add_string arg "&grave;")))
           ('=' ((buffer_add_string arg "&equals;")))
           (_ ((buffer_add_char arg arg))))))))))))
  (let$ buffer_add_sep =
   (lambda arg
    ((return
      (lambda arg
       ((return
         (lambda arg
          ((if_else (not ((buffer_length arg) = 0))
            (then (buffer_add_string arg arg))
            (else (unit)))
           (buffer_add_string arg arg))))))))))
  (let$ stack_empty = (lambda arg ((unit))))
  (let$ stack_is_empty =
   (lambda arg
    ((let& result = true)
     (stm (arg @@ (lambda arg ((result := false)))))
     (return !result))))
  (let$ stack_add =
   (lambda arg
    ((return
      (lambda arg
       ((return (lambda arg ((stm (arg @@ arg)) (return (arg @@ arg)))))))))))
  (let$ Component =
   (async_lambda arg
    ((let$ buf = (buffer_create))
     (stm ((buffer_add_escape @@ buf) @@ (get_string (arg.%{"a_prop"}))))
     (buffer_add_string buf "\n")
     (stm ((buffer_add_escape @@ buf) @@ (get_string (arg.%{"c_prop"}))))
     (buffer_add_string buf "\n")
     (stm ((buffer_add_escape @@ buf) @@ (get_string (arg.%{"d_prop"}))))
     (buffer_add_string buf "\n")
     (stm ((buffer_add_escape @@ buf) @@ (get_string (arg.%{"f_prop"}))))
     (buffer_add_string buf "\n")
     (stm ((buffer_add_escape @@ buf) @@ (get_string (arg.%{"g_prop"}))))
     (buffer_add_string buf "\n")
     (stm ((buffer_add_escape @@ buf) @@ (get_string (arg.%{"h_prop"}))))
     (buffer_add_string buf "\n")
     (stm ((buffer_add_escape @@ buf) @@ (get_string (arg.%{"i_prop"}))))
     (buffer_add_string buf "\n")
     (return (promise (buffer_contents buf))))))
  (let$ Component2 =
   (async_lambda arg
    ((let$ buf = (buffer_create))
     (stm ((buffer_add_escape @@ buf) @@ (get_string (arg.%{"children"}))))
     (buffer_add_string buf "\n")
     (return (promise (buffer_contents buf))))))
  (export
   (async_lambda arg
    ((let$ errors = (buffer_create))
     (let$ error_aux =
      (lambda arg
       ((return
         (lambda arg
          ((return
            (lambda arg
             ((return
               (lambda arg
                ((if_else (not ((buffer_length errors) = 0))
                  (then (buffer_add_string errors "\n\n"))
                  (else (unit)))
                 (buffer_add_string errors "File \"")
                 (buffer_add_string errors "template.acutis")
                 (buffer_add_string errors
                  "\"\nRender error.\nThe data supplied does not match this template's interface.\n")
                 (buffer_add_string errors "Path:\n<input>")
                 (stm (arg @@ ((buffer_add_sep @@ errors) @@ " -> ")))
                 (buffer_add_string errors "\nExpected type:\n")
                 (buffer_add_string errors arg)
                 (buffer_add_string errors arg)
                 (buffer_add_string errors arg)))))))))))))
     (let$ decode_error =
      (lambda arg
       ((return
         ((error_aux @@ "\nReceived value:\n") @@ (External.to_string arg))))))
     (let$ key_error =
      (lambda arg
       ((let$ buf = (buffer_create))
        (stm (arg @@ ((buffer_add_sep @@ buf) @@ ", ")))
        (return
         ((error_aux @@ "\nInput is missing keys:\n") @@ (buffer_contents buf))))))
     (let$ props = (hashtbl_create))
     (let$ type =
      "{\n  a: {b: {c: false | true}},\n  a_prop: string,\n  b_prop: string,\n  c_prop: string,\n  d: string,\n  dict: <int>,\n  e: string,\n  e_prop: string,\n  ech_a: string,\n  ech_b: false | true,\n  ech_d: ?string,\n  ech_e: ?string,\n  ech_f: float,\n  ech_i: int,\n  enums: (@\"a\" | ..., @1 | ..., false | true, false | true),\n  f_prop: string,\n  list: [?string],\n  map_d: <int>,\n  map_l: [int],\n  match_a: int,\n  match_b: string,\n  numbers:\n    {\n      exp1: float,\n      exp2: float,\n      exp3: float,\n      frac: float,\n      int: int,\n      negfrac: float,\n      negint: int\n    },\n  record: {\"!#%@\": string, a: string},\n  tagged: {@tag: false} | {@tag: true, a: string},\n  trim_a: string,\n  trim_b: string,\n  trim_c: string,\n  trim_d: string,\n  trim_e: string,\n  trim_f: string,\n  trim_g: string,\n  tuple: (int, float, string),\n  zero: {\"\": string}\n}")
     (External.decode (assoc) arg
      (ok decoded
       (let& missing_keys = stack_empty)
       (if_else (External.assoc_mem "a" decoded)
        (then
         (let$ input = (External.assoc_find "a" decoded))
         (let$ stack = ((stack_add @@ "a") @@ stack_empty))
         (let$ type = "{b: {c: false | true}}")
         (External.decode (assoc) input
          (ok decoded
           (let$ decoded = (hashtbl_create))
           (let& missing_keys = stack_empty)
           (if_else (External.assoc_mem "b" decoded)
            (then
             (let$ input = (External.assoc_find "b" decoded))
             (let$ stack = ((stack_add @@ "b") @@ stack))
             (let$ type = "{c: false | true}")
             (External.decode (assoc) input
              (ok decoded
               (let$ decoded = (hashtbl_create))
               (let& missing_keys = stack_empty)
               (if_else (External.assoc_mem "c" decoded)
                (then
                 (let$ input = (External.assoc_find "c" decoded))
                 (let$ stack = ((stack_add @@ "c") @@ stack))
                 (let$ type = "false | true")
                 (External.decode (bool) input
                  (ok decoded
                   (if_else decoded
                    (then (decoded.%{"c"} <- (set_int 1)))
                    (else (decoded.%{"c"} <- (set_int 0)))))
                  (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
                (else (missing_keys := ((stack_add @@ "c") @@ !missing_keys))))
               (if_else (not (stack_is_empty @@ !missing_keys))
                (then (stm (((key_error @@ !missing_keys) @@ stack) @@ type)))
                (else (unit)))
               (decoded.%{"b"} <- (set_hashtbl decoded)))
              (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
            (else (missing_keys := ((stack_add @@ "b") @@ !missing_keys))))
           (if_else (not (stack_is_empty @@ !missing_keys))
            (then (stm (((key_error @@ !missing_keys) @@ stack) @@ type)))
            (else (unit)))
           (props.%{"a"} <- (set_hashtbl decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "a") @@ !missing_keys))))
       (if_else (External.assoc_mem "a_prop" decoded)
        (then
         (let$ input = (External.assoc_find "a_prop" decoded))
         (let$ stack = ((stack_add @@ "a_prop") @@ stack_empty))
         (let$ type = "string")
         (External.decode (string) input
          (ok decoded (props.%{"a_prop"} <- (set_string decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "a_prop") @@ !missing_keys))))
       (if_else (External.assoc_mem "b_prop" decoded)
        (then
         (let$ input = (External.assoc_find "b_prop" decoded))
         (let$ stack = ((stack_add @@ "b_prop") @@ stack_empty))
         (let$ type = "string")
         (External.decode (string) input
          (ok decoded (props.%{"b_prop"} <- (set_string decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "b_prop") @@ !missing_keys))))
       (if_else (External.assoc_mem "c_prop" decoded)
        (then
         (let$ input = (External.assoc_find "c_prop" decoded))
         (let$ stack = ((stack_add @@ "c_prop") @@ stack_empty))
         (let$ type = "string")
         (External.decode (string) input
          (ok decoded (props.%{"c_prop"} <- (set_string decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "c_prop") @@ !missing_keys))))
       (if_else (External.assoc_mem "d" decoded)
        (then
         (let$ input = (External.assoc_find "d" decoded))
         (let$ stack = ((stack_add @@ "d") @@ stack_empty))
         (let$ type = "string")
         (External.decode (string) input
          (ok decoded (props.%{"d"} <- (set_string decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "d") @@ !missing_keys))))
       (if_else (External.assoc_mem "dict" decoded)
        (then
         (let$ input = (External.assoc_find "dict" decoded))
         (let$ stack = ((stack_add @@ "dict") @@ stack_empty))
         (let$ type = "<int>")
         (External.decode (assoc) input
          (ok decoded
           (let$ decoded = (hashtbl_create))
           (iter (External.assoc_to_seq decoded)
            (let$ stack = ((stack_add @@ (fst arg)) @@ stack))
            (let$ type = "int")
            (External.decode (int) (snd arg)
             (ok decoded (decoded.%{(fst arg)} <- (set_int decoded)))
             (error (stm (((decode_error @@ (snd arg)) @@ stack) @@ type))))
            (props.%{"dict"} <- (set_hashtbl decoded))))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "dict") @@ !missing_keys))))
       (if_else (External.assoc_mem "e" decoded)
        (then
         (let$ input = (External.assoc_find "e" decoded))
         (let$ stack = ((stack_add @@ "e") @@ stack_empty))
         (let$ type = "string")
         (External.decode (string) input
          (ok decoded (props.%{"e"} <- (set_string decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "e") @@ !missing_keys))))
       (if_else (External.assoc_mem "e_prop" decoded)
        (then
         (let$ input = (External.assoc_find "e_prop" decoded))
         (let$ stack = ((stack_add @@ "e_prop") @@ stack_empty))
         (let$ type = "string")
         (External.decode (string) input
          (ok decoded (props.%{"e_prop"} <- (set_string decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "e_prop") @@ !missing_keys))))
       (if_else (External.assoc_mem "ech_a" decoded)
        (then
         (let$ input = (External.assoc_find "ech_a" decoded))
         (let$ stack = ((stack_add @@ "ech_a") @@ stack_empty))
         (let$ type = "string")
         (External.decode (string) input
          (ok decoded (props.%{"ech_a"} <- (set_string decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "ech_a") @@ !missing_keys))))
       (if_else (External.assoc_mem "ech_b" decoded)
        (then
         (let$ input = (External.assoc_find "ech_b" decoded))
         (let$ stack = ((stack_add @@ "ech_b") @@ stack_empty))
         (let$ type = "false | true")
         (External.decode (bool) input
          (ok decoded
           (if_else decoded
            (then (props.%{"ech_b"} <- (set_int 1)))
            (else (props.%{"ech_b"} <- (set_int 0)))))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "ech_b") @@ !missing_keys))))
       (if_else (External.assoc_mem "ech_d" decoded)
        (then
         (let$ input = (External.assoc_find "ech_d" decoded))
         (let$ stack = ((stack_add @@ "ech_d") @@ stack_empty))
         (let$ type = "?string")
         (External.decode (some) input
          (ok decoded
           (let$ decoded = [(set_int 0)])
           (let$ stack = ((stack_add @@ "<nullable>") @@ stack))
           (let$ type = "string")
           (External.decode (string) decoded
            (ok decoded (decoded.%(0) <- (set_string decoded)))
            (error (stm (((decode_error @@ decoded) @@ stack) @@ type))))
           (props.%{"ech_d"} <- (set_array decoded)))
          (error (props.%{"ech_d"} <- (set_int 0)))))
        (else (props.%{"ech_d"} <- (set_int 0))))
       (if_else (External.assoc_mem "ech_e" decoded)
        (then
         (let$ input = (External.assoc_find "ech_e" decoded))
         (let$ stack = ((stack_add @@ "ech_e") @@ stack_empty))
         (let$ type = "?string")
         (External.decode (some) input
          (ok decoded
           (let$ decoded = [(set_int 0)])
           (let$ stack = ((stack_add @@ "<nullable>") @@ stack))
           (let$ type = "string")
           (External.decode (string) decoded
            (ok decoded (decoded.%(0) <- (set_string decoded)))
            (error (stm (((decode_error @@ decoded) @@ stack) @@ type))))
           (props.%{"ech_e"} <- (set_array decoded)))
          (error (props.%{"ech_e"} <- (set_int 0)))))
        (else (props.%{"ech_e"} <- (set_int 0))))
       (if_else (External.assoc_mem "ech_f" decoded)
        (then
         (let$ input = (External.assoc_find "ech_f" decoded))
         (let$ stack = ((stack_add @@ "ech_f") @@ stack_empty))
         (let$ type = "float")
         (External.decode (float) input
          (ok decoded (props.%{"ech_f"} <- (set_float decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "ech_f") @@ !missing_keys))))
       (if_else (External.assoc_mem "ech_i" decoded)
        (then
         (let$ input = (External.assoc_find "ech_i" decoded))
         (let$ stack = ((stack_add @@ "ech_i") @@ stack_empty))
         (let$ type = "int")
         (External.decode (int) input
          (ok decoded (props.%{"ech_i"} <- (set_int decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "ech_i") @@ !missing_keys))))
       (if_else (External.assoc_mem "enums" decoded)
        (then
         (let$ input = (External.assoc_find "enums" decoded))
         (let$ stack = ((stack_add @@ "enums") @@ stack_empty))
         (let$ type = "(@\"a\" | ..., @1 | ..., false | true, false | true)")
         (External.decode (seq) input
          (ok decoded
           (let$ decoded = (array_make 4 (set_int 0)))
           (uncons decoded
            (nil (stm (((decode_error @@ input) @@ stack) @@ type)))
            (cons hd seq
             (let$ stack = ((stack_add @@ (string_of_int 0)) @@ stack))
             (let$ type = "@\"a\" | ...")
             (External.decode (string) hd
              (ok decoded (decoded.%(0) <- (set_string decoded)))
              (error (stm (((decode_error @@ hd) @@ stack) @@ type))))
             (uncons seq
              (nil (stm (((decode_error @@ input) @@ stack) @@ type)))
              (cons hd seq
               (let$ stack = ((stack_add @@ (string_of_int 1)) @@ stack))
               (let$ type = "@1 | ...")
               (External.decode (int) hd
                (ok decoded (decoded.%(1) <- (set_int decoded)))
                (error (stm (((decode_error @@ hd) @@ stack) @@ type))))
               (uncons seq
                (nil (stm (((decode_error @@ input) @@ stack) @@ type)))
                (cons hd seq
                 (let$ stack = ((stack_add @@ (string_of_int 2)) @@ stack))
                 (let$ type = "false | true")
                 (External.decode (bool) hd
                  (ok decoded
                   (if_else decoded
                    (then (decoded.%(2) <- (set_int 1)))
                    (else (decoded.%(2) <- (set_int 0)))))
                  (error (stm (((decode_error @@ hd) @@ stack) @@ type))))
                 (uncons seq
                  (nil (stm (((decode_error @@ input) @@ stack) @@ type)))
                  (cons hd seq
                   (let$ stack = ((stack_add @@ (string_of_int 3)) @@ stack))
                   (let$ type = "false | true")
                   (External.decode (bool) hd
                    (ok decoded
                     (if_else decoded
                      (then (decoded.%(3) <- (set_int 1)))
                      (else (decoded.%(3) <- (set_int 0)))))
                    (error (stm (((decode_error @@ hd) @@ stack) @@ type))))
                   (unit)))))))))
           (props.%{"enums"} <- (set_array decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "enums") @@ !missing_keys))))
       (if_else (External.assoc_mem "f_prop" decoded)
        (then
         (let$ input = (External.assoc_find "f_prop" decoded))
         (let$ stack = ((stack_add @@ "f_prop") @@ stack_empty))
         (let$ type = "string")
         (External.decode (string) input
          (ok decoded (props.%{"f_prop"} <- (set_string decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "f_prop") @@ !missing_keys))))
       (if_else (External.assoc_mem "list" decoded)
        (then
         (let$ input = (External.assoc_find "list" decoded))
         (let$ stack = ((stack_add @@ "list") @@ stack_empty))
         (let$ type = "[?string]")
         (External.decode (seq) input
          (ok decoded
           (let& index = 0)
           (let$ decoded = [(set_int 0), (set_int 0)])
           (let& decode_dst = decoded)
           (iter decoded (let$ decode_dst_new = [(set_int 0), (set_int 0)])
            (let$ stack = ((stack_add @@ (string_of_int !index)) @@ stack))
            (let$ type = "?string")
            (External.decode (some) arg
             (ok decoded
              (let$ decoded = [(set_int 0)])
              (let$ stack = ((stack_add @@ "<nullable>") @@ stack))
              (let$ type = "string")
              (External.decode (string) decoded
               (ok decoded (decoded.%(0) <- (set_string decoded)))
               (error (stm (((decode_error @@ decoded) @@ stack) @@ type))))
              (decode_dst_new.%(0) <- (set_array decoded)))
             (error (decode_dst_new.%(0) <- (set_int 0))))
            (!decode_dst.%(1) <- (set_array decode_dst_new)) (incr index)
            (decode_dst := decode_dst_new))
           (props.%{"list"} <- (decoded.%(1))))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "list") @@ !missing_keys))))
       (if_else (External.assoc_mem "map_d" decoded)
        (then
         (let$ input = (External.assoc_find "map_d" decoded))
         (let$ stack = ((stack_add @@ "map_d") @@ stack_empty))
         (let$ type = "<int>")
         (External.decode (assoc) input
          (ok decoded
           (let$ decoded = (hashtbl_create))
           (iter (External.assoc_to_seq decoded)
            (let$ stack = ((stack_add @@ (fst arg)) @@ stack))
            (let$ type = "int")
            (External.decode (int) (snd arg)
             (ok decoded (decoded.%{(fst arg)} <- (set_int decoded)))
             (error (stm (((decode_error @@ (snd arg)) @@ stack) @@ type))))
            (props.%{"map_d"} <- (set_hashtbl decoded))))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "map_d") @@ !missing_keys))))
       (if_else (External.assoc_mem "map_l" decoded)
        (then
         (let$ input = (External.assoc_find "map_l" decoded))
         (let$ stack = ((stack_add @@ "map_l") @@ stack_empty))
         (let$ type = "[int]")
         (External.decode (seq) input
          (ok decoded
           (let& index = 0)
           (let$ decoded = [(set_int 0), (set_int 0)])
           (let& decode_dst = decoded)
           (iter decoded (let$ decode_dst_new = [(set_int 0), (set_int 0)])
            (let$ stack = ((stack_add @@ (string_of_int !index)) @@ stack))
            (let$ type = "int")
            (External.decode (int) arg
             (ok decoded (decode_dst_new.%(0) <- (set_int decoded)))
             (error (stm (((decode_error @@ arg) @@ stack) @@ type))))
            (!decode_dst.%(1) <- (set_array decode_dst_new)) (incr index)
            (decode_dst := decode_dst_new))
           (props.%{"map_l"} <- (decoded.%(1))))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "map_l") @@ !missing_keys))))
       (if_else (External.assoc_mem "match_a" decoded)
        (then
         (let$ input = (External.assoc_find "match_a" decoded))
         (let$ stack = ((stack_add @@ "match_a") @@ stack_empty))
         (let$ type = "int")
         (External.decode (int) input
          (ok decoded (props.%{"match_a"} <- (set_int decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "match_a") @@ !missing_keys))))
       (if_else (External.assoc_mem "match_b" decoded)
        (then
         (let$ input = (External.assoc_find "match_b" decoded))
         (let$ stack = ((stack_add @@ "match_b") @@ stack_empty))
         (let$ type = "string")
         (External.decode (string) input
          (ok decoded (props.%{"match_b"} <- (set_string decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "match_b") @@ !missing_keys))))
       (if_else (External.assoc_mem "numbers" decoded)
        (then
         (let$ input = (External.assoc_find "numbers" decoded))
         (let$ stack = ((stack_add @@ "numbers") @@ stack_empty))
         (let$ type =
          "{\n  exp1: float,\n  exp2: float,\n  exp3: float,\n  frac: float,\n  int: int,\n  negfrac: float,\n  negint: int\n}")
         (External.decode (assoc) input
          (ok decoded
           (let$ decoded = (hashtbl_create))
           (let& missing_keys = stack_empty)
           (if_else (External.assoc_mem "exp1" decoded)
            (then
             (let$ input = (External.assoc_find "exp1" decoded))
             (let$ stack = ((stack_add @@ "exp1") @@ stack))
             (let$ type = "float")
             (External.decode (float) input
              (ok decoded (decoded.%{"exp1"} <- (set_float decoded)))
              (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
            (else (missing_keys := ((stack_add @@ "exp1") @@ !missing_keys))))
           (if_else (External.assoc_mem "exp2" decoded)
            (then
             (let$ input = (External.assoc_find "exp2" decoded))
             (let$ stack = ((stack_add @@ "exp2") @@ stack))
             (let$ type = "float")
             (External.decode (float) input
              (ok decoded (decoded.%{"exp2"} <- (set_float decoded)))
              (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
            (else (missing_keys := ((stack_add @@ "exp2") @@ !missing_keys))))
           (if_else (External.assoc_mem "exp3" decoded)
            (then
             (let$ input = (External.assoc_find "exp3" decoded))
             (let$ stack = ((stack_add @@ "exp3") @@ stack))
             (let$ type = "float")
             (External.decode (float) input
              (ok decoded (decoded.%{"exp3"} <- (set_float decoded)))
              (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
            (else (missing_keys := ((stack_add @@ "exp3") @@ !missing_keys))))
           (if_else (External.assoc_mem "frac" decoded)
            (then
             (let$ input = (External.assoc_find "frac" decoded))
             (let$ stack = ((stack_add @@ "frac") @@ stack))
             (let$ type = "float")
             (External.decode (float) input
              (ok decoded (decoded.%{"frac"} <- (set_float decoded)))
              (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
            (else (missing_keys := ((stack_add @@ "frac") @@ !missing_keys))))
           (if_else (External.assoc_mem "int" decoded)
            (then
             (let$ input = (External.assoc_find "int" decoded))
             (let$ stack = ((stack_add @@ "int") @@ stack))
             (let$ type = "int")
             (External.decode (int) input
              (ok decoded (decoded.%{"int"} <- (set_int decoded)))
              (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
            (else (missing_keys := ((stack_add @@ "int") @@ !missing_keys))))
           (if_else (External.assoc_mem "negfrac" decoded)
            (then
             (let$ input = (External.assoc_find "negfrac" decoded))
             (let$ stack = ((stack_add @@ "negfrac") @@ stack))
             (let$ type = "float")
             (External.decode (float) input
              (ok decoded (decoded.%{"negfrac"} <- (set_float decoded)))
              (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
            (else (missing_keys := ((stack_add @@ "negfrac") @@ !missing_keys))))
           (if_else (External.assoc_mem "negint" decoded)
            (then
             (let$ input = (External.assoc_find "negint" decoded))
             (let$ stack = ((stack_add @@ "negint") @@ stack))
             (let$ type = "int")
             (External.decode (int) input
              (ok decoded (decoded.%{"negint"} <- (set_int decoded)))
              (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
            (else (missing_keys := ((stack_add @@ "negint") @@ !missing_keys))))
           (if_else (not (stack_is_empty @@ !missing_keys))
            (then (stm (((key_error @@ !missing_keys) @@ stack) @@ type)))
            (else (unit)))
           (props.%{"numbers"} <- (set_hashtbl decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "numbers") @@ !missing_keys))))
       (if_else (External.assoc_mem "record" decoded)
        (then
         (let$ input = (External.assoc_find "record" decoded))
         (let$ stack = ((stack_add @@ "record") @@ stack_empty))
         (let$ type = "{\"!#%@\": string, a: string}")
         (External.decode (assoc) input
          (ok decoded
           (let$ decoded = (hashtbl_create))
           (let& missing_keys = stack_empty)
           (if_else (External.assoc_mem "!#%@" decoded)
            (then
             (let$ input = (External.assoc_find "!#%@" decoded))
             (let$ stack = ((stack_add @@ "!#%@") @@ stack))
             (let$ type = "string")
             (External.decode (string) input
              (ok decoded (decoded.%{"!#%@"} <- (set_string decoded)))
              (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
            (else (missing_keys := ((stack_add @@ "!#%@") @@ !missing_keys))))
           (if_else (External.assoc_mem "a" decoded)
            (then
             (let$ input = (External.assoc_find "a" decoded))
             (let$ stack = ((stack_add @@ "a") @@ stack))
             (let$ type = "string")
             (External.decode (string) input
              (ok decoded (decoded.%{"a"} <- (set_string decoded)))
              (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
            (else (missing_keys := ((stack_add @@ "a") @@ !missing_keys))))
           (if_else (not (stack_is_empty @@ !missing_keys))
            (then (stm (((key_error @@ !missing_keys) @@ stack) @@ type)))
            (else (unit)))
           (props.%{"record"} <- (set_hashtbl decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "record") @@ !missing_keys))))
       (if_else (External.assoc_mem "tagged" decoded)
        (then
         (let$ input = (External.assoc_find "tagged" decoded))
         (let$ stack = ((stack_add @@ "tagged") @@ stack_empty))
         (let$ type = "{@tag: false} | {@tag: true, a: string}")
         (External.decode (assoc) input
          (ok decoded
           (if_else (External.assoc_mem "tag" decoded)
            (then
             (External.decode (bool) (External.assoc_find "tag" decoded)
              (ok decoded
               (let$ decoded = (hashtbl_create))
               (if_else (not decoded)
                (then
                 (decoded.%{"tag"} <- (set_int 0))
                 (let& missing_keys = stack_empty)
                 (unit)
                 (if_else (not (stack_is_empty @@ !missing_keys))
                  (then (stm (((key_error @@ !missing_keys) @@ stack) @@ type)))
                  (else (unit))))
                (else
                 (if_else decoded
                  (then
                   (decoded.%{"tag"} <- (set_int 1))
                   (let& missing_keys = stack_empty)
                   (if_else (External.assoc_mem "a" decoded)
                    (then
                     (let$ input = (External.assoc_find "a" decoded))
                     (let$ stack = ((stack_add @@ "a") @@ stack))
                     (let$ type = "string")
                     (External.decode (string) input
                      (ok decoded (decoded.%{"a"} <- (set_string decoded)))
                      (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
                    (else
                     (missing_keys := ((stack_add @@ "a") @@ !missing_keys))))
                   (if_else (not (stack_is_empty @@ !missing_keys))
                    (then
                     (stm (((key_error @@ !missing_keys) @@ stack) @@ type)))
                    (else (unit))))
                  (else (stm (((decode_error @@ input) @@ stack) @@ type))))))
               (props.%{"tagged"} <- (set_hashtbl decoded)))
              (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
            (else (stm (((decode_error @@ input) @@ stack) @@ type)))))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "tagged") @@ !missing_keys))))
       (if_else (External.assoc_mem "trim_a" decoded)
        (then
         (let$ input = (External.assoc_find "trim_a" decoded))
         (let$ stack = ((stack_add @@ "trim_a") @@ stack_empty))
         (let$ type = "string")
         (External.decode (string) input
          (ok decoded (props.%{"trim_a"} <- (set_string decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "trim_a") @@ !missing_keys))))
       (if_else (External.assoc_mem "trim_b" decoded)
        (then
         (let$ input = (External.assoc_find "trim_b" decoded))
         (let$ stack = ((stack_add @@ "trim_b") @@ stack_empty))
         (let$ type = "string")
         (External.decode (string) input
          (ok decoded (props.%{"trim_b"} <- (set_string decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "trim_b") @@ !missing_keys))))
       (if_else (External.assoc_mem "trim_c" decoded)
        (then
         (let$ input = (External.assoc_find "trim_c" decoded))
         (let$ stack = ((stack_add @@ "trim_c") @@ stack_empty))
         (let$ type = "string")
         (External.decode (string) input
          (ok decoded (props.%{"trim_c"} <- (set_string decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "trim_c") @@ !missing_keys))))
       (if_else (External.assoc_mem "trim_d" decoded)
        (then
         (let$ input = (External.assoc_find "trim_d" decoded))
         (let$ stack = ((stack_add @@ "trim_d") @@ stack_empty))
         (let$ type = "string")
         (External.decode (string) input
          (ok decoded (props.%{"trim_d"} <- (set_string decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "trim_d") @@ !missing_keys))))
       (if_else (External.assoc_mem "trim_e" decoded)
        (then
         (let$ input = (External.assoc_find "trim_e" decoded))
         (let$ stack = ((stack_add @@ "trim_e") @@ stack_empty))
         (let$ type = "string")
         (External.decode (string) input
          (ok decoded (props.%{"trim_e"} <- (set_string decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "trim_e") @@ !missing_keys))))
       (if_else (External.assoc_mem "trim_f" decoded)
        (then
         (let$ input = (External.assoc_find "trim_f" decoded))
         (let$ stack = ((stack_add @@ "trim_f") @@ stack_empty))
         (let$ type = "string")
         (External.decode (string) input
          (ok decoded (props.%{"trim_f"} <- (set_string decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "trim_f") @@ !missing_keys))))
       (if_else (External.assoc_mem "trim_g" decoded)
        (then
         (let$ input = (External.assoc_find "trim_g" decoded))
         (let$ stack = ((stack_add @@ "trim_g") @@ stack_empty))
         (let$ type = "string")
         (External.decode (string) input
          (ok decoded (props.%{"trim_g"} <- (set_string decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "trim_g") @@ !missing_keys))))
       (if_else (External.assoc_mem "tuple" decoded)
        (then
         (let$ input = (External.assoc_find "tuple" decoded))
         (let$ stack = ((stack_add @@ "tuple") @@ stack_empty))
         (let$ type = "(int, float, string)")
         (External.decode (seq) input
          (ok decoded
           (let$ decoded = (array_make 3 (set_int 0)))
           (uncons decoded
            (nil (stm (((decode_error @@ input) @@ stack) @@ type)))
            (cons hd seq
             (let$ stack = ((stack_add @@ (string_of_int 0)) @@ stack))
             (let$ type = "int")
             (External.decode (int) hd
              (ok decoded (decoded.%(0) <- (set_int decoded)))
              (error (stm (((decode_error @@ hd) @@ stack) @@ type))))
             (uncons seq
              (nil (stm (((decode_error @@ input) @@ stack) @@ type)))
              (cons hd seq
               (let$ stack = ((stack_add @@ (string_of_int 1)) @@ stack))
               (let$ type = "float")
               (External.decode (float) hd
                (ok decoded (decoded.%(1) <- (set_float decoded)))
                (error (stm (((decode_error @@ hd) @@ stack) @@ type))))
               (uncons seq
                (nil (stm (((decode_error @@ input) @@ stack) @@ type)))
                (cons hd seq
                 (let$ stack = ((stack_add @@ (string_of_int 2)) @@ stack))
                 (let$ type = "string")
                 (External.decode (string) hd
                  (ok decoded (decoded.%(2) <- (set_string decoded)))
                  (error (stm (((decode_error @@ hd) @@ stack) @@ type))))
                 (unit)))))))
           (props.%{"tuple"} <- (set_array decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "tuple") @@ !missing_keys))))
       (if_else (External.assoc_mem "zero" decoded)
        (then
         (let$ input = (External.assoc_find "zero" decoded))
         (let$ stack = ((stack_add @@ "zero") @@ stack_empty))
         (let$ type = "{\"\": string}")
         (External.decode (assoc) input
          (ok decoded
           (let$ decoded = (hashtbl_create))
           (let& missing_keys = stack_empty)
           (if_else (External.assoc_mem "" decoded)
            (then
             (let$ input = (External.assoc_find "" decoded))
             (let$ stack = ((stack_add @@ "") @@ stack))
             (let$ type = "string")
             (External.decode (string) input
              (ok decoded (decoded.%{""} <- (set_string decoded)))
              (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
            (else (missing_keys := ((stack_add @@ "") @@ !missing_keys))))
           (if_else (not (stack_is_empty @@ !missing_keys))
            (then (stm (((key_error @@ !missing_keys) @@ stack) @@ type)))
            (else (unit)))
           (props.%{"zero"} <- (set_hashtbl decoded)))
          (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
        (else (missing_keys := ((stack_add @@ "zero") @@ !missing_keys))))
       (if_else (not (stack_is_empty @@ !missing_keys))
        (then (stm (((key_error @@ !missing_keys) @@ stack_empty) @@ type)))
        (else (unit))))
      (error (stm (((decode_error @@ arg) @@ stack_empty) @@ type))))
     (if_else ((buffer_length errors) = 0)
      (then
       (let$ buf = (buffer_create))
       (buffer_add_string buf "Echoes\n")
       (stm ((buffer_add_escape @@ buf) @@ (get_string (props.%{"ech_a"}))))
       (buffer_add_string buf " ")
       (stm ((buffer_add_escape @@ buf) @@ "b"))
       (buffer_add_string buf " ")
       (let$ nullable = (props.%{"ech_d"}))
       (if_else (test_int nullable)
        (then
         (let$ nullable = (props.%{"ech_e"}))
         (if_else (test_int nullable)
          (then (buffer_add_string buf "f\"g"))
          (else
           (buffer_add_string buf (get_string ((get_array nullable).%(0)))))))
        (else (buffer_add_string buf (get_string ((get_array nullable).%(0))))))
       (buffer_add_string buf "\n")
       (stm
        ((buffer_add_escape @@ buf)
         @@ (string_of_int (get_int (props.%{"ech_i"})))))
       (buffer_add_string buf " ")
       (stm
        ((buffer_add_escape @@ buf)
         @@ (string_of_float (get_float (props.%{"ech_f"})))))
       (buffer_add_string buf " ")
       (stm
        ((buffer_add_escape @@ buf)
         @@ (string_of_bool (not ((get_int (props.%{"ech_b"})) = 0)))))
       (buffer_add_string buf "\n\nNumbers\n")
       (let$ arg_match = [(props.%{"numbers"})])
       (let& exit = -1)
       (let$ match_arg = (arg_match.%(0)))
       (let$ match_arg = ((get_hashtbl match_arg).%{"exp1"}))
       (if_else ((get_float match_arg) = 150)
        (then
         (let$ match_arg = ((get_hashtbl match_arg).%{"exp2"}))
         (if_else ((get_float match_arg) = -1000)
          (then
           (let$ match_arg = ((get_hashtbl match_arg).%{"exp3"}))
           (if_else ((get_float match_arg) = 0.2)
            (then
             (let$ match_arg = ((get_hashtbl match_arg).%{"frac"}))
             (if_else ((get_float match_arg) = 10.55)
              (then
               (let$ match_arg = ((get_hashtbl match_arg).%{"int"}))
               (if_else ((get_int match_arg) = 1000)
                (then
                 (let$ match_arg = ((get_hashtbl match_arg).%{"negfrac"}))
                 (if_else ((get_float match_arg) = -12.34)
                  (then
                   (let$ match_arg = ((get_hashtbl match_arg).%{"negint"}))
                   (if_else ((get_int match_arg) = -999)
                    (then (unit) (exit := 0))
                    (else (unit))))
                  (else (unit))))
                (else (unit))))
              (else (unit))))
            (else (unit))))
          (else (unit))))
        (else (unit)))
       (if_else (!exit = -1) (then (unit) (exit := 1)) (else (unit)))
       (if_else (!exit = 0) (then (unit)) (else (unit)))
       (buffer_add_string buf "\n\nTrim")
       (stm ((buffer_add_escape @@ buf) @@ (get_string (props.%{"trim_a"}))))
       (stm ((buffer_add_escape @@ buf) @@ (get_string (props.%{"trim_b"}))))
       (buffer_add_string buf " ")
       (stm ((buffer_add_escape @@ buf) @@ (get_string (props.%{"trim_c"}))))
       (stm ((buffer_add_escape @@ buf) @@ (get_string (props.%{"trim_d"}))))
       (buffer_add_string buf (get_string (props.%{"trim_e"})))
       (buffer_add_string buf "\n")
       (buffer_add_string buf (get_string (props.%{"trim_f"})))
       (buffer_add_string buf (get_string (props.%{"trim_g"})))
       (buffer_add_string buf "Comments\na ")
       (buffer_add_string buf "b")
       (buffer_add_string buf " c\n\nFlat match\n")
       (let$ arg_match = [(props.%{"match_a"})])
       (let& exit = -1)
       (let$ match_arg = (arg_match.%(0)))
       (if_else ((get_int match_arg) = 1)
        (then (unit) (exit := 0))
        (else
         (if_else ((get_int match_arg) = 2)
          (then (unit) (exit := 0))
          (else
           (if_else ((get_int match_arg) = 3)
            (then (unit) (exit := 1))
            (else (unit) (exit := 2)))))))
       (if_else (!exit = 0)
        (then (unit))
        (else
         (if_else (!exit = 1)
          (then (buffer_add_string buf " "))
          (else (buffer_add_string buf " ")))))
       (buffer_add_string buf "\n\nNested match\n")
       (let$ arg_match = [(props.%{"match_b"})])
       (let$ match_props = (hashtbl_create))
       (let& exit = -1)
       (let$ match_arg = (arg_match.%(0)))
       (match_props.%{"c"} <- match_arg)
       (exit := 0)
       (buffer_add_string buf "\n  ")
       (let$ arg_match = [(props.%{"d"}), (props.%{"e"})])
       (let$ match_props = (hashtbl_create))
       (let& exit = -1)
       (let$ match_arg = (arg_match.%(0)))
       (let$ match_arg = (arg_match.%(1)))
       (match_props.%{"f"} <- match_arg)
       (match_props.%{"g"} <- match_arg)
       (exit := 0)
       (buffer_add_string buf " ")
       (stm ((buffer_add_escape @@ buf) @@ (get_string (match_props.%{"c"}))))
       (buffer_add_string buf " ")
       (stm ((buffer_add_escape @@ buf) @@ (get_string (match_props.%{"f"}))))
       (buffer_add_string buf " ")
       (stm ((buffer_add_escape @@ buf) @@ (get_string (match_props.%{"g"}))))
       (buffer_add_string buf " ")
       (buffer_add_string buf "\n")
       (buffer_add_string buf "\n\nMap list\n")
       (let& index = 0)
       (let& cell = (props.%{"map_l"}))
       (while (not (test_int !cell))
        ((let$ match_props = (hashtbl_create))
         (let$ list = (get_array !cell))
         (let$ head = (list.%(0)))
         (let& exit = -1)
         (if_else ((get_int head) = 1)
          (then (unit) (exit := 0))
          (else
           (if_else ((get_int head) = 2)
            (then (unit) (exit := 0))
            (else
             (if_else ((get_int head) = 3)
              (then (match_props.%{"i"} <- (set_int !index)) (exit := 1))
              (else (unit) (exit := 2)))))))
         (if_else (!exit = 0)
          (then (unit))
          (else
           (if_else (!exit = 1)
            (then
             (buffer_add_string buf " ")
             (stm
              ((buffer_add_escape @@ buf)
               @@ (string_of_int (get_int (match_props.%{"i"})))))
             (buffer_add_string buf " "))
            (else (buffer_add_string buf " ")))))
         (incr index)
         (cell := (list.%(1)))))
       (buffer_add_string buf "\n\nMap dict\n")
       (let$ match_arg = (props.%{"map_d"}))
       (iter (hashtbl_to_seq (get_hashtbl match_arg))
        (let$ match_props = (hashtbl_create)) (let& exit = -1)
        (if_else ((get_int (snd arg)) = 1)
         (then (unit) (exit := 0))
         (else
          (if_else ((get_int (snd arg)) = 2)
           (then (unit) (exit := 0))
           (else
            (if_else ((get_int (snd arg)) = 3)
             (then (match_props.%{"k"} <- (set_string (fst arg))) (exit := 1))
             (else (unit) (exit := 2)))))))
        (if_else (!exit = 0)
         (then (unit))
         (else
          (if_else (!exit = 1)
           (then
            (buffer_add_string buf " ")
            (stm
             ((buffer_add_escape @@ buf) @@ (get_string (match_props.%{"k"}))))
            (buffer_add_string buf " "))
           (else (buffer_add_string buf "\n"))))))
       (buffer_add_string buf "\n\nComponent with props\n")
       (let$ buf = (buffer_create))
       (buffer_add_string buf " ")
       (let$ buf = (buffer_create))
       (let$ arg_match = [(props.%{"a_prop"})])
       (let$ match_props = (hashtbl_create))
       (let& exit = -1)
       (let$ match_arg = (arg_match.%(0)))
       (match_props.%{"b_prop"} <- match_arg)
       (exit := 0)
       (buffer_add_string buf " ")
       (stm
        ((buffer_add_escape @@ buf) @@ (get_string (match_props.%{"b_prop"}))))
       (buffer_add_string buf " ")
       (let$ buf = (buffer_create))
       (unit)
       (buffer_add_string buf
        (await
         (Component
          @@ (hashtbl
              [("a_prop", (props.%{"b_prop"})),
               ("c_prop", (props.%{"c_prop"})),
               ("d_prop", (props.%{"e_prop"})),
               ("f_prop", (props.%{"f_prop"})),
               ("g_prop", (set_string (buffer_contents buf))),
               ("h_prop", (set_string (buffer_contents buf))),
               ("i_prop", (set_string (buffer_contents buf)))]))))
       (buffer_add_string buf "\n\nComponent with implicit children\n")
       (let$ buf = (buffer_create))
       (buffer_add_string buf " ")
       (buffer_add_string buf
        (await
         (Component2
          @@ (hashtbl [("children", (set_string (buffer_contents buf)))]))))
       (buffer_add_string buf
        "\n\nComponents are only bound once in the instructions.\n")
       (let$ buf = (buffer_create))
       (buffer_add_string buf " ")
       (buffer_add_string buf
        (await
         (Component2
          @@ (hashtbl [("children", (set_string (buffer_contents buf)))]))))
       (buffer_add_string buf "\n\nPatterns\n\nTuple:\n")
       (let$ arg_match = [(props.%{"tuple"})])
       (let& exit = -1)
       (let$ match_arg = (arg_match.%(0)))
       (let$ match_arg = ((get_array match_arg).%(0)))
       (if_else ((get_int match_arg) = 1)
        (then
         (let$ match_arg = ((get_array match_arg).%(1)))
         (if_else ((get_float match_arg) = 2.5)
          (then
           (let$ match_arg = ((get_array match_arg).%(2)))
           (if_else ((get_string match_arg) = "a")
            (then (unit) (exit := 0))
            (else (unit))))
          (else (unit))))
        (else (unit)))
       (if_else (!exit = -1) (then (unit) (exit := 1)) (else (unit)))
       (if_else (!exit = 0)
        (then (buffer_add_string buf " "))
        (else (buffer_add_string buf " ")))
       (buffer_add_string buf "\n\nList:\n")
       (let$ arg_match = [(props.%{"list"})])
       (let$ match_props = (hashtbl_create))
       (let& exit = -1)
       (let$ match_arg = (arg_match.%(0)))
       (if_else (test_int match_arg)
        (then (unit) (exit := 0))
        (else
         (let$ match_arg = (arg_match.%(0)))
         (let$ match_arg = ((get_array match_arg).%(0)))
         (if_else (test_int match_arg)
          (then
           (let$ match_arg = ((get_array match_arg).%(1)))
           (match_props.%{"_tl"} <- match_arg)
           (match_props.%{"_z"} <- match_arg)
           (exit := 2))
          (else
           (let$ match_arg = ((get_array match_arg).%(0)))
           (let$ match_arg = ((get_array match_arg).%(0)))
           (let$ match_arg = ((get_array match_arg).%(1)))
           (if_else (test_int match_arg)
            (then
             (match_props.%{"_tl"} <- match_arg)
             (match_props.%{"_z"} <- match_arg)
             (exit := 2))
            (else
             (let$ match_arg = ((get_array match_arg).%(1)))
             (let$ match_arg = ((get_array match_arg).%(0)))
             (if_else (test_int match_arg)
              (then
               (let$ match_arg = ((get_array match_arg).%(1)))
               (if_else (test_int match_arg)
                (then (match_props.%{"a"} <- match_arg) (exit := 1))
                (else (unit))))
              (else (unit)))
             (if_else (!exit = -1)
              (then
               (match_props.%{"_tl"} <- match_arg)
               (match_props.%{"_z"} <- match_arg)
               (exit := 2))
              (else (unit)))))))))
       (if_else (!exit = 0)
        (then (buffer_add_string buf "\n"))
        (else
         (if_else (!exit = 1)
          (then
           (buffer_add_string buf " ")
           (stm
            ((buffer_add_escape @@ buf) @@ (get_string (match_props.%{"a"}))))
           (buffer_add_string buf "\n"))
          (else (buffer_add_string buf "\n")))))
       (buffer_add_string buf "\n\nRecord:\n")
       (let$ arg_match = [(props.%{"record"})])
       (let$ match_props = (hashtbl_create))
       (let& exit = -1)
       (let$ match_arg = (arg_match.%(0)))
       (let$ match_arg = ((get_hashtbl match_arg).%{"!#%@"}))
       (let$ match_arg = ((get_hashtbl match_arg).%{"a"}))
       (match_props.%{"a"} <- match_arg)
       (match_props.%{"b"} <- match_arg)
       (exit := 0)
       (buffer_add_string buf " ")
       (stm ((buffer_add_escape @@ buf) @@ (get_string (match_props.%{"a"}))))
       (buffer_add_string buf " ")
       (stm ((buffer_add_escape @@ buf) @@ (get_string (match_props.%{"b"}))))
       (buffer_add_string buf " ")
       (buffer_add_string buf "\n\nEnum:\n")
       (let$ arg_match = [(props.%{"enums"})])
       (let& exit = -1)
       (let$ match_arg = (arg_match.%(0)))
       (let$ match_arg = ((get_array match_arg).%(0)))
       (if_else ((get_string match_arg) = "a")
        (then
         (let$ match_arg = ((get_array match_arg).%(1)))
         (if_else ((get_int match_arg) = 1)
          (then
           (let$ match_arg = ((get_array match_arg).%(2)))
           (if_else ((get_int match_arg) = 1)
            (then
             (let$ match_arg = ((get_array match_arg).%(3)))
             (if_else ((get_int match_arg) = 0)
              (then (unit) (exit := 0))
              (else (unit))))
            (else (unit))))
          (else (unit))))
        (else (unit)))
       (if_else (!exit = -1) (then (unit) (exit := 1)) (else (unit)))
       (if_else (!exit = 0)
        (then (buffer_add_string buf " "))
        (else (buffer_add_string buf " ")))
       (buffer_add_string buf "\n\nTagged union:\n")
       (let$ arg_match = [(props.%{"tagged"})])
       (let$ match_props = (hashtbl_create))
       (let& exit = -1)
       (let$ match_arg = (arg_match.%(0)))
       (let$ match_arg = ((get_hashtbl match_arg).%{"tag"}))
       (if_else ((get_int match_arg) = 0)
        (then (unit) (exit := 1))
        (else
         (if_else ((get_int match_arg) = 1)
          (then
           (let$ match_arg = ((get_hashtbl match_arg).%{"a"}))
           (match_props.%{"a"} <- match_arg)
           (exit := 0))
          (else (unit)))))
       (if_else (!exit = 0)
        (then
         (buffer_add_string buf " ")
         (stm ((buffer_add_escape @@ buf) @@ (get_string (match_props.%{"a"}))))
         (buffer_add_string buf " "))
        (else (buffer_add_string buf "\n")))
       (buffer_add_string buf "\n\nDictionary:\n")
       (let$ arg_match = [(props.%{"dict"})])
       (let& exit = -1)
       (let$ match_arg = (arg_match.%(0)))
       (if_else (hashtbl_mem (get_hashtbl match_arg) "a")
        (then
         (let$ match_arg = ((get_hashtbl match_arg).%{"a"}))
         (if_else ((get_int match_arg) = 1)
          (then
           (if_else (hashtbl_mem (get_hashtbl match_arg) "b")
            (then
             (let$ match_arg = ((get_hashtbl match_arg).%{"b"}))
             (if_else ((get_int match_arg) = 2)
              (then (unit) (exit := 0))
              (else (unit))))
            (else (unit))))
          (else (unit))))
        (else (unit)))
       (if_else (!exit = -1) (then (unit) (exit := 1)) (else (unit)))
       (if_else (!exit = 0)
        (then (buffer_add_string buf " "))
        (else (buffer_add_string buf " ")))
       (buffer_add_string buf "\n\n! and . precedence works correctly\n")
       (let$ arg_match =
        [(set_array
          [(set_array
            [((get_hashtbl ((get_hashtbl (props.%{"a"})).%{"b"})).%{"c"})])])])
       (let& exit = -1)
       (let$ match_arg = (arg_match.%(0)))
       (if_else (test_int match_arg)
        (then (unit) (exit := 1))
        (else
         (let$ match_arg = (arg_match.%(0)))
         (let$ match_arg = ((get_array match_arg).%(0)))
         (if_else (not (test_int match_arg))
          (then
           (let$ match_arg = ((get_array match_arg).%(0)))
           (let$ match_arg = ((get_array match_arg).%(0)))
           (if_else ((get_int match_arg) = 0)
            (then (unit) (exit := 0))
            (else (unit))))
          (else (unit)))
         (if_else (!exit = -1) (then (unit) (exit := 1)) (else (unit)))))
       (if_else (!exit = 0) (then (unit)) (else (unit)))
       (buffer_add_string buf
        "\n\nOther syntax features\n\nTrailing commas parse correctly:\n")
       (let$ arg_match =
        [(set_hashtbl
          (hashtbl
           [("a",
             (set_array [(set_int 1), (set_array [(set_int 2), (set_int 0)])])),
            ("b", (set_array [(set_int 3), (set_int 4)])),
            ("c", (set_hashtbl (hashtbl [("k", (set_int 5))])))]))])
       (let& exit = -1)
       (let$ match_arg = (arg_match.%(0)))
       (unit)
       (exit := 0)
       (buffer_add_string buf " ")
       (buffer_add_string buf "\n\nStrings may contain line breaks:\n")
       (stm ((buffer_add_escape @@ buf) @@ "a\nb"))
       (buffer_add_string buf "\n\nZero-length record fields:\n")
       (stm
        ((buffer_add_escape @@ buf)
         @@ (get_string ((get_hashtbl (props.%{"zero"})).%{""}))))
       (buffer_add_string buf "\n")
       (let$ arg_match = [(props.%{"zero"})])
       (let$ match_props = (hashtbl_create))
       (let& exit = -1)
       (let$ match_arg = (arg_match.%(0)))
       (let$ match_arg = ((get_hashtbl match_arg).%{""}))
       (match_props.%{"empty"} <- match_arg)
       (exit := 0)
       (buffer_add_string buf " ")
       (stm
        ((buffer_add_escape @@ buf) @@ (get_string (match_props.%{"empty"}))))
       (buffer_add_string buf " ")
       (buffer_add_string buf "\n")
       (return (promise (buffer_contents buf))))
      (else (return (error (buffer_contents errors))))))))
