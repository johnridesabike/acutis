Print the untyped AST to make sure parsing works
  $ acutis template.acutis --printast
  ((Text No_trim "Echoes\n" No_trim)
   (Echo () Fmt_string (Echo_var "ech_a") Escape)
   (Text No_trim " " No_trim)
   (Echo () Fmt_string (Echo_string "b") Escape)
   (Text No_trim " " No_trim)
   (Echo
    ((Fmt_string (Echo_var "ech_d")) (Fmt_string (Echo_var "ech_e")))
    Fmt_string
    (Echo_string "f\"g")
    No_escape)
   (Text No_trim "\n" No_trim)
   (Echo () Fmt_int (Echo_var "ech_i") Escape)
   (Text No_trim " " No_trim)
   (Echo () Fmt_float (Echo_var "ech_f") Escape)
   (Text No_trim " " No_trim)
   (Echo () Fmt_bool (Echo_var "ech_b") Escape)
   (Text No_trim "\n\nNumbers\n" No_trim)
   (Match
    ((Var "numbers"))
    (((pats
       ((((Record
           (("int" (Value (Int 1000)))
            ("frac" (Value (Float 10.55)))
            ("negint" (Value (Int -999)))
            ("negfrac" (Value (Float -12.34)))
            ("exp1" (Value (Float 150)))
            ("exp2" (Value (Float -1000)))
            ("exp3" (Value (Float 0.2)))))))))
      (nodes ((Text No_trim "" No_trim))))
     ((pats ((((Var "_"))))) (nodes ((Text No_trim "" No_trim))))))
   (Text No_trim "\n\nTrim\n" Trim)
   (Echo () Fmt_string (Echo_var "trim_a") Escape)
   (Text No_trim " " Trim)
   (Echo () Fmt_string (Echo_var "trim_b") Escape)
   (Text No_trim " " No_trim)
   (Echo () Fmt_string (Echo_var "trim_c") Escape)
   (Text Trim " " Trim)
   (Echo () Fmt_string (Echo_var "trim_d") Escape)
   (Text Trim " " Trim)
   (Echo () Fmt_string (Echo_var "trim_e") No_escape)
   (Text No_trim "\n" No_trim)
   (Echo () Fmt_string (Echo_var "trim_f") No_escape)
   (Text Trim " " Trim)
   (Echo () Fmt_string (Echo_var "trim_g") No_escape)
   (Text Trim "\n\nComments\na " No_trim)
   (Comment "{* {* *} *}")
   (Text No_trim "b" No_trim)
   (Comment "{*\n *}")
   (Text No_trim " c\n\nFlat match\n" No_trim)
   (Match
    ((Var "match_a"))
    (((pats ((((Int 1))) (((Int 2))))) (nodes ((Text No_trim "" No_trim))))
     ((pats ((((Int 3))))) (nodes ((Text No_trim " " No_trim))))
     ((pats ((((Var "_"))))) (nodes ((Text No_trim " " No_trim))))))
   (Text No_trim "\n\nNested match\n" No_trim)
   (Match
    ((Var "match_b"))
    (((pats ((((Var "c")))))
      (nodes
       ((Text No_trim "\n  " No_trim)
        (Match
         ((Var "d") (Var "e"))
         (((pats ((((Var "f") (Var "g")))))
           (nodes
            ((Text No_trim " " No_trim)
             (Echo () Fmt_string (Echo_var "c") Escape)
             (Text No_trim " " No_trim)
             (Echo () Fmt_string (Echo_var "f") Escape)
             (Text No_trim " " No_trim)
             (Echo () Fmt_string (Echo_var "g") Escape)
             (Text No_trim " " No_trim))))))
        (Text No_trim "\n" No_trim))))))
   (Text No_trim "\n\nMap list\n" No_trim)
   (Map
    (Var "map_l")
    (((pats ((((Int 1))) (((Int 2))))) (nodes ((Text No_trim "" No_trim))))
     ((pats ((((Int 3) (Var "i")))))
      (nodes
       ((Text No_trim " " No_trim)
        (Echo () Fmt_int (Echo_var "i") Escape)
        (Text No_trim " " No_trim))))
     ((pats ((((Var "_"))))) (nodes ((Text No_trim " " No_trim))))))
   (Text No_trim "\n\nMap dict\n" No_trim)
   (Map_dict
    (Var "map_d")
    (((pats ((((Int 1))) (((Int 2))))) (nodes ((Text No_trim "" No_trim))))
     ((pats ((((Int 3) (Var "k")))))
      (nodes
       ((Text No_trim " " No_trim)
        (Echo () Fmt_string (Echo_var "k") Escape)
        (Text No_trim " " No_trim))))
     ((pats ((((Var "_"))))) (nodes ((Text No_trim "\n" No_trim))))))
   (Text No_trim "\n\nComponent with props\n" No_trim)
   (Component
    "Component"
    "Component"
    (("a_prop" (Var "b_prop"))
     ("c_prop" (Var "c_prop"))
     ("d_prop" (Var "e_prop"))
     ("f_prop" (Var "f_prop"))
     ("g_prop" (Block ((Text No_trim " " No_trim))))
     ("h_prop"
      (Block
       ((Text No_trim "" No_trim)
        (Match
         ((Var "a_prop"))
         (((pats ((((Var "b_prop")))))
           (nodes
            ((Text No_trim " " No_trim)
             (Echo () Fmt_string (Echo_var "b_prop") Escape)
             (Text No_trim " " No_trim))))))
        (Text No_trim "" No_trim))))
     ("i_prop" (Block ()))))
   (Text No_trim "\n\nComponent with implicit children\n" No_trim)
   (Component
    "Component2"
    "Component2"
    (("children" (Block ((Text No_trim " " No_trim))))))
   (Text
    No_trim
    "\n\nComponents are only bound once in the instructions.\n"
    No_trim)
   (Component
    "Component2"
    "Component2"
    (("children" (Block ((Text No_trim " " No_trim))))))
   (Text No_trim "\n\nPatterns\n\nTuple:\n" No_trim)
   (Match
    ((Var "tuple"))
    (((pats ((((Tuple ((Int 1) (Float 2.5) (String "a")))))))
      (nodes ((Text No_trim " " No_trim))))
     ((pats ((((Var "_"))))) (nodes ((Text No_trim " " No_trim))))))
   (Text No_trim "\n\nList:\n" No_trim)
   (Match
    ((Var "list"))
    (((pats ((((List () None))))) (nodes ((Text No_trim "\n" No_trim))))
     ((pats ((((List ((Nullable (Some (Var "a"))) (Nullable None)) None)))))
      (nodes
       ((Text No_trim " " No_trim)
        (Echo () Fmt_string (Echo_var "a") Escape)
        (Text No_trim "\n" No_trim))))
     ((pats ((((List ((Var "_z")) (Some (Var "_tl")))))))
      (nodes ((Text No_trim "\n" No_trim))))))
   (Text No_trim "\n\nRecord:\n" No_trim)
   (Match
    ((Var "record"))
    (((pats ((((Record (("a" (Value (Var "a"))) ("!#%@" (Value (Var "b")))))))))
      (nodes
       ((Text No_trim " " No_trim)
        (Echo () Fmt_string (Echo_var "a") Escape)
        (Text No_trim " " No_trim)
        (Echo () Fmt_string (Echo_var "b") Escape)
        (Text No_trim " " No_trim))))))
   (Text No_trim "\n\nEnum:\n" No_trim)
   (Match
    ((Var "enums"))
    (((pats ((((Tuple ((Enum_string "a") (Enum_int 1) (Bool 1) (Bool 0)))))))
      (nodes ((Text No_trim " " No_trim))))
     ((pats ((((Var "_"))))) (nodes ((Text No_trim " " No_trim))))))
   (Text No_trim "\n\nTagged union:\n" No_trim)
   (Match
    ((Var "tagged"))
    (((pats ((((Record (("tag" (Tag (Tag_bool 1))) ("a" (Value (Var "a")))))))))
      (nodes
       ((Text No_trim " " No_trim)
        (Echo () Fmt_string (Echo_var "a") Escape)
        (Text No_trim " " No_trim))))
     ((pats ((((Record (("tag" (Tag (Tag_bool 0)))))))))
      (nodes ((Text No_trim "\n" No_trim))))))
   (Text No_trim "\n\nDictionary:\n" No_trim)
   (Match
    ((Var "dict"))
    (((pats ((((Dict (("a" (Int 1)) ("b" (Int 2))))))))
      (nodes ((Text No_trim " " No_trim))))
     ((pats ((((Var "_"))))) (nodes ((Text No_trim " " No_trim))))))
   (Text No_trim "\n\n! and . precedence works correctly\n" No_trim)
   (Match
    ((Nullable (Some (Nullable (Some (Field (Field (Var "a") "b") "c"))))))
    (((pats ((((Nullable (Some (Nullable (Some (Bool 0)))))))))
      (nodes ((Text No_trim "" No_trim))))
     ((pats ((((Var "_"))))) (nodes ((Text No_trim "" No_trim))))))
   (Text
    No_trim
    "\n\nOther syntax features\n\nTrailing commas parse correctly:\n"
    No_trim)
   (Match
    ((Record
      (("a" (Value (List ((Int 1) (Int 2)) None)))
       ("b" (Value (Tuple ((Int 3) (Int 4)))))
       ("c" (Value (Dict (("k" (Int 5)))))))))
    (((pats ((((Var "_"))))) (nodes ((Text No_trim " " No_trim))))))
   (Text No_trim "\n\nStrings may contain line breaks:\n" No_trim)
   (Echo () Fmt_string (Echo_string "a\nb") Escape)
   (Text No_trim "\n\nZero-length record fields:\n" No_trim)
   (Echo () Fmt_string (Echo_field (Echo_var "zero") "") Escape)
   (Text No_trim "\n" No_trim)
   (Match
    ((Var "zero"))
    (((pats ((((Record (("" (Value (Var "empty")))))))))
      (nodes
       ((Text No_trim " " No_trim)
        (Echo () Fmt_string (Echo_var "empty") Escape)
        (Text No_trim " " No_trim))))))
   (Text No_trim "\n" No_trim))

Interfaces parse correctly. Use a separate file to minimize type conficts.
  $ acutis interface.acutis --printast
  ((Text No_trim "" No_trim)
   (Interface
    (((name "a")
      (ty
       (Ty_record
        (((("a" (Value (Ty_enum_int (0 1) (`Closed))))
           ("b" (Value (Ty_enum_string ("a" "b") (`Closed)))))))
        (`Closed))))
     ((name "b")
      (ty
       (Ty_record
        (((("tag" (Tag (Tag_bool 1))) ("a" (Value (Ty_list (Ty_named "int"))))))
         ((("tag" (Tag (Tag_bool 0)))
           ("a" (Value (Ty_dict (Ty_nullable (Ty_named "string"))))))))
        (`Closed))))
     ((name "c")
      (ty
       (Ty_record
        (((("tag" (Tag (Tag_int 0)))))
         ((("tag" (Tag (Tag_int 1)))
           ("a" (Value (Ty_tuple ((Ty_named "float") (Ty_enum_bool (1 0)))))))))
        (`Closed))))
     ((name "d")
      (ty
       (Ty_record
        (((("tag" (Tag (Tag_string "a"))) ("a" (Value (Ty_named "float")))))
         ((("tag" (Tag (Tag_string "b")))
           ("a" (Value (Ty_enum_int (0 1) (`Open)))))))
        (`Closed))))
     ((name "e")
      (ty
       (Ty_record
        (((("tag" (Tag (Tag_int 0))) ("a" (Value (Ty_named "_")))))
         ((("tag" (Tag (Tag_int 1)))
           ("b" (Value (Ty_enum_string ("a" "b") (`Open)))))))
        (`Open))))
     ((name "trailing_commas")
      (ty
       (Ty_record
        (((("a" (Value (Ty_tuple ((Ty_named "int") (Ty_named "string"))))))))
        (`Closed))))
     ((name "children") (ty (Ty_named "string")))
     ((name "optionalChildren") (ty (Ty_nullable (Ty_named "string"))))))
   (Text No_trim "\n" No_trim))

Print the optimized form
  $ acutis template.acutis component.acutis component2.acutis --printopt
  ((Text "Echoes\n")
   (Echo () Fmt_string (`Var "ech_a") Escape)
   (Text " ")
   (Echo () Fmt_string (`String "b") Escape)
   (Text " ")
   (Echo
    ((Fmt_string (`Var "ech_d")) (Fmt_string (`Var "ech_e")))
    Fmt_string
    (`String "f\"g")
    No_escape)
   (Text "\n")
   (Echo () Fmt_int (`Var "ech_i") Escape)
   (Text " ")
   (Echo () Fmt_float (`Var "ech_f") Escape)
   (Text " ")
   (Echo () Fmt_bool (`Var "ech_b") Escape)
   (Text "\n\nNumbers\n")
   (Match
    ()
    ((`Var "numbers"))
    ((tree
      (Nest
       ((key 0)
        (ids (Set ()))
        (child
         (String_keys
          (Switch
           ((key "exp1")
            (ids (Set ()))
            (cases
             ((data (`Float 150))
              (if_match
               (Switch
                ((key "exp2")
                 (ids (Set ()))
                 (cases
                  ((data (`Float -1000))
                   (if_match
                    (Switch
                     ((key "exp3")
                      (ids (Set ()))
                      (cases
                       ((data (`Float 0.2))
                        (if_match
                         (Switch
                          ((key "frac")
                           (ids (Set ()))
                           (cases
                            ((data (`Float 10.55))
                             (if_match
                              (Switch
                               ((key "int")
                                (ids (Set ()))
                                (cases
                                 ((data (`Int 1000))
                                  (if_match
                                   (Switch
                                    ((key "negfrac")
                                     (ids (Set ()))
                                     (cases
                                      ((data (`Float -12.34))
                                       (if_match
                                        (Switch
                                         ((key "negint")
                                          (ids (Set ()))
                                          (cases
                                           ((data (`Int -999))
                                            (if_match
                                             (end
                                              (end ((names (Map ())) (exit 0)))))
                                            (next None)))
                                          (wildcard None)
                                          (check_cases None))))
                                       (next None)))
                                     (wildcard None)
                                     (check_cases None))))
                                  (next None)))
                                (wildcard None)
                                (check_cases None))))
                             (next None)))
                           (wildcard None)
                           (check_cases None))))
                        (next None)))
                      (wildcard None)
                      (check_cases None))))
                   (next None)))
                 (wildcard None)
                 (check_cases None))))
              (next None)))
            (wildcard None)
            (check_cases None)))))
        (wildcard (Some (end ((names (Map ())) (exit 1))))))))
     (exits
      (((id 0) (bindings ()) (nodes ())) ((id 1) (bindings ()) (nodes ()))))))
   (Text "\n\nTrim")
   (Echo () Fmt_string (`Var "trim_a") Escape)
   (Echo () Fmt_string (`Var "trim_b") Escape)
   (Text " ")
   (Echo () Fmt_string (`Var "trim_c") Escape)
   (Echo () Fmt_string (`Var "trim_d") Escape)
   (Echo () Fmt_string (`Var "trim_e") No_escape)
   (Text "\n")
   (Echo () Fmt_string (`Var "trim_f") No_escape)
   (Echo () Fmt_string (`Var "trim_g") No_escape)
   (Text "Comments\na ")
   (Text "b")
   (Text " c\n\nFlat match\n")
   (Match
    ()
    ((`Var "match_a"))
    ((tree
      (Switch
       ((key 0)
        (ids (Set ()))
        (cases
         ((data (`Int 1))
          (if_match (end ((names (Map ())) (exit 0))))
          (next
           (Some
            ((data (`Int 2))
             (if_match (end ((names (Map ())) (exit 0))))
             (next
              (Some
               ((data (`Int 3))
                (if_match (end ((names (Map ())) (exit 1))))
                (next None)))))))))
        (wildcard (Some (end ((names (Map ())) (exit 2)))))
        (check_cases None))))
     (exits
      (((id 0) (bindings ()) (nodes ()))
       ((id 1) (bindings ()) (nodes ((Text " "))))
       ((id 2) (bindings ()) (nodes ((Text " "))))))))
   (Text "\n\nNested match\n")
   (Match
    ()
    ((`Var "match_b"))
    ((tree
      (Wildcard
       ((key 0)
        (ids (Set (0)))
        (child (end ((names (Map (("c" 0)))) (exit 0)))))))
     (exits
      (((id 0)
        (bindings ("c"))
        (nodes
         ((Text "\n  ")
          (Match
           ()
           ((`Var "d") (`Var "e"))
           ((tree
             (Wildcard
              ((key 0)
               (ids (Set (0)))
               (child
                (Wildcard
                 ((key 1)
                  (ids (Set (1)))
                  (child (end ((names (Map (("f" 0) ("g" 1)))) (exit 0))))))))))
            (exits
             (((id 0)
               (bindings ("f" "g"))
               (nodes
                ((Text " ")
                 (Echo () Fmt_string (`Var "c") Escape)
                 (Text " ")
                 (Echo () Fmt_string (`Var "f") Escape)
                 (Text " ")
                 (Echo () Fmt_string (`Var "g") Escape)
                 (Text " "))))))))
          (Text "\n"))))))))
   (Text "\n\nMap list\n")
   (Map_list
    ()
    (`Var "map_l")
    ((tree
      (Switch
       ((key 0)
        (ids (Set ()))
        (cases
         ((data (`Int 1))
          (if_match
           (Wildcard
            ((key 1) (ids (Set ())) (child (end ((names (Map ())) (exit 0)))))))
          (next
           (Some
            ((data (`Int 2))
             (if_match
              (Wildcard
               ((key 1)
                (ids (Set ()))
                (child (end ((names (Map ())) (exit 0)))))))
             (next
              (Some
               ((data (`Int 3))
                (if_match
                 (Wildcard
                  ((key 1)
                   (ids (Set (0)))
                   (child (end ((names (Map (("i" 0)))) (exit 1)))))))
                (next None)))))))))
        (wildcard
         (Some
          (Wildcard
           ((key 1) (ids (Set ())) (child (end ((names (Map ())) (exit 2))))))))
        (check_cases None))))
     (exits
      (((id 0) (bindings ()) (nodes ()))
       ((id 1)
        (bindings ("i"))
        (nodes ((Text " ") (Echo () Fmt_int (`Var "i") Escape) (Text " "))))
       ((id 2) (bindings ()) (nodes ((Text " "))))))))
   (Text "\n\nMap dict\n")
   (Map_dict
    ()
    (`Var "map_d")
    ((tree
      (Switch
       ((key 0)
        (ids (Set ()))
        (cases
         ((data (`Int 1))
          (if_match
           (Wildcard
            ((key 1) (ids (Set ())) (child (end ((names (Map ())) (exit 0)))))))
          (next
           (Some
            ((data (`Int 2))
             (if_match
              (Wildcard
               ((key 1)
                (ids (Set ()))
                (child (end ((names (Map ())) (exit 0)))))))
             (next
              (Some
               ((data (`Int 3))
                (if_match
                 (Wildcard
                  ((key 1)
                   (ids (Set (0)))
                   (child (end ((names (Map (("k" 0)))) (exit 1)))))))
                (next None)))))))))
        (wildcard
         (Some
          (Wildcard
           ((key 1) (ids (Set ())) (child (end ((names (Map ())) (exit 2))))))))
        (check_cases None))))
     (exits
      (((id 0) (bindings ()) (nodes ()))
       ((id 1)
        (bindings ("k"))
        (nodes ((Text " ") (Echo () Fmt_string (`Var "k") Escape) (Text " "))))
       ((id 2) (bindings ()) (nodes ((Text "\n"))))))))
   (Text "\n\nComponent with props\n")
   (Component
    ((0 ((Text " ")))
     (1
      ((Match
        ()
        ((`Var "a_prop"))
        ((tree
          (Wildcard
           ((key 0)
            (ids (Set (0)))
            (child (end ((names (Map (("b_prop" 0)))) (exit 0)))))))
         (exits
          (((id 0)
            (bindings ("b_prop"))
            (nodes
             ((Text " ") (Echo () Fmt_string (`Var "b_prop") Escape) (Text " "))))))))))
     (2 ()))
    "Component"
    (Map
     (("a_prop" (`Var "b_prop"))
      ("c_prop" (`Var "c_prop"))
      ("d_prop" (`Var "e_prop"))
      ("f_prop" (`Var "f_prop"))
      ("g_prop" (`Block 0))
      ("h_prop" (`Block 1))
      ("i_prop" (`Block 2)))))
   (Text "\n\nComponent with implicit children\n")
   (Component ((0 ((Text " ")))) "Component2" (Map (("children" (`Block 0)))))
   (Text "\n\nComponents are only bound once in the instructions.\n")
   (Component ((0 ((Text " ")))) "Component2" (Map (("children" (`Block 0)))))
   (Text "\n\nPatterns\n\nTuple:\n")
   (Match
    ()
    ((`Var "tuple"))
    ((tree
      (Nest
       ((key 0)
        (ids (Set ()))
        (child
         (Int_keys
          (Switch
           ((key 0)
            (ids (Set ()))
            (cases
             ((data (`Int 1))
              (if_match
               (Switch
                ((key 1)
                 (ids (Set ()))
                 (cases
                  ((data (`Float 2.5))
                   (if_match
                    (Switch
                     ((key 2)
                      (ids (Set ()))
                      (cases
                       ((data (`String "a"))
                        (if_match (end (end ((names (Map ())) (exit 0)))))
                        (next None)))
                      (wildcard None)
                      (check_cases None))))
                   (next None)))
                 (wildcard None)
                 (check_cases None))))
              (next None)))
            (wildcard None)
            (check_cases None)))))
        (wildcard (Some (end ((names (Map ())) (exit 1))))))))
     (exits
      (((id 0) (bindings ()) (nodes ((Text " "))))
       ((id 1) (bindings ()) (nodes ((Text " "))))))))
   (Text "\n\nList:\n")
   (Match
    ()
    ((`Var "list"))
    ((tree
      (Nil_or_cons
       ((key 0)
        (ids (Set ()))
        (nil (end ((names (Map ())) (exit 0))))
        (cons
         (Nest
          ((key 0)
           (ids (Set ()))
           (child
            (Int_keys
             (Nil_or_cons
              ((key 0)
               (ids (Set (1)))
               (nil
                (Wildcard
                 ((key 1)
                  (ids (Set (2)))
                  (child
                   (end (end ((names (Map (("_tl" 2) ("_z" 1)))) (exit 2))))))))
               (cons
                (Nest
                 ((key 0)
                  (ids (Set (1)))
                  (child
                   (Int_keys
                    (Wildcard
                     ((key 0)
                      (ids (Set (0)))
                      (child
                       (end
                        (Nil_or_cons
                         ((key 1)
                          (ids (Set (2)))
                          (nil
                           (end
                            (end ((names (Map (("_tl" 2) ("_z" 1)))) (exit 2)))))
                          (cons
                           (Nest
                            ((key 1)
                             (ids (Set (2)))
                             (child
                              (Int_keys
                               (Nil
                                ((key 0)
                                 (ids (Set ()))
                                 (child
                                  (Nil
                                   ((key 1)
                                    (ids (Set ()))
                                    (child
                                     (end
                                      (end
                                       (end ((names (Map (("a" 0)))) (exit 1)))))))))))))
                             (wildcard
                              (Some
                               (end
                                (end
                                 ((names (Map (("_tl" 2) ("_z" 1)))) (exit 2)))))))))))))))))
                  (wildcard None))))))))
           (wildcard None)))))))
     (exits
      (((id 0) (bindings ()) (nodes ((Text "\n"))))
       ((id 1)
        (bindings ("a"))
        (nodes ((Text " ") (Echo () Fmt_string (`Var "a") Escape) (Text "\n"))))
       ((id 2) (bindings ("_tl" "_z")) (nodes ((Text "\n"))))))))
   (Text "\n\nRecord:\n")
   (Match
    ()
    ((`Var "record"))
    ((tree
      (Nest
       ((key 0)
        (ids (Set ()))
        (child
         (String_keys
          (Wildcard
           ((key "!#%@")
            (ids (Set (0)))
            (child
             (Wildcard
              ((key "a")
               (ids (Set (1)))
               (child (end (end ((names (Map (("a" 1) ("b" 0)))) (exit 0))))))))))))
        (wildcard None))))
     (exits
      (((id 0)
        (bindings ("a" "b"))
        (nodes
         ((Text " ")
          (Echo () Fmt_string (`Var "a") Escape)
          (Text " ")
          (Echo () Fmt_string (`Var "b") Escape)
          (Text " "))))))))
   (Text "\n\nEnum:\n")
   (Match
    ()
    ((`Var "enums"))
    ((tree
      (Nest
       ((key 0)
        (ids (Set ()))
        (child
         (Int_keys
          (Switch
           ((key 0)
            (ids (Set ()))
            (cases
             ((data (`String "a"))
              (if_match
               (Switch
                ((key 1)
                 (ids (Set ()))
                 (cases
                  ((data (`Int 1))
                   (if_match
                    (Switch
                     ((key 2)
                      (ids (Set ()))
                      (cases
                       ((data (`Int 1))
                        (if_match
                         (Switch
                          ((key 3)
                           (ids (Set ()))
                           (cases
                            ((data (`Int 0))
                             (if_match (end (end ((names (Map ())) (exit 0)))))
                             (next None)))
                           (wildcard None)
                           (check_cases (Some ((`Int 0) (`Int 1)))))))
                        (next None)))
                      (wildcard None)
                      (check_cases (Some ((`Int 0) (`Int 1)))))))
                   (next None)))
                 (wildcard None)
                 (check_cases None))))
              (next None)))
            (wildcard None)
            (check_cases None)))))
        (wildcard (Some (end ((names (Map ())) (exit 1))))))))
     (exits
      (((id 0) (bindings ()) (nodes ((Text " "))))
       ((id 1) (bindings ()) (nodes ((Text " "))))))))
   (Text "\n\nTagged union:\n")
   (Match
    ()
    ((`Var "tagged"))
    ((tree
      (Nest
       ((key 0)
        (ids (Set ()))
        (child
         (String_keys
          (Switch
           ((key "tag")
            (ids (Set ()))
            (cases
             ((data (`Int 0))
              (if_match (end (end ((names (Map ())) (exit 1)))))
              (next
               (Some
                ((data (`Int 1))
                 (if_match
                  (Wildcard
                   ((key "a")
                    (ids (Set (0)))
                    (child (end (end ((names (Map (("a" 0)))) (exit 0))))))))
                 (next None))))))
            (wildcard None)
            (check_cases (Some ((`Int 0) (`Int 1))))))))
        (wildcard None))))
     (exits
      (((id 0)
        (bindings ("a"))
        (nodes ((Text " ") (Echo () Fmt_string (`Var "a") Escape) (Text " "))))
       ((id 1) (bindings ()) (nodes ((Text "\n"))))))))
   (Text "\n\nDictionary:\n")
   (Match
    ()
    ((`Var "dict"))
    ((tree
      (Nest
       ((key 0)
        (ids (Set ()))
        (child
         (String_keys
          (Optional
           ((child
             (Switch
              ((key "a")
               (ids (Set ()))
               (cases
                ((data (`Int 1))
                 (if_match
                  (Optional
                   ((child
                     (Switch
                      ((key "b")
                       (ids (Set ()))
                       (cases
                        ((data (`Int 2))
                         (if_match (end (end ((names (Map ())) (exit 0)))))
                         (next None)))
                       (wildcard None)
                       (check_cases None))))
                    (next None))))
                 (next None)))
               (wildcard None)
               (check_cases None))))
            (next None)))))
        (wildcard (Some (end ((names (Map ())) (exit 1))))))))
     (exits
      (((id 0) (bindings ()) (nodes ((Text " "))))
       ((id 1) (bindings ()) (nodes ((Text " "))))))))
   (Text "\n\n! and . precedence works correctly\n")
   (Match
    ()
    ((`Array ((`Array ((`Field (`Field (`Var "a") "b") "c"))))))
    ((tree
      (Nil_or_cons
       ((key 0)
        (ids (Set ()))
        (nil (end ((names (Map ())) (exit 1))))
        (cons
         (Nest
          ((key 0)
           (ids (Set ()))
           (child
            (Int_keys
             (Cons
              ((key 0)
               (ids (Set ()))
               (child
                (Nest
                 ((key 0)
                  (ids (Set ()))
                  (child
                   (Int_keys
                    (Switch
                     ((key 0)
                      (ids (Set ()))
                      (cases
                       ((data (`Int 0))
                        (if_match (end (end (end ((names (Map ())) (exit 0))))))
                        (next None)))
                      (wildcard None)
                      (check_cases (Some ((`Int 0) (`Int 1))))))))
                  (wildcard None))))))))
           (wildcard (Some (end ((names (Map ())) (exit 1)))))))))))
     (exits
      (((id 0) (bindings ()) (nodes ())) ((id 1) (bindings ()) (nodes ()))))))
   (Text "\n\nOther syntax features\n\nTrailing commas parse correctly:\n")
   (Match
    ()
    ((`Assoc
      (Map
       (("a" (`Array ((`Int 1) (`Array ((`Int 2) `Null)))))
        ("b" (`Array ((`Int 3) (`Int 4))))
        ("c" (`Assoc (Map (("k" (`Int 5))))))))))
    ((tree
      (Wildcard
       ((key 0) (ids (Set ())) (child (end ((names (Map ())) (exit 0)))))))
     (exits (((id 0) (bindings ()) (nodes ((Text " "))))))))
   (Text "\n\nStrings may contain line breaks:\n")
   (Echo () Fmt_string (`String "a\nb") Escape)
   (Text "\n\nZero-length record fields:\n")
   (Echo () Fmt_string (`Field (`Var "zero") "") Escape)
   (Text "\n")
   (Match
    ()
    ((`Var "zero"))
    ((tree
      (Nest
       ((key 0)
        (ids (Set ()))
        (child
         (String_keys
          (Wildcard
           ((key "")
            (ids (Set (0)))
            (child (end (end ((names (Map (("empty" 0)))) (exit 0)))))))))
        (wildcard None))))
     (exits
      (((id 0)
        (bindings ("empty"))
        (nodes
         ((Text " ") (Echo () Fmt_string (`Var "empty") Escape) (Text " "))))))))
   (Text "\n"))

Print the runtime instructions
  $ acutis template.acutis component.acutis component2.acutis --printinst
  (let$ buffer_add_escape =
   (lambda
    (arg ->
     (return
      (lambda
       (arg ->
        (iter (string_to_seq arg)
         (arg ->
          (match_char arg
           (('&' -> (buffer_add_string arg "&amp;"))
            ('"' -> (buffer_add_string arg "&quot;"))
            ('\'' -> (buffer_add_string arg "&apos;"))
            ('>' -> (buffer_add_string arg "&gt;"))
            ('<' -> (buffer_add_string arg "&lt;"))
            ('/' -> (buffer_add_string arg "&sol;"))
            ('`' -> (buffer_add_string arg "&grave;"))
            ('=' -> (buffer_add_string arg "&equals;"))
            (_ -> (buffer_add_char arg arg))))))))))))
  (let$ buffer_add_sep =
   (lambda
    (arg ->
     (return
      (lambda
       (arg ->
        (return
         (lambda
          (arg ->
           (if_else (not ((buffer_length arg) = 0))
            (then (buffer_add_string arg arg)) (else (unit)))
           (buffer_add_string arg arg))))))))))
  (let$ stack_empty = (lambda (arg -> (unit))))
  (let$ stack_is_empty =
   (lambda
    (arg ->
     (let& result = true)
     (stm (arg @@ (lambda (arg -> (result := false)))))
     (return !result))))
  (let$ stack_add =
   (lambda
    (arg ->
     (return
      (lambda
       (arg ->
        (return (lambda (arg -> (stm (arg @@ arg)) (return (arg @@ arg)))))))))))
  (let$ Component =
   (async_lambda
    (arg ->
     (let$ buf = (buffer_create ())))
     (stm ((buffer_add_escape @@ buf) @@ (prj_string arg.%{"a_prop"})))
     (buffer_add_string buf "\n")
     (stm ((buffer_add_escape @@ buf) @@ (prj_string arg.%{"c_prop"})))
     (buffer_add_string buf "\n")
     (stm ((buffer_add_escape @@ buf) @@ (prj_string arg.%{"d_prop"})))
     (buffer_add_string buf "\n")
     (stm ((buffer_add_escape @@ buf) @@ (prj_string arg.%{"f_prop"})))
     (buffer_add_string buf "\n")
     (stm ((buffer_add_escape @@ buf) @@ (prj_string arg.%{"g_prop"})))
     (buffer_add_string buf "\n")
     (stm ((buffer_add_escape @@ buf) @@ (prj_string arg.%{"h_prop"})))
     (buffer_add_string buf "\n")
     (stm ((buffer_add_escape @@ buf) @@ (prj_string arg.%{"i_prop"})))
     (buffer_add_string buf "\n")
     (return (buffer_contents buf)))))
  (let$ Component2 =
   (async_lambda
    (arg ->
     (let$ buf = (buffer_create ())))
     (stm ((buffer_add_escape @@ buf) @@ (prj_string arg.%{"children"})))
     (buffer_add_string buf "\n")
     (return (buffer_contents buf)))))
  (export
   (async_lambda
    (arg ->
     (let$ errors = (buffer_create ())))
     (let$ error_aux =
      (lambda
       (arg ->
        (return
         (lambda
          (arg ->
           (return
            (lambda
             (arg ->
              (return
               (lambda
                (arg ->
                 (if_else (not ((buffer_length errors) = 0))
                  (then (buffer_add_string errors "\n\n")) (else (unit)))
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
      (lambda
       (arg ->
        (return
         ((error_aux @@ "\nReceived value:\n") @@ (External.to_string arg))))))
     (let$ key_error =
      (lambda
       (arg ->
        (let$ buf = (buffer_create ())))
        (stm (arg @@ ((buffer_add_sep @@ buf) @@ ", ")))
        (return
         ((error_aux @@ "\nInput is missing keys:\n") @@ (buffer_contents buf))))))
     (let$ props = (hashtbl_create ())))
     (let$ type =
      "{\n  a: {b: {c: false | true}},\n  a_prop: string,\n  b_prop: string,\n  c_prop: string,\n  d: string,\n  dict: <int>,\n  e: string,\n  e_prop: string,\n  ech_a: string,\n  ech_b: false | true,\n  ech_d: ?string,\n  ech_e: ?string,\n  ech_f: float,\n  ech_i: int,\n  enums: (@\"a\" | ..., @1 | ..., false | true, false | true),\n  f_prop: string,\n  list: [?string],\n  map_d: <int>,\n  map_l: [int],\n  match_a: int,\n  match_b: string,\n  numbers:\n    {\n      exp1: float,\n      exp2: float,\n      exp3: float,\n      frac: float,\n      int: int,\n      negfrac: float,\n      negint: int\n    },\n  record: {\"!#%@\": string, a: string},\n  tagged: {@tag: false} | {@tag: true, a: string},\n  trim_a: string,\n  trim_b: string,\n  trim_c: string,\n  trim_d: string,\n  trim_e: string,\n  trim_f: string,\n  trim_g: string,\n  tuple: (int, float, string),\n  zero: {\"\": string}\n}")
     (External.decode assoc arg
      (ok
       (decoded ->
        (let& missing_keys = stack_empty)
        (if_else (External.assoc_mem "a" decoded)
         (then
          (let$ input = (External.assoc_find "a" decoded))
          (let$ stack = ((stack_add @@ "a") @@ stack_empty))
          (let$ type = "{b: {c: false | true}}")
          (External.decode assoc input
           (ok
            (decoded ->
             (let$ decoded = (hashtbl_create ())))
             (let& missing_keys = stack_empty)
             (if_else (External.assoc_mem "b" decoded)
              (then
               (let$ input = (External.assoc_find "b" decoded))
               (let$ stack = ((stack_add @@ "b") @@ stack))
               (let$ type = "{c: false | true}")
               (External.decode assoc input
                (ok
                 (decoded ->
                  (let$ decoded = (hashtbl_create ())))
                  (let& missing_keys = stack_empty)
                  (if_else (External.assoc_mem "c" decoded)
                   (then
                    (let$ input = (External.assoc_find "c" decoded))
                    (let$ stack = ((stack_add @@ "c") @@ stack))
                    (let$ type = "false | true")
                    (External.decode bool input
                     (ok
                      (decoded ->
                       (if_else decoded (then (decoded.%{"c"} <- (inj_int 1)))
                        (else (decoded.%{"c"} <- (inj_int 0))))))
                     (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
                   (else
                    (missing_keys := ((stack_add @@ "c") @@ !missing_keys))))
                  (if_else (not (stack_is_empty @@ !missing_keys))
                   (then
                    (stm (((key_error @@ !missing_keys) @@ stack) @@ type)))
                   (else (unit)))
                  (decoded.%{"b"} <- (inj_hashtbl decoded))))
                (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
              (else (missing_keys := ((stack_add @@ "b") @@ !missing_keys))))
             (if_else (not (stack_is_empty @@ !missing_keys))
              (then (stm (((key_error @@ !missing_keys) @@ stack) @@ type)))
              (else (unit)))
             (props.%{"a"} <- (inj_hashtbl decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "a") @@ !missing_keys))))
        (if_else (External.assoc_mem "a_prop" decoded)
         (then
          (let$ input = (External.assoc_find "a_prop" decoded))
          (let$ stack = ((stack_add @@ "a_prop") @@ stack_empty))
          (let$ type = "string")
          (External.decode string input
           (ok (decoded -> (props.%{"a_prop"} <- (inj_string decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "a_prop") @@ !missing_keys))))
        (if_else (External.assoc_mem "b_prop" decoded)
         (then
          (let$ input = (External.assoc_find "b_prop" decoded))
          (let$ stack = ((stack_add @@ "b_prop") @@ stack_empty))
          (let$ type = "string")
          (External.decode string input
           (ok (decoded -> (props.%{"b_prop"} <- (inj_string decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "b_prop") @@ !missing_keys))))
        (if_else (External.assoc_mem "c_prop" decoded)
         (then
          (let$ input = (External.assoc_find "c_prop" decoded))
          (let$ stack = ((stack_add @@ "c_prop") @@ stack_empty))
          (let$ type = "string")
          (External.decode string input
           (ok (decoded -> (props.%{"c_prop"} <- (inj_string decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "c_prop") @@ !missing_keys))))
        (if_else (External.assoc_mem "d" decoded)
         (then
          (let$ input = (External.assoc_find "d" decoded))
          (let$ stack = ((stack_add @@ "d") @@ stack_empty))
          (let$ type = "string")
          (External.decode string input
           (ok (decoded -> (props.%{"d"} <- (inj_string decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "d") @@ !missing_keys))))
        (if_else (External.assoc_mem "dict" decoded)
         (then
          (let$ input = (External.assoc_find "dict" decoded))
          (let$ stack = ((stack_add @@ "dict") @@ stack_empty))
          (let$ type = "<int>")
          (External.decode assoc input
           (ok
            (decoded ->
             (let$ decoded = (hashtbl_create ())))
             (iter (External.assoc_to_seq decoded)
              (arg ->
               (let$ stack = ((stack_add @@ (fst arg)) @@ stack))
               (let$ type = "int")
               (External.decode int (snd arg)
                (ok (decoded -> (decoded.%{(fst arg)} <- (inj_int decoded))))
                (error (stm (((decode_error @@ (snd arg)) @@ stack) @@ type))))
               (props.%{"dict"} <- (inj_hashtbl decoded))))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "dict") @@ !missing_keys))))
        (if_else (External.assoc_mem "e" decoded)
         (then
          (let$ input = (External.assoc_find "e" decoded))
          (let$ stack = ((stack_add @@ "e") @@ stack_empty))
          (let$ type = "string")
          (External.decode string input
           (ok (decoded -> (props.%{"e"} <- (inj_string decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "e") @@ !missing_keys))))
        (if_else (External.assoc_mem "e_prop" decoded)
         (then
          (let$ input = (External.assoc_find "e_prop" decoded))
          (let$ stack = ((stack_add @@ "e_prop") @@ stack_empty))
          (let$ type = "string")
          (External.decode string input
           (ok (decoded -> (props.%{"e_prop"} <- (inj_string decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "e_prop") @@ !missing_keys))))
        (if_else (External.assoc_mem "ech_a" decoded)
         (then
          (let$ input = (External.assoc_find "ech_a" decoded))
          (let$ stack = ((stack_add @@ "ech_a") @@ stack_empty))
          (let$ type = "string")
          (External.decode string input
           (ok (decoded -> (props.%{"ech_a"} <- (inj_string decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "ech_a") @@ !missing_keys))))
        (if_else (External.assoc_mem "ech_b" decoded)
         (then
          (let$ input = (External.assoc_find "ech_b" decoded))
          (let$ stack = ((stack_add @@ "ech_b") @@ stack_empty))
          (let$ type = "false | true")
          (External.decode bool input
           (ok
            (decoded ->
             (if_else decoded (then (props.%{"ech_b"} <- (inj_int 1)))
              (else (props.%{"ech_b"} <- (inj_int 0))))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "ech_b") @@ !missing_keys))))
        (if_else (External.assoc_mem "ech_d" decoded)
         (then
          (let$ input = (External.assoc_find "ech_d" decoded))
          (let$ stack = ((stack_add @@ "ech_d") @@ stack_empty))
          (let$ type = "?string")
          (External.decode some input
           (ok
            (decoded ->
             (let$ decoded = [(inj_int 0)])
             (let$ stack = ((stack_add @@ "<nullable>") @@ stack))
             (let$ type = "string")
             (External.decode string decoded
              (ok (decoded -> (decoded.%(0) <- (inj_string decoded))))
              (error (stm (((decode_error @@ decoded) @@ stack) @@ type))))
             (props.%{"ech_d"} <- (inj_array decoded))))
           (error (props.%{"ech_d"} <- (inj_int 0)))))
         (else (props.%{"ech_d"} <- (inj_int 0))))
        (if_else (External.assoc_mem "ech_e" decoded)
         (then
          (let$ input = (External.assoc_find "ech_e" decoded))
          (let$ stack = ((stack_add @@ "ech_e") @@ stack_empty))
          (let$ type = "?string")
          (External.decode some input
           (ok
            (decoded ->
             (let$ decoded = [(inj_int 0)])
             (let$ stack = ((stack_add @@ "<nullable>") @@ stack))
             (let$ type = "string")
             (External.decode string decoded
              (ok (decoded -> (decoded.%(0) <- (inj_string decoded))))
              (error (stm (((decode_error @@ decoded) @@ stack) @@ type))))
             (props.%{"ech_e"} <- (inj_array decoded))))
           (error (props.%{"ech_e"} <- (inj_int 0)))))
         (else (props.%{"ech_e"} <- (inj_int 0))))
        (if_else (External.assoc_mem "ech_f" decoded)
         (then
          (let$ input = (External.assoc_find "ech_f" decoded))
          (let$ stack = ((stack_add @@ "ech_f") @@ stack_empty))
          (let$ type = "float")
          (External.decode float input
           (ok (decoded -> (props.%{"ech_f"} <- (inj_float decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "ech_f") @@ !missing_keys))))
        (if_else (External.assoc_mem "ech_i" decoded)
         (then
          (let$ input = (External.assoc_find "ech_i" decoded))
          (let$ stack = ((stack_add @@ "ech_i") @@ stack_empty))
          (let$ type = "int")
          (External.decode int input
           (ok (decoded -> (props.%{"ech_i"} <- (inj_int decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "ech_i") @@ !missing_keys))))
        (if_else (External.assoc_mem "enums" decoded)
         (then
          (let$ input = (External.assoc_find "enums" decoded))
          (let$ stack = ((stack_add @@ "enums") @@ stack_empty))
          (let$ type = "(@\"a\" | ..., @1 | ..., false | true, false | true)")
          (External.decode seq input
           (ok
            (decoded ->
             (let$ decoded = (array_make 4 (inj_int 0)))
             (uncons decoded
              (nil (stm (((decode_error @@ input) @@ stack) @@ type)))
              (cons
               (hd ->
                (seq ->
                 (let$ stack = ((stack_add @@ (string_of_int 0)) @@ stack))
                 (let$ type = "@\"a\" | ...")
                 (External.decode string hd
                  (ok (decoded -> (decoded.%(0) <- (inj_string decoded))))
                  (error (stm (((decode_error @@ hd) @@ stack) @@ type))))
                 (uncons seq
                  (nil (stm (((decode_error @@ input) @@ stack) @@ type)))
                  (cons
                   (hd ->
                    (seq ->
                     (let$ stack = ((stack_add @@ (string_of_int 1)) @@ stack))
                     (let$ type = "@1 | ...")
                     (External.decode int hd
                      (ok (decoded -> (decoded.%(1) <- (inj_int decoded))))
                      (error (stm (((decode_error @@ hd) @@ stack) @@ type))))
                     (uncons seq
                      (nil (stm (((decode_error @@ input) @@ stack) @@ type)))
                      (cons
                       (hd ->
                        (seq ->
                         (let$ stack =
                          ((stack_add @@ (string_of_int 2)) @@ stack))
                         (let$ type = "false | true")
                         (External.decode bool hd
                          (ok
                           (decoded ->
                            (if_else decoded
                             (then (decoded.%(2) <- (inj_int 1)))
                             (else (decoded.%(2) <- (inj_int 0))))))
                          (error
                           (stm (((decode_error @@ hd) @@ stack) @@ type))))
                         (uncons seq
                          (nil
                           (stm (((decode_error @@ input) @@ stack) @@ type)))
                          (cons
                           (hd ->
                            (seq ->
                             (let$ stack =
                              ((stack_add @@ (string_of_int 3)) @@ stack))
                             (let$ type = "false | true")
                             (External.decode bool hd
                              (ok
                               (decoded ->
                                (if_else decoded
                                 (then (decoded.%(3) <- (inj_int 1)))
                                 (else (decoded.%(3) <- (inj_int 0))))))
                              (error
                               (stm (((decode_error @@ hd) @@ stack) @@ type))))
                             (unit)))))))))))))))))
             (props.%{"enums"} <- (inj_array decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "enums") @@ !missing_keys))))
        (if_else (External.assoc_mem "f_prop" decoded)
         (then
          (let$ input = (External.assoc_find "f_prop" decoded))
          (let$ stack = ((stack_add @@ "f_prop") @@ stack_empty))
          (let$ type = "string")
          (External.decode string input
           (ok (decoded -> (props.%{"f_prop"} <- (inj_string decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "f_prop") @@ !missing_keys))))
        (if_else (External.assoc_mem "list" decoded)
         (then
          (let$ input = (External.assoc_find "list" decoded))
          (let$ stack = ((stack_add @@ "list") @@ stack_empty))
          (let$ type = "[?string]")
          (External.decode seq input
           (ok
            (decoded ->
             (let& index = 0)
             (let$ decoded = [(inj_int 0), (inj_int 0)])
             (let& decode_dst = decoded)
             (iter decoded
              (arg ->
               (let$ decode_dst_new = [(inj_int 0), (inj_int 0)])
               (let$ stack = ((stack_add @@ (string_of_int !index)) @@ stack))
               (let$ type = "?string")
               (External.decode some arg
                (ok
                 (decoded ->
                  (let$ decoded = [(inj_int 0)])
                  (let$ stack = ((stack_add @@ "<nullable>") @@ stack))
                  (let$ type = "string")
                  (External.decode string decoded
                   (ok (decoded -> (decoded.%(0) <- (inj_string decoded))))
                   (error (stm (((decode_error @@ decoded) @@ stack) @@ type))))
                  (decode_dst_new.%(0) <- (inj_array decoded))))
                (error (decode_dst_new.%(0) <- (inj_int 0))))
               (!decode_dst.%(1) <- (inj_array decode_dst_new))
               (incr index)
               (decode_dst := decode_dst_new)))
             (props.%{"list"} <- decoded.%(1))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "list") @@ !missing_keys))))
        (if_else (External.assoc_mem "map_d" decoded)
         (then
          (let$ input = (External.assoc_find "map_d" decoded))
          (let$ stack = ((stack_add @@ "map_d") @@ stack_empty))
          (let$ type = "<int>")
          (External.decode assoc input
           (ok
            (decoded ->
             (let$ decoded = (hashtbl_create ())))
             (iter (External.assoc_to_seq decoded)
              (arg ->
               (let$ stack = ((stack_add @@ (fst arg)) @@ stack))
               (let$ type = "int")
               (External.decode int (snd arg)
                (ok (decoded -> (decoded.%{(fst arg)} <- (inj_int decoded))))
                (error (stm (((decode_error @@ (snd arg)) @@ stack) @@ type))))
               (props.%{"map_d"} <- (inj_hashtbl decoded))))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "map_d") @@ !missing_keys))))
        (if_else (External.assoc_mem "map_l" decoded)
         (then
          (let$ input = (External.assoc_find "map_l" decoded))
          (let$ stack = ((stack_add @@ "map_l") @@ stack_empty))
          (let$ type = "[int]")
          (External.decode seq input
           (ok
            (decoded ->
             (let& index = 0)
             (let$ decoded = [(inj_int 0), (inj_int 0)])
             (let& decode_dst = decoded)
             (iter decoded
              (arg ->
               (let$ decode_dst_new = [(inj_int 0), (inj_int 0)])
               (let$ stack = ((stack_add @@ (string_of_int !index)) @@ stack))
               (let$ type = "int")
               (External.decode int arg
                (ok (decoded -> (decode_dst_new.%(0) <- (inj_int decoded))))
                (error (stm (((decode_error @@ arg) @@ stack) @@ type))))
               (!decode_dst.%(1) <- (inj_array decode_dst_new))
               (incr index)
               (decode_dst := decode_dst_new)))
             (props.%{"map_l"} <- decoded.%(1))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "map_l") @@ !missing_keys))))
        (if_else (External.assoc_mem "match_a" decoded)
         (then
          (let$ input = (External.assoc_find "match_a" decoded))
          (let$ stack = ((stack_add @@ "match_a") @@ stack_empty))
          (let$ type = "int")
          (External.decode int input
           (ok (decoded -> (props.%{"match_a"} <- (inj_int decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "match_a") @@ !missing_keys))))
        (if_else (External.assoc_mem "match_b" decoded)
         (then
          (let$ input = (External.assoc_find "match_b" decoded))
          (let$ stack = ((stack_add @@ "match_b") @@ stack_empty))
          (let$ type = "string")
          (External.decode string input
           (ok (decoded -> (props.%{"match_b"} <- (inj_string decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "match_b") @@ !missing_keys))))
        (if_else (External.assoc_mem "numbers" decoded)
         (then
          (let$ input = (External.assoc_find "numbers" decoded))
          (let$ stack = ((stack_add @@ "numbers") @@ stack_empty))
          (let$ type =
           "{\n  exp1: float,\n  exp2: float,\n  exp3: float,\n  frac: float,\n  int: int,\n  negfrac: float,\n  negint: int\n}")
          (External.decode assoc input
           (ok
            (decoded ->
             (let$ decoded = (hashtbl_create ())))
             (let& missing_keys = stack_empty)
             (if_else (External.assoc_mem "exp1" decoded)
              (then
               (let$ input = (External.assoc_find "exp1" decoded))
               (let$ stack = ((stack_add @@ "exp1") @@ stack))
               (let$ type = "float")
               (External.decode float input
                (ok (decoded -> (decoded.%{"exp1"} <- (inj_float decoded))))
                (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
              (else (missing_keys := ((stack_add @@ "exp1") @@ !missing_keys))))
             (if_else (External.assoc_mem "exp2" decoded)
              (then
               (let$ input = (External.assoc_find "exp2" decoded))
               (let$ stack = ((stack_add @@ "exp2") @@ stack))
               (let$ type = "float")
               (External.decode float input
                (ok (decoded -> (decoded.%{"exp2"} <- (inj_float decoded))))
                (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
              (else (missing_keys := ((stack_add @@ "exp2") @@ !missing_keys))))
             (if_else (External.assoc_mem "exp3" decoded)
              (then
               (let$ input = (External.assoc_find "exp3" decoded))
               (let$ stack = ((stack_add @@ "exp3") @@ stack))
               (let$ type = "float")
               (External.decode float input
                (ok (decoded -> (decoded.%{"exp3"} <- (inj_float decoded))))
                (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
              (else (missing_keys := ((stack_add @@ "exp3") @@ !missing_keys))))
             (if_else (External.assoc_mem "frac" decoded)
              (then
               (let$ input = (External.assoc_find "frac" decoded))
               (let$ stack = ((stack_add @@ "frac") @@ stack))
               (let$ type = "float")
               (External.decode float input
                (ok (decoded -> (decoded.%{"frac"} <- (inj_float decoded))))
                (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
              (else (missing_keys := ((stack_add @@ "frac") @@ !missing_keys))))
             (if_else (External.assoc_mem "int" decoded)
              (then
               (let$ input = (External.assoc_find "int" decoded))
               (let$ stack = ((stack_add @@ "int") @@ stack))
               (let$ type = "int")
               (External.decode int input
                (ok (decoded -> (decoded.%{"int"} <- (inj_int decoded))))
                (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
              (else (missing_keys := ((stack_add @@ "int") @@ !missing_keys))))
             (if_else (External.assoc_mem "negfrac" decoded)
              (then
               (let$ input = (External.assoc_find "negfrac" decoded))
               (let$ stack = ((stack_add @@ "negfrac") @@ stack))
               (let$ type = "float")
               (External.decode float input
                (ok (decoded -> (decoded.%{"negfrac"} <- (inj_float decoded))))
                (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
              (else
               (missing_keys := ((stack_add @@ "negfrac") @@ !missing_keys))))
             (if_else (External.assoc_mem "negint" decoded)
              (then
               (let$ input = (External.assoc_find "negint" decoded))
               (let$ stack = ((stack_add @@ "negint") @@ stack))
               (let$ type = "int")
               (External.decode int input
                (ok (decoded -> (decoded.%{"negint"} <- (inj_int decoded))))
                (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
              (else
               (missing_keys := ((stack_add @@ "negint") @@ !missing_keys))))
             (if_else (not (stack_is_empty @@ !missing_keys))
              (then (stm (((key_error @@ !missing_keys) @@ stack) @@ type)))
              (else (unit)))
             (props.%{"numbers"} <- (inj_hashtbl decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "numbers") @@ !missing_keys))))
        (if_else (External.assoc_mem "record" decoded)
         (then
          (let$ input = (External.assoc_find "record" decoded))
          (let$ stack = ((stack_add @@ "record") @@ stack_empty))
          (let$ type = "{\"!#%@\": string, a: string}")
          (External.decode assoc input
           (ok
            (decoded ->
             (let$ decoded = (hashtbl_create ())))
             (let& missing_keys = stack_empty)
             (if_else (External.assoc_mem "!#%@" decoded)
              (then
               (let$ input = (External.assoc_find "!#%@" decoded))
               (let$ stack = ((stack_add @@ "!#%@") @@ stack))
               (let$ type = "string")
               (External.decode string input
                (ok (decoded -> (decoded.%{"!#%@"} <- (inj_string decoded))))
                (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
              (else (missing_keys := ((stack_add @@ "!#%@") @@ !missing_keys))))
             (if_else (External.assoc_mem "a" decoded)
              (then
               (let$ input = (External.assoc_find "a" decoded))
               (let$ stack = ((stack_add @@ "a") @@ stack))
               (let$ type = "string")
               (External.decode string input
                (ok (decoded -> (decoded.%{"a"} <- (inj_string decoded))))
                (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
              (else (missing_keys := ((stack_add @@ "a") @@ !missing_keys))))
             (if_else (not (stack_is_empty @@ !missing_keys))
              (then (stm (((key_error @@ !missing_keys) @@ stack) @@ type)))
              (else (unit)))
             (props.%{"record"} <- (inj_hashtbl decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "record") @@ !missing_keys))))
        (if_else (External.assoc_mem "tagged" decoded)
         (then
          (let$ input = (External.assoc_find "tagged" decoded))
          (let$ stack = ((stack_add @@ "tagged") @@ stack_empty))
          (let$ type = "{@tag: false} | {@tag: true, a: string}")
          (External.decode assoc input
           (ok
            (decoded ->
             (if_else (External.assoc_mem "tag" decoded)
              (then
               (External.decode bool (External.assoc_find "tag" decoded)
                (ok
                 (decoded ->
                  (let$ decoded = (hashtbl_create ())))
                  (if_else (not decoded)
                   (then
                    (decoded.%{"tag"} <- (inj_int 0))
                    (let& missing_keys = stack_empty)
                    (unit)
                    (if_else (not (stack_is_empty @@ !missing_keys))
                     (then
                      (stm (((key_error @@ !missing_keys) @@ stack) @@ type)))
                     (else (unit))))
                   (else
                    (if_else decoded
                     (then
                      (decoded.%{"tag"} <- (inj_int 1))
                      (let& missing_keys = stack_empty)
                      (if_else (External.assoc_mem "a" decoded)
                       (then
                        (let$ input = (External.assoc_find "a" decoded))
                        (let$ stack = ((stack_add @@ "a") @@ stack))
                        (let$ type = "string")
                        (External.decode string input
                         (ok
                          (decoded -> (decoded.%{"a"} <- (inj_string decoded))))
                         (error
                          (stm (((decode_error @@ input) @@ stack) @@ type)))))
                       (else
                        (missing_keys := ((stack_add @@ "a") @@ !missing_keys))))
                      (if_else (not (stack_is_empty @@ !missing_keys))
                       (then
                        (stm (((key_error @@ !missing_keys) @@ stack) @@ type)))
                       (else (unit))))
                     (else (stm (((decode_error @@ input) @@ stack) @@ type))))))
                  (props.%{"tagged"} <- (inj_hashtbl decoded))))
                (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
              (else (stm (((decode_error @@ input) @@ stack) @@ type))))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "tagged") @@ !missing_keys))))
        (if_else (External.assoc_mem "trim_a" decoded)
         (then
          (let$ input = (External.assoc_find "trim_a" decoded))
          (let$ stack = ((stack_add @@ "trim_a") @@ stack_empty))
          (let$ type = "string")
          (External.decode string input
           (ok (decoded -> (props.%{"trim_a"} <- (inj_string decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "trim_a") @@ !missing_keys))))
        (if_else (External.assoc_mem "trim_b" decoded)
         (then
          (let$ input = (External.assoc_find "trim_b" decoded))
          (let$ stack = ((stack_add @@ "trim_b") @@ stack_empty))
          (let$ type = "string")
          (External.decode string input
           (ok (decoded -> (props.%{"trim_b"} <- (inj_string decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "trim_b") @@ !missing_keys))))
        (if_else (External.assoc_mem "trim_c" decoded)
         (then
          (let$ input = (External.assoc_find "trim_c" decoded))
          (let$ stack = ((stack_add @@ "trim_c") @@ stack_empty))
          (let$ type = "string")
          (External.decode string input
           (ok (decoded -> (props.%{"trim_c"} <- (inj_string decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "trim_c") @@ !missing_keys))))
        (if_else (External.assoc_mem "trim_d" decoded)
         (then
          (let$ input = (External.assoc_find "trim_d" decoded))
          (let$ stack = ((stack_add @@ "trim_d") @@ stack_empty))
          (let$ type = "string")
          (External.decode string input
           (ok (decoded -> (props.%{"trim_d"} <- (inj_string decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "trim_d") @@ !missing_keys))))
        (if_else (External.assoc_mem "trim_e" decoded)
         (then
          (let$ input = (External.assoc_find "trim_e" decoded))
          (let$ stack = ((stack_add @@ "trim_e") @@ stack_empty))
          (let$ type = "string")
          (External.decode string input
           (ok (decoded -> (props.%{"trim_e"} <- (inj_string decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "trim_e") @@ !missing_keys))))
        (if_else (External.assoc_mem "trim_f" decoded)
         (then
          (let$ input = (External.assoc_find "trim_f" decoded))
          (let$ stack = ((stack_add @@ "trim_f") @@ stack_empty))
          (let$ type = "string")
          (External.decode string input
           (ok (decoded -> (props.%{"trim_f"} <- (inj_string decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "trim_f") @@ !missing_keys))))
        (if_else (External.assoc_mem "trim_g" decoded)
         (then
          (let$ input = (External.assoc_find "trim_g" decoded))
          (let$ stack = ((stack_add @@ "trim_g") @@ stack_empty))
          (let$ type = "string")
          (External.decode string input
           (ok (decoded -> (props.%{"trim_g"} <- (inj_string decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "trim_g") @@ !missing_keys))))
        (if_else (External.assoc_mem "tuple" decoded)
         (then
          (let$ input = (External.assoc_find "tuple" decoded))
          (let$ stack = ((stack_add @@ "tuple") @@ stack_empty))
          (let$ type = "(int, float, string)")
          (External.decode seq input
           (ok
            (decoded ->
             (let$ decoded = (array_make 3 (inj_int 0)))
             (uncons decoded
              (nil (stm (((decode_error @@ input) @@ stack) @@ type)))
              (cons
               (hd ->
                (seq ->
                 (let$ stack = ((stack_add @@ (string_of_int 0)) @@ stack))
                 (let$ type = "int")
                 (External.decode int hd
                  (ok (decoded -> (decoded.%(0) <- (inj_int decoded))))
                  (error (stm (((decode_error @@ hd) @@ stack) @@ type))))
                 (uncons seq
                  (nil (stm (((decode_error @@ input) @@ stack) @@ type)))
                  (cons
                   (hd ->
                    (seq ->
                     (let$ stack = ((stack_add @@ (string_of_int 1)) @@ stack))
                     (let$ type = "float")
                     (External.decode float hd
                      (ok (decoded -> (decoded.%(1) <- (inj_float decoded))))
                      (error (stm (((decode_error @@ hd) @@ stack) @@ type))))
                     (uncons seq
                      (nil (stm (((decode_error @@ input) @@ stack) @@ type)))
                      (cons
                       (hd ->
                        (seq ->
                         (let$ stack =
                          ((stack_add @@ (string_of_int 2)) @@ stack))
                         (let$ type = "string")
                         (External.decode string hd
                          (ok
                           (decoded -> (decoded.%(2) <- (inj_string decoded))))
                          (error
                           (stm (((decode_error @@ hd) @@ stack) @@ type))))
                         (unit)))))))))))))
             (props.%{"tuple"} <- (inj_array decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "tuple") @@ !missing_keys))))
        (if_else (External.assoc_mem "zero" decoded)
         (then
          (let$ input = (External.assoc_find "zero" decoded))
          (let$ stack = ((stack_add @@ "zero") @@ stack_empty))
          (let$ type = "{\"\": string}")
          (External.decode assoc input
           (ok
            (decoded ->
             (let$ decoded = (hashtbl_create ())))
             (let& missing_keys = stack_empty)
             (if_else (External.assoc_mem "" decoded)
              (then
               (let$ input = (External.assoc_find "" decoded))
               (let$ stack = ((stack_add @@ "") @@ stack))
               (let$ type = "string")
               (External.decode string input
                (ok (decoded -> (decoded.%{""} <- (inj_string decoded))))
                (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
              (else (missing_keys := ((stack_add @@ "") @@ !missing_keys))))
             (if_else (not (stack_is_empty @@ !missing_keys))
              (then (stm (((key_error @@ !missing_keys) @@ stack) @@ type)))
              (else (unit)))
             (props.%{"zero"} <- (inj_hashtbl decoded))))
           (error (stm (((decode_error @@ input) @@ stack) @@ type)))))
         (else (missing_keys := ((stack_add @@ "zero") @@ !missing_keys))))
        (if_else (not (stack_is_empty @@ !missing_keys))
         (then (stm (((key_error @@ !missing_keys) @@ stack_empty) @@ type)))
         (else (unit)))))
      (error (stm (((decode_error @@ arg) @@ stack_empty) @@ type))))
     (if_else ((buffer_length errors) = 0)
      (then
       (let$ buf = (buffer_create ())))
       (buffer_add_string buf "Echoes\n")
       (stm ((buffer_add_escape @@ buf) @@ (prj_string props.%{"ech_a"})))
       (buffer_add_string buf " ")
       (stm ((buffer_add_escape @@ buf) @@ "b"))
       (buffer_add_string buf " ")
       (let$ nullable = props.%{"ech_d"})
       (if_else (test_int nullable)
        (then
         (let$ nullable = props.%{"ech_e"})
         (if_else (test_int nullable) (then (buffer_add_string buf "f\"g"))
          (else (buffer_add_string buf (prj_string (prj_array nullable).%(0))))))
        (else (buffer_add_string buf (prj_string (prj_array nullable).%(0)))))
       (buffer_add_string buf "\n")
       (stm
        ((buffer_add_escape @@ buf) @@
         (string_of_int (prj_int props.%{"ech_i"}))))
       (buffer_add_string buf " ")
       (stm
        ((buffer_add_escape @@ buf) @@
         (string_of_float (prj_float props.%{"ech_f"}))))
       (buffer_add_string buf " ")
       (stm
        ((buffer_add_escape @@ buf) @@
         (string_of_bool (not ((prj_int props.%{"ech_b"}) = 0)))))
       (buffer_add_string buf "\n\nNumbers\n")
       (let$ arg_match = [props.%{"numbers"}])
       (let& exit = -1)
       (let$ match_arg = arg_match.%(0))
       (let$ match_arg = (prj_hashtbl match_arg).%{"exp1"})
       (if_else ((prj_float match_arg) = 150)
        (then
         (let$ match_arg = (prj_hashtbl match_arg).%{"exp2"})
         (if_else ((prj_float match_arg) = -1000)
          (then
           (let$ match_arg = (prj_hashtbl match_arg).%{"exp3"})
           (if_else ((prj_float match_arg) = 0.2)
            (then
             (let$ match_arg = (prj_hashtbl match_arg).%{"frac"})
             (if_else ((prj_float match_arg) = 10.55)
              (then
               (let$ match_arg = (prj_hashtbl match_arg).%{"int"})
               (if_else ((prj_int match_arg) = 1000)
                (then
                 (let$ match_arg = (prj_hashtbl match_arg).%{"negfrac"})
                 (if_else ((prj_float match_arg) = -12.34)
                  (then
                   (let$ match_arg = (prj_hashtbl match_arg).%{"negint"})
                   (if_else ((prj_int match_arg) = -999)
                    (then (unit) (exit := 0)) (else (unit))))
                  (else (unit))))
                (else (unit))))
              (else (unit))))
            (else (unit))))
          (else (unit))))
        (else (unit)))
       (if_else (!exit = -1) (then (unit) (exit := 1)) (else (unit)))
       (if_else (!exit = 0) (then (unit)) (else (unit)))
       (buffer_add_string buf "\n\nTrim")
       (stm ((buffer_add_escape @@ buf) @@ (prj_string props.%{"trim_a"})))
       (stm ((buffer_add_escape @@ buf) @@ (prj_string props.%{"trim_b"})))
       (buffer_add_string buf " ")
       (stm ((buffer_add_escape @@ buf) @@ (prj_string props.%{"trim_c"})))
       (stm ((buffer_add_escape @@ buf) @@ (prj_string props.%{"trim_d"})))
       (buffer_add_string buf (prj_string props.%{"trim_e"}))
       (buffer_add_string buf "\n")
       (buffer_add_string buf (prj_string props.%{"trim_f"}))
       (buffer_add_string buf (prj_string props.%{"trim_g"}))
       (buffer_add_string buf "Comments\na ")
       (buffer_add_string buf "b")
       (buffer_add_string buf " c\n\nFlat match\n")
       (let$ arg_match = [props.%{"match_a"}])
       (let& exit = -1)
       (let$ match_arg = arg_match.%(0))
       (if_else ((prj_int match_arg) = 1) (then (unit) (exit := 0))
        (else
         (if_else ((prj_int match_arg) = 2) (then (unit) (exit := 0))
          (else
           (if_else ((prj_int match_arg) = 3) (then (unit) (exit := 1))
            (else (unit) (exit := 2)))))))
       (if_else (!exit = 0) (then (unit))
        (else
         (if_else (!exit = 1) (then (buffer_add_string buf " "))
          (else (buffer_add_string buf " ")))))
       (buffer_add_string buf "\n\nNested match\n")
       (let$ arg_match = [props.%{"match_b"}])
       (let$ match_props = (hashtbl_create ())))
       (let& exit = -1)
       (let$ match_arg = arg_match.%(0))
       (match_props.%{"c"} <- match_arg)
       (exit := 0)
       (buffer_add_string buf "\n  ")
       (let$ arg_match = [props.%{"d"}, props.%{"e"}])
       (let$ match_props = (hashtbl_create ())))
       (let& exit = -1)
       (let$ match_arg = arg_match.%(0))
       (let$ match_arg = arg_match.%(1))
       (match_props.%{"f"} <- match_arg)
       (match_props.%{"g"} <- match_arg)
       (exit := 0)
       (buffer_add_string buf " ")
       (stm ((buffer_add_escape @@ buf) @@ (prj_string match_props.%{"c"})))
       (buffer_add_string buf " ")
       (stm ((buffer_add_escape @@ buf) @@ (prj_string match_props.%{"f"})))
       (buffer_add_string buf " ")
       (stm ((buffer_add_escape @@ buf) @@ (prj_string match_props.%{"g"})))
       (buffer_add_string buf " ")
       (buffer_add_string buf "\n")
       (buffer_add_string buf "\n\nMap list\n")
       (let& index = 0)
       (let& cell = props.%{"map_l"})
       (while (not (test_int !cell))
        ((let$ match_props = (hashtbl_create ())))
         (let$ list = (prj_array !cell))
         (let$ head = list.%(0))
         (let& exit = -1)
         (if_else ((prj_int head) = 1) (then (unit) (exit := 0))
          (else
           (if_else ((prj_int head) = 2) (then (unit) (exit := 0))
            (else
             (if_else ((prj_int head) = 3)
              (then (match_props.%{"i"} <- (inj_int !index)) (exit := 1))
              (else (unit) (exit := 2)))))))
         (if_else (!exit = 0) (then (unit))
          (else
           (if_else (!exit = 1)
            (then
             (buffer_add_string buf " ")
             (stm
              ((buffer_add_escape @@ buf) @@
               (string_of_int (prj_int match_props.%{"i"}))))
             (buffer_add_string buf " "))
            (else (buffer_add_string buf " ")))))
         (incr index)
         (cell := list.%(1))))
       (buffer_add_string buf "\n\nMap dict\n")
       (let$ match_arg = props.%{"map_d"})
       (iter (hashtbl_to_seq (prj_hashtbl match_arg))
        (arg ->
         (let$ match_props = (hashtbl_create ())))
         (let& exit = -1)
         (if_else ((prj_int (snd arg)) = 1) (then (unit) (exit := 0))
          (else
           (if_else ((prj_int (snd arg)) = 2) (then (unit) (exit := 0))
            (else
             (if_else ((prj_int (snd arg)) = 3)
              (then (match_props.%{"k"} <- (inj_string (fst arg))) (exit := 1))
              (else (unit) (exit := 2)))))))
         (if_else (!exit = 0) (then (unit))
          (else
           (if_else (!exit = 1)
            (then
             (buffer_add_string buf " ")
             (stm
              ((buffer_add_escape @@ buf) @@ (prj_string match_props.%{"k"})))
             (buffer_add_string buf " "))
            (else (buffer_add_string buf "\n")))))))
       (buffer_add_string buf "\n\nComponent with props\n")
       (let$ buf = (buffer_create ())))
       (buffer_add_string buf " ")
       (let$ buf = (buffer_create ())))
       (let$ arg_match = [props.%{"a_prop"}])
       (let$ match_props = (hashtbl_create ())))
       (let& exit = -1)
       (let$ match_arg = arg_match.%(0))
       (match_props.%{"b_prop"} <- match_arg)
       (exit := 0)
       (buffer_add_string buf " ")
       (stm
        ((buffer_add_escape @@ buf) @@ (prj_string match_props.%{"b_prop"})))
       (buffer_add_string buf " ")
       (let$ buf = (buffer_create ())))
       (unit)
       (buffer_add_string buf
        (await
         (Component @@
          (hashtbl
           (("a_prop", props.%{"b_prop"}),
            ("c_prop", props.%{"c_prop"}),
            ("d_prop", props.%{"e_prop"}),
            ("f_prop", props.%{"f_prop"}),
            ("g_prop", (inj_string (buffer_contents buf))),
            ("h_prop", (inj_string (buffer_contents buf))),
            ("i_prop", (inj_string (buffer_contents buf))))))))
       (buffer_add_string buf "\n\nComponent with implicit children\n")
       (let$ buf = (buffer_create ())))
       (buffer_add_string buf " ")
       (buffer_add_string buf
        (await
         (Component2 @@
          (hashtbl (("children", (inj_string (buffer_contents buf))))))))
       (buffer_add_string buf
        "\n\nComponents are only bound once in the instructions.\n")
       (let$ buf = (buffer_create ())))
       (buffer_add_string buf " ")
       (buffer_add_string buf
        (await
         (Component2 @@
          (hashtbl (("children", (inj_string (buffer_contents buf))))))))
       (buffer_add_string buf "\n\nPatterns\n\nTuple:\n")
       (let$ arg_match = [props.%{"tuple"}])
       (let& exit = -1)
       (let$ match_arg = arg_match.%(0))
       (let$ match_arg = (prj_array match_arg).%(0))
       (if_else ((prj_int match_arg) = 1)
        (then
         (let$ match_arg = (prj_array match_arg).%(1))
         (if_else ((prj_float match_arg) = 2.5)
          (then
           (let$ match_arg = (prj_array match_arg).%(2))
           (if_else ((prj_string match_arg) = "a") (then (unit) (exit := 0))
            (else (unit))))
          (else (unit))))
        (else (unit)))
       (if_else (!exit = -1) (then (unit) (exit := 1)) (else (unit)))
       (if_else (!exit = 0) (then (buffer_add_string buf " "))
        (else (buffer_add_string buf " ")))
       (buffer_add_string buf "\n\nList:\n")
       (let$ arg_match = [props.%{"list"}])
       (let$ match_props = (hashtbl_create ())))
       (let& exit = -1)
       (let$ match_arg = arg_match.%(0))
       (if_else (test_int match_arg) (then (unit) (exit := 0))
        (else
         (let$ match_arg = arg_match.%(0))
         (let$ match_arg = (prj_array match_arg).%(0))
         (if_else (test_int match_arg)
          (then
           (let$ match_arg = (prj_array match_arg).%(1))
           (match_props.%{"_tl"} <- match_arg)
           (match_props.%{"_z"} <- match_arg)
           (exit := 2))
          (else
           (let$ match_arg = (prj_array match_arg).%(0))
           (let$ match_arg = (prj_array match_arg).%(0))
           (let$ match_arg = (prj_array match_arg).%(1))
           (if_else (test_int match_arg)
            (then
             (match_props.%{"_tl"} <- match_arg)
             (match_props.%{"_z"} <- match_arg)
             (exit := 2))
            (else
             (let$ match_arg = (prj_array match_arg).%(1))
             (let$ match_arg = (prj_array match_arg).%(0))
             (if_else (test_int match_arg)
              (then
               (let$ match_arg = (prj_array match_arg).%(1))
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
       (if_else (!exit = 0) (then (buffer_add_string buf "\n"))
        (else
         (if_else (!exit = 1)
          (then
           (buffer_add_string buf " ")
           (stm ((buffer_add_escape @@ buf) @@ (prj_string match_props.%{"a"})))
           (buffer_add_string buf "\n"))
          (else (buffer_add_string buf "\n")))))
       (buffer_add_string buf "\n\nRecord:\n")
       (let$ arg_match = [props.%{"record"}])
       (let$ match_props = (hashtbl_create ())))
       (let& exit = -1)
       (let$ match_arg = arg_match.%(0))
       (let$ match_arg = (prj_hashtbl match_arg).%{"!#%@"})
       (let$ match_arg = (prj_hashtbl match_arg).%{"a"})
       (match_props.%{"a"} <- match_arg)
       (match_props.%{"b"} <- match_arg)
       (exit := 0)
       (buffer_add_string buf " ")
       (stm ((buffer_add_escape @@ buf) @@ (prj_string match_props.%{"a"})))
       (buffer_add_string buf " ")
       (stm ((buffer_add_escape @@ buf) @@ (prj_string match_props.%{"b"})))
       (buffer_add_string buf " ")
       (buffer_add_string buf "\n\nEnum:\n")
       (let$ arg_match = [props.%{"enums"}])
       (let& exit = -1)
       (let$ match_arg = arg_match.%(0))
       (let$ match_arg = (prj_array match_arg).%(0))
       (if_else ((prj_string match_arg) = "a")
        (then
         (let$ match_arg = (prj_array match_arg).%(1))
         (if_else ((prj_int match_arg) = 1)
          (then
           (let$ match_arg = (prj_array match_arg).%(2))
           (if_else ((prj_int match_arg) = 1)
            (then
             (let$ match_arg = (prj_array match_arg).%(3))
             (if_else ((prj_int match_arg) = 0) (then (unit) (exit := 0))
              (else (unit))))
            (else (unit))))
          (else (unit))))
        (else (unit)))
       (if_else (!exit = -1) (then (unit) (exit := 1)) (else (unit)))
       (if_else (!exit = 0) (then (buffer_add_string buf " "))
        (else (buffer_add_string buf " ")))
       (buffer_add_string buf "\n\nTagged union:\n")
       (let$ arg_match = [props.%{"tagged"}])
       (let$ match_props = (hashtbl_create ())))
       (let& exit = -1)
       (let$ match_arg = arg_match.%(0))
       (let$ match_arg = (prj_hashtbl match_arg).%{"tag"})
       (if_else ((prj_int match_arg) = 0) (then (unit) (exit := 1))
        (else
         (if_else ((prj_int match_arg) = 1)
          (then
           (let$ match_arg = (prj_hashtbl match_arg).%{"a"})
           (match_props.%{"a"} <- match_arg)
           (exit := 0))
          (else (unit)))))
       (if_else (!exit = 0)
        (then
         (buffer_add_string buf " ")
         (stm ((buffer_add_escape @@ buf) @@ (prj_string match_props.%{"a"})))
         (buffer_add_string buf " "))
        (else (buffer_add_string buf "\n")))
       (buffer_add_string buf "\n\nDictionary:\n")
       (let$ arg_match = [props.%{"dict"}])
       (let& exit = -1)
       (let$ match_arg = arg_match.%(0))
       (if_else (hashtbl_mem (prj_hashtbl match_arg) "a")
        (then
         (let$ match_arg = (prj_hashtbl match_arg).%{"a"})
         (if_else ((prj_int match_arg) = 1)
          (then
           (if_else (hashtbl_mem (prj_hashtbl match_arg) "b")
            (then
             (let$ match_arg = (prj_hashtbl match_arg).%{"b"})
             (if_else ((prj_int match_arg) = 2) (then (unit) (exit := 0))
              (else (unit))))
            (else (unit))))
          (else (unit))))
        (else (unit)))
       (if_else (!exit = -1) (then (unit) (exit := 1)) (else (unit)))
       (if_else (!exit = 0) (then (buffer_add_string buf " "))
        (else (buffer_add_string buf " ")))
       (buffer_add_string buf "\n\n! and . precedence works correctly\n")
       (let$ arg_match =
        [(inj_array
          [(inj_array [(prj_hashtbl (prj_hashtbl props.%{"a"}).%{"b"}).%{"c"}])])])
       (let& exit = -1)
       (let$ match_arg = arg_match.%(0))
       (if_else (test_int match_arg) (then (unit) (exit := 1))
        (else
         (let$ match_arg = arg_match.%(0))
         (let$ match_arg = (prj_array match_arg).%(0))
         (if_else (not (test_int match_arg))
          (then
           (let$ match_arg = (prj_array match_arg).%(0))
           (let$ match_arg = (prj_array match_arg).%(0))
           (if_else ((prj_int match_arg) = 0) (then (unit) (exit := 0))
            (else (unit))))
          (else (unit)))
         (if_else (!exit = -1) (then (unit) (exit := 1)) (else (unit)))))
       (if_else (!exit = 0) (then (unit)) (else (unit)))
       (buffer_add_string buf
        "\n\nOther syntax features\n\nTrailing commas parse correctly:\n")
       (let$ arg_match =
        [(inj_hashtbl
          (hashtbl
           (("a",
             (inj_array [(inj_int 1), (inj_array [(inj_int 2), (inj_int 0)])])),
            ("b", (inj_array [(inj_int 3), (inj_int 4)])),
            ("c", (inj_hashtbl (hashtbl (("k", (inj_int 5)))))))))])
       (let& exit = -1)
       (let$ match_arg = arg_match.%(0))
       (unit)
       (exit := 0)
       (buffer_add_string buf " ")
       (buffer_add_string buf "\n\nStrings may contain line breaks:\n")
       (stm ((buffer_add_escape @@ buf) @@ "a\nb"))
       (buffer_add_string buf "\n\nZero-length record fields:\n")
       (stm
        ((buffer_add_escape @@ buf) @@
         (prj_string (prj_hashtbl props.%{"zero"}).%{""})))
       (buffer_add_string buf "\n")
       (let$ arg_match = [props.%{"zero"}])
       (let$ match_props = (hashtbl_create ())))
       (let& exit = -1)
       (let$ match_arg = arg_match.%(0))
       (let$ match_arg = (prj_hashtbl match_arg).%{""})
       (match_props.%{"empty"} <- match_arg)
       (exit := 0)
       (buffer_add_string buf " ")
       (stm ((buffer_add_escape @@ buf) @@ (prj_string match_props.%{"empty"})))
       (buffer_add_string buf " ")
       (buffer_add_string buf "\n")
       (return (buffer_contents buf)))
      (else (raise (buffer_contents errors)))))))
