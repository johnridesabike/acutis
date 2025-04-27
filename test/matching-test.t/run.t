Basic tree
  $ acutis basic.acutis --printopt
  ((Match
    ()
    ((`Var "a"))
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
             (Wildcard
              ((key 0)
               (ids (Set (0)))
               (child (end (end ((names (Map (("_a" 0)))) (exit 0)))))))))
           (wildcard None)))))))
     (exits
      (((id 0) (bindings ("_a")) (nodes ((Text " "))))
       ((id 1) (bindings ()) (nodes ((Text " "))))))))
   (Text "\n"))

Cases are sorted correctly
  $ acutis sorting.acutis --printopt
  ((Match
    ()
    ((`Var "a"))
    ((tree
      (Switch
       ((key 0)
        (ids (Set ()))
        (cases
         ((data (`Int 0))
          (if_match (end ((names (Map ())) (exit 0))))
          (next
           (Some
            ((data (`Int 10))
             (if_match (end ((names (Map ())) (exit 0))))
             (next
              (Some
               ((data (`Int 15))
                (if_match (end ((names (Map ())) (exit 1))))
                (next
                 (Some
                  ((data (`Int 20))
                   (if_match (end ((names (Map ())) (exit 0))))
                   (next
                    (Some
                     ((data (`Int 30))
                      (if_match (end ((names (Map ())) (exit 0))))
                      (next None)))))))))))))))
        (wildcard (Some (end ((names (Map ())) (exit 2)))))
        (check_cases None))))
     (exits
      (((id 0) (bindings ()) (nodes ((Text " "))))
       ((id 1) (bindings ()) (nodes ((Text " "))))
       ((id 2) (bindings ()) (nodes ((Text "\n"))))))))
   (Text "\n"))

A basic decision tree works
  $ acutis basic_decision_tree.acutis --printopt
  ((Match
    ()
    ((`Var "a") (`Var "b") (`Var "c"))
    ((tree
      (Switch
       ((key 0)
        (ids (Set (0 2)))
        (cases
         ((data (`Int 10))
          (if_match
           (Switch
            ((key 1)
             (ids (Set (3)))
             (cases
              ((data (`Int 11))
               (if_match
                (Switch
                 ((key 2)
                  (ids (Set (4)))
                  (cases
                   ((data (`Int 12))
                    (if_match (end ((names (Map ())) (exit 0))))
                    (next None)))
                  (wildcard
                   (Some
                    (end ((names (Map (("_a" 2) ("_b" 3) ("_c" 4)))) (exit 4)))))
                  (check_cases None))))
               (next
                (Some
                 ((data (`Int 21))
                  (if_match
                   (Switch
                    ((key 2)
                     (ids (Set (4)))
                     (cases
                      ((data (`Int 22))
                       (if_match (end ((names (Map (("_x" 0)))) (exit 1))))
                       (next None)))
                     (wildcard
                      (Some
                       (end
                        ((names (Map (("_a" 2) ("_b" 3) ("_c" 4)))) (exit 4)))))
                     (check_cases None))))
                  (next None))))))
             (wildcard
              (Some
               (Wildcard
                ((key 2)
                 (ids (Set (4)))
                 (child
                  (end ((names (Map (("_a" 2) ("_b" 3) ("_c" 4)))) (exit 4))))))))
             (check_cases None))))
          (next
           (Some
            ((data (`Int 30))
             (if_match
              (Switch
               ((key 1)
                (ids (Set (1 3)))
                (cases
                 ((data (`Int 21))
                  (if_match
                   (Switch
                    ((key 2)
                     (ids (Set (4)))
                     (cases
                      ((data (`Int 22))
                       (if_match (end ((names (Map (("_x" 0)))) (exit 1))))
                       (next
                        (Some
                         ((data (`Int 42))
                          (if_match (end ((names (Map (("_y" 1)))) (exit 3))))
                          (next None))))))
                     (wildcard
                      (Some
                       (end
                        ((names (Map (("_a" 2) ("_b" 3) ("_c" 4)))) (exit 4)))))
                     (check_cases None))))
                  (next
                   (Some
                    ((data (`Int 31))
                     (if_match
                      (Switch
                       ((key 2)
                        (ids (Set (4)))
                        (cases
                         ((data (`Int 32))
                          (if_match (end ((names (Map ())) (exit 2))))
                          (next
                           (Some
                            ((data (`Int 42))
                             (if_match
                              (end ((names (Map (("_y" 1)))) (exit 3))))
                             (next None))))))
                        (wildcard
                         (Some
                          (end
                           ((names (Map (("_a" 2) ("_b" 3) ("_c" 4)))) (exit 4)))))
                        (check_cases None))))
                     (next None))))))
                (wildcard
                 (Some
                  (Switch
                   ((key 2)
                    (ids (Set (4)))
                    (cases
                     ((data (`Int 42))
                      (if_match (end ((names (Map (("_y" 1)))) (exit 3))))
                      (next None)))
                    (wildcard
                     (Some
                      (end
                       ((names (Map (("_a" 2) ("_b" 3) ("_c" 4)))) (exit 4)))))
                    (check_cases None)))))
                (check_cases None))))
             (next None))))))
        (wildcard
         (Some
          (Switch
           ((key 1)
            (ids (Set (3)))
            (cases
             ((data (`Int 21))
              (if_match
               (Switch
                ((key 2)
                 (ids (Set (4)))
                 (cases
                  ((data (`Int 22))
                   (if_match (end ((names (Map (("_x" 0)))) (exit 1))))
                   (next None)))
                 (wildcard
                  (Some
                   (end ((names (Map (("_a" 2) ("_b" 3) ("_c" 4)))) (exit 4)))))
                 (check_cases None))))
              (next None)))
            (wildcard
             (Some
              (Wildcard
               ((key 2)
                (ids (Set (4)))
                (child
                 (end ((names (Map (("_a" 2) ("_b" 3) ("_c" 4)))) (exit 4))))))))
            (check_cases None)))))
        (check_cases None))))
     (exits
      (((id 0) (bindings ()) (nodes ((Text "\n"))))
       ((id 1) (bindings ("_x")) (nodes ((Text "\n"))))
       ((id 2) (bindings ()) (nodes ((Text "\n"))))
       ((id 3) (bindings ("_y")) (nodes ((Text "\n"))))
       ((id 4) (bindings ("_a" "_b" "_c")) (nodes ((Text "\n"))))))))
   (Text "\n"))

Nests merge correctly
  $ acutis merge_nests.acutis --printopt
  ((Match
    ()
    ((`Var "a") (`Var "b") (`Var "c"))
    ((tree
      (Wildcard
       ((key 0)
        (ids (Set ()))
        (child
         (Nest
          ((key 1)
           (ids (Set ()))
           (child
            (Int_keys
             (Switch
              ((key 0)
               (ids (Set ()))
               (cases
                ((data (`Int 20))
                 (if_match
                  (Switch
                   ((key 1)
                    (ids (Set ()))
                    (cases
                     ((data (`Int 21))
                      (if_match
                       (end
                        (Switch
                         ((key 2)
                          (ids (Set ()))
                          (cases
                           ((data (`Int 12))
                            (if_match (end ((names (Map ())) (exit 0))))
                            (next
                             (Some
                              ((data (`Int 22))
                               (if_match (end ((names (Map ())) (exit 1))))
                               (next
                                (Some
                                 ((data (`Int 32))
                                  (if_match (end ((names (Map ())) (exit 2))))
                                  (next None)))))))))
                          (wildcard (Some (end ((names (Map ())) (exit 3)))))
                          (check_cases None)))))
                      (next None)))
                    (wildcard
                     (Some
                      (end
                       (Switch
                        ((key 2)
                         (ids (Set ()))
                         (cases
                          ((data (`Int 12))
                           (if_match (end ((names (Map ())) (exit 0))))
                           (next None)))
                         (wildcard (Some (end ((names (Map ())) (exit 3)))))
                         (check_cases None))))))
                    (check_cases None))))
                 (next None)))
               (wildcard
                (Some
                 (Wildcard
                  ((key 1)
                   (ids (Set ()))
                   (child
                    (end
                     (Switch
                      ((key 2)
                       (ids (Set ()))
                       (cases
                        ((data (`Int 12))
                         (if_match (end ((names (Map ())) (exit 0))))
                         (next None)))
                       (wildcard (Some (end ((names (Map ())) (exit 3)))))
                       (check_cases None)))))))))
               (check_cases None)))))
           (wildcard None)))))))
     (exits
      (((id 0) (bindings ()) (nodes ((Text "\n"))))
       ((id 1) (bindings ()) (nodes ((Text "\n"))))
       ((id 2) (bindings ()) (nodes ((Text "\n"))))
       ((id 3) (bindings ()) (nodes ((Text "\n"))))))))
   (Text "\n"))

Wildcards merge after nests correctly
  $ acutis wildcard_merge_after_nest.acutis --printopt
  ((Match
    ()
    ((`Var "a") (`Var "b"))
    ((tree
      (Nest
       ((key 0)
        (ids (Set (0 1)))
        (child
         (Int_keys
          (Nest
           ((key 0)
            (ids (Set ()))
            (child
             (Int_keys
              (Switch
               ((key 0)
                (ids (Set ()))
                (cases
                 ((data (`Int 10))
                  (if_match
                   (Switch
                    ((key 1)
                     (ids (Set ()))
                     (cases
                      ((data (`Int 20))
                       (if_match
                        (end
                         (Switch
                          ((key 1)
                           (ids (Set ()))
                           (cases
                            ((data (`Int 30))
                             (if_match
                              (end
                               (Switch
                                ((key 1)
                                 (ids (Set (2)))
                                 (cases
                                  ((data (`Int 40))
                                   (if_match (end ((names (Map ())) (exit 1))))
                                   (next
                                    (Some
                                     ((data (`Int 41))
                                      (if_match
                                       (end ((names (Map (("_x" 0)))) (exit 0))))
                                      (next None))))))
                                 (wildcard
                                  (Some
                                   (end
                                    ((names (Map (("_y" 1) ("_z" 2)))) (exit 2)))))
                                 (check_cases None)))))
                             (next None)))
                           (wildcard None)
                           (check_cases None)))))
                       (next None)))
                     (wildcard None)
                     (check_cases None))))
                  (next None)))
                (wildcard None)
                (check_cases None)))))
            (wildcard None)))))
        (wildcard
         (Some
          (Switch
           ((key 1)
            (ids (Set (2)))
            (cases
             ((data (`Int 41))
              (if_match (end ((names (Map (("_x" 0)))) (exit 0))))
              (next None)))
            (wildcard (Some (end ((names (Map (("_y" 1) ("_z" 2)))) (exit 2)))))
            (check_cases None))))))))
     (exits
      (((id 0) (bindings ("_x")) (nodes ((Text "\n"))))
       ((id 1) (bindings ()) (nodes ((Text "\n"))))
       ((id 2) (bindings ("_y" "_z")) (nodes ((Text "\n"))))))))
   (Text "\n"))

Different-sized lists merge correctly
  $ acutis diff_size_lists.acutis --printopt
  ((Match
    ()
    ((`Var "a"))
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
             (Wildcard
              ((key 0)
               (ids (Set (0 1)))
               (child
                (Nil_or_cons
                 ((key 1)
                  (ids (Set (2)))
                  (nil (end (end ((names (Map (("_x" 0)))) (exit 1)))))
                  (cons
                   (Wildcard
                    ((key 1)
                     (ids (Set (2)))
                     (child
                      (end (end ((names (Map (("_x" 1) ("_y" 2)))) (exit 2)))))))))))))))
           (wildcard None)))))))
     (exits
      (((id 0) (bindings ()) (nodes ((Text "\n"))))
       ((id 1) (bindings ("_x")) (nodes ((Text "\n"))))
       ((id 2) (bindings ("_x" "_y")) (nodes ((Text "\n"))))))))
   (Text "\n"))

A big list pattern works
  $ acutis big_list.acutis --printopt
  ((Match
    ()
    ((`Var "a") (`Var "b"))
    ((tree
      (Nil_or_cons
       ((key 0)
        (ids (Set (1)))
        (nil
         (Switch
          ((key 1)
           (ids (Set ()))
           (cases
            ((data (`Int 42))
             (if_match (end ((names (Map (("_y" 1)))) (exit 3))))
             (next None)))
           (wildcard (Some (end ((names (Map ())) (exit 4)))))
           (check_cases None))))
        (cons
         (Nest
          ((key 0)
           (ids (Set (1)))
           (child
            (Int_keys
             (Switch
              ((key 0)
               (ids (Set ()))
               (cases
                ((data (`Int 10))
                 (if_match
                  (Cons
                   ((key 1)
                    (ids (Set ()))
                    (child
                     (Nest
                      ((key 1)
                       (ids (Set ()))
                       (child
                        (Int_keys
                         (Switch
                          ((key 0)
                           (ids (Set ()))
                           (cases
                            ((data (`Int 11))
                             (if_match
                              (Nil_or_cons
                               ((key 1)
                                (ids (Set (0)))
                                (nil
                                 (end
                                  (end
                                   (Switch
                                    ((key 1)
                                     (ids (Set ()))
                                     (cases
                                      ((data (`Int 12))
                                       (if_match
                                        (end ((names (Map ())) (exit 0))))
                                       (next
                                        (Some
                                         ((data (`Int 22))
                                          (if_match
                                           (end
                                            ((names (Map (("_x" 0)))) (exit 1))))
                                          (next
                                           (Some
                                            ((data (`Int 42))
                                             (if_match
                                              (end
                                               ((names (Map (("_y" 1))))
                                                (exit 3))))
                                             (next None)))))))))
                                     (wildcard
                                      (Some (end ((names (Map ())) (exit 4)))))
                                     (check_cases None))))))
                                (cons
                                 (Wildcard
                                  ((key 1)
                                   (ids (Set (0)))
                                   (child
                                    (end
                                     (end
                                      (Switch
                                       ((key 1)
                                        (ids (Set ()))
                                        (cases
                                         ((data (`Int 22))
                                          (if_match
                                           (end
                                            ((names (Map (("_x" 0)))) (exit 1))))
                                          (next
                                           (Some
                                            ((data (`Int 42))
                                             (if_match
                                              (end
                                               ((names (Map (("_y" 1))))
                                                (exit 3))))
                                             (next None))))))
                                        (wildcard
                                         (Some
                                          (end ((names (Map ())) (exit 4)))))
                                        (check_cases None))))))))))))
                             (next None)))
                           (wildcard None)
                           (check_cases None)))))
                       (wildcard None)))))))
                 (next
                  (Some
                   ((data (`Int 30))
                    (if_match
                     (Nil
                      ((key 1)
                       (ids (Set ()))
                       (child
                        (end
                         (Switch
                          ((key 1)
                           (ids (Set ()))
                           (cases
                            ((data (`Int 32))
                             (if_match (end ((names (Map ())) (exit 2))))
                             (next
                              (Some
                               ((data (`Int 42))
                                (if_match
                                 (end ((names (Map (("_y" 1)))) (exit 3))))
                                (next None))))))
                           (wildcard (Some (end ((names (Map ())) (exit 4)))))
                           (check_cases None))))))))
                    (next None))))))
               (wildcard None)
               (check_cases None)))))
           (wildcard
            (Some
             (Switch
              ((key 1)
               (ids (Set ()))
               (cases
                ((data (`Int 42))
                 (if_match (end ((names (Map (("_y" 1)))) (exit 3))))
                 (next None)))
               (wildcard (Some (end ((names (Map ())) (exit 4)))))
               (check_cases None)))))))))))
     (exits
      (((id 0) (bindings ()) (nodes ((Text "\n"))))
       ((id 1) (bindings ("_x")) (nodes ((Text "\n"))))
       ((id 2) (bindings ()) (nodes ((Text "\n"))))
       ((id 3) (bindings ("_y")) (nodes ((Text "\n"))))
       ((id 4) (bindings ()) (nodes ((Text "\n"))))))))
   (Text "\n"))

Record fields sort correctly
  $ acutis record_field_sort.acutis --printopt
  ((Match
    ()
    ((`Var "a") (`Var "b"))
    ((tree
      (Nest
       ((key 0)
        (ids (Set ()))
        (child
         (String_keys
          (Switch
           ((key "a")
            (ids (Set ()))
            (cases
             ((data (`Int 10))
              (if_match
               (Switch
                ((key "b")
                 (ids (Set ()))
                 (cases
                  ((data (`Int 11))
                   (if_match
                    (end
                     (Switch
                      ((key 1)
                       (ids (Set ()))
                       (cases
                        ((data (`Int 12))
                         (if_match (end ((names (Map ())) (exit 0))))
                         (next None)))
                       (wildcard (Some (end ((names (Map ())) (exit 2)))))
                       (check_cases None)))))
                   (next None)))
                 (wildcard None)
                 (check_cases None))))
              (next
               (Some
                ((data (`Int 20))
                 (if_match
                  (Switch
                   ((key "b")
                    (ids (Set ()))
                    (cases
                     ((data (`Int 21))
                      (if_match
                       (end
                        (Switch
                         ((key 1)
                          (ids (Set ()))
                          (cases
                           ((data (`Int 22))
                            (if_match (end ((names (Map ())) (exit 1))))
                            (next None)))
                          (wildcard (Some (end ((names (Map ())) (exit 2)))))
                          (check_cases None)))))
                      (next None)))
                    (wildcard None)
                    (check_cases None))))
                 (next None))))))
            (wildcard None)
            (check_cases None)))))
        (wildcard
         (Some
          (Wildcard
           ((key 1) (ids (Set ())) (child (end ((names (Map ())) (exit 2)))))))))))
     (exits
      (((id 0) (bindings ()) (nodes ((Text "\n"))))
       ((id 1) (bindings ()) (nodes ((Text "\n"))))
       ((id 2) (bindings ()) (nodes ((Text "\n"))))))))
   (Text "\n"))

New fields expand existing rows
  $ acutis record_fields_expand.acutis --printopt
  ((Match
    ()
    ((`Var "a"))
    ((tree
      (Nest
       ((key 0)
        (ids (Set (0)))
        (child
         (String_keys
          (Switch
           ((key "a")
            (ids (Set ()))
            (cases
             ((data (`Int 20))
              (if_match
               (Switch
                ((key "b")
                 (ids (Set ()))
                 (cases
                  ((data (`Int 10))
                   (if_match
                    (Wildcard
                     ((key "c")
                      (ids (Set ()))
                      (child (end (end ((names (Map ())) (exit 0))))))))
                   (next None)))
                 (wildcard
                  (Some
                   (Wildcard
                    ((key "c")
                     (ids (Set ()))
                     (child (end (end ((names (Map ())) (exit 1)))))))))
                 (check_cases None))))
              (next None)))
            (wildcard
             (Some
              (Switch
               ((key "b")
                (ids (Set ()))
                (cases
                 ((data (`Int 10))
                  (if_match
                   (Wildcard
                    ((key "c")
                     (ids (Set ()))
                     (child (end (end ((names (Map ())) (exit 0))))))))
                  (next None)))
                (wildcard
                 (Some
                  (Switch
                   ((key "c")
                    (ids (Set ()))
                    (cases
                     ((data (`Int 30))
                      (if_match (end (end ((names (Map ())) (exit 2)))))
                      (next None)))
                    (wildcard None)
                    (check_cases None)))))
                (check_cases None)))))
            (check_cases None)))))
        (wildcard (Some (end ((names (Map (("_x" 0)))) (exit 3))))))))
     (exits
      (((id 0) (bindings ()) (nodes ((Text "\n"))))
       ((id 1) (bindings ()) (nodes ((Text "\n"))))
       ((id 2) (bindings ()) (nodes ((Text "\n"))))
       ((id 3) (bindings ("_x")) (nodes ((Text "\n"))))))))
   (Text "\n"))

Dictionaries merge correctly
  $ acutis dicts.acutis --printopt
  ((Match
    ()
    ((`Var "a"))
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
                     (Wildcard
                      ((key "b")
                       (ids (Set (0)))
                       (child
                        (Optional
                         ((child
                           (Wildcard
                            ((key "c")
                             (ids (Set ()))
                             (child
                              (end (end ((names (Map (("b" 0)))) (exit 0))))))))
                          (next
                           (Some (end (end ((names (Map (("b" 0)))) (exit 0))))))))))))
                    (next None))))
                 (next None)))
               (wildcard
                (Some
                 (Optional
                  ((child
                    (Switch
                     ((key "b")
                      (ids (Set ()))
                      (cases
                       ((data (`Int 2))
                        (if_match
                         (Optional
                          ((child
                            (Switch
                             ((key "c")
                              (ids (Set ()))
                              (cases
                               ((data (`Int 3))
                                (if_match
                                 (end (end ((names (Map ())) (exit 1)))))
                                (next None)))
                              (wildcard None)
                              (check_cases None))))
                           (next None))))
                        (next None)))
                      (wildcard None)
                      (check_cases None))))
                   (next None)))))
               (check_cases None))))
            (next
             (Some
              (Optional
               ((child
                 (Switch
                  ((key "b")
                   (ids (Set ()))
                   (cases
                    ((data (`Int 2))
                     (if_match
                      (Optional
                       ((child
                         (Switch
                          ((key "c")
                           (ids (Set ()))
                           (cases
                            ((data (`Int 3))
                             (if_match (end (end ((names (Map ())) (exit 1)))))
                             (next None)))
                           (wildcard None)
                           (check_cases None))))
                        (next None))))
                     (next None)))
                   (wildcard None)
                   (check_cases None))))
                (next None)))))))))
        (wildcard (Some (end ((names (Map ())) (exit 2))))))))
     (exits
      (((id 0)
        (bindings ("b"))
        (nodes ((Text " ") (Echo () Fmt_int (`Var "b") Escape) (Text "\n"))))
       ((id 1) (bindings ()) (nodes ((Text " bc\n"))))
       ((id 2) (bindings ()) (nodes ((Text " _\n"))))))))
   (Match
    ()
    ((`Assoc (Map (("a" (`String "empty dicts always match"))))))
    ((tree
      (Nest
       ((key 0)
        (ids (Set ()))
        (child
         (String_keys
          (Optional
           ((child
             (Wildcard
              ((key "a")
               (ids (Set ()))
               (child
                (Optional
                 ((child
                   (Wildcard
                    ((key "b")
                     (ids (Set (0)))
                     (child (end (end ((names (Map (("b" 0)))) (exit 0))))))))
                  (next (Some (end (end ((names (Map ())) (exit 1))))))))))))
            (next
             (Some
              (Optional
               ((child
                 (Wildcard
                  ((key "b")
                   (ids (Set (0)))
                   (child (end (end ((names (Map (("b" 0)))) (exit 0))))))))
                (next (Some (end (end ((names (Map ())) (exit 1))))))))))))))
        (wildcard None))))
     (exits
      (((id 0)
        (bindings ("b"))
        (nodes ((Text " ") (Echo () Fmt_string (`Var "b") Escape) (Text "\n"))))
       ((id 1) (bindings ()) (nodes ((Text "empty\n"))))))))
   (Text "\n"))
