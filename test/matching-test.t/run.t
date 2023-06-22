Basic tree
  $ acutis basic.acutis --printopt
  ((match
    ((var "a"))
    (matching
     (tree
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
           (wildcard
            (key 0)
            (ids (0))
            (child (end (end (leaf (names (("_a" 0))) (exit 0))))))))
         (wildcard ())))))
     (exits ((0 ((text " "))) (1 ((text " ")))))))
   (text "\n"))

Cases are sorted correctly
  $ acutis sorting.acutis --printopt
  ((match
    ((var "a"))
    (matching
     (tree
      (switch
       (key 0)
       (ids ())
       (cases
        (case
         (data 0)
         (if_match (end (leaf (names ()) (exit 0))))
         (next
          (case
           (data 10)
           (if_match (end (leaf (names ()) (exit 0))))
           (next
            (case
             (data 15)
             (if_match (end (leaf (names ()) (exit 1))))
             (next
              (case
               (data 20)
               (if_match (end (leaf (names ()) (exit 0))))
               (next
                (case
                 (data 30)
                 (if_match (end (leaf (names ()) (exit 0))))
                 (next ())))))))))))
       (wildcard (end (leaf (names ()) (exit 2))))
       (check_cases ())))
     (exits ((0 ((text " "))) (1 ((text " "))) (2 ((text "\n")))))))
   (text "\n"))

A basic decision tree works
  $ acutis basic_decision_tree.acutis --printopt
  ((match
    ((var "a") (var "b") (var "c"))
    (matching
     (tree
      (switch
       (key 0)
       (ids (0 2))
       (cases
        (case
         (data 10)
         (if_match
          (switch
           (key 1)
           (ids (3))
           (cases
            (case
             (data 11)
             (if_match
              (switch
               (key 2)
               (ids (4))
               (cases
                (case
                 (data 12)
                 (if_match (end (leaf (names ()) (exit 0))))
                 (next ())))
               (wildcard
                (end (leaf (names (("_a" 2) ("_b" 3) ("_c" 4))) (exit 4))))
               (check_cases ())))
             (next
              (case
               (data 21)
               (if_match
                (switch
                 (key 2)
                 (ids (4))
                 (cases
                  (case
                   (data 22)
                   (if_match (end (leaf (names (("_x" 0))) (exit 1))))
                   (next ())))
                 (wildcard
                  (end (leaf (names (("_a" 2) ("_b" 3) ("_c" 4))) (exit 4))))
                 (check_cases ())))
               (next ())))))
           (wildcard
            (wildcard
             (key 2)
             (ids (4))
             (child (end (leaf (names (("_a" 2) ("_b" 3) ("_c" 4))) (exit 4))))))
           (check_cases ())))
         (next
          (case
           (data 30)
           (if_match
            (switch
             (key 1)
             (ids (1 3))
             (cases
              (case
               (data 21)
               (if_match
                (switch
                 (key 2)
                 (ids (4))
                 (cases
                  (case
                   (data 22)
                   (if_match (end (leaf (names (("_x" 0))) (exit 1))))
                   (next
                    (case
                     (data 42)
                     (if_match (end (leaf (names (("_y" 1))) (exit 3))))
                     (next ())))))
                 (wildcard
                  (end (leaf (names (("_a" 2) ("_b" 3) ("_c" 4))) (exit 4))))
                 (check_cases ())))
               (next
                (case
                 (data 31)
                 (if_match
                  (switch
                   (key 2)
                   (ids (4))
                   (cases
                    (case
                     (data 32)
                     (if_match (end (leaf (names ()) (exit 2))))
                     (next
                      (case
                       (data 42)
                       (if_match (end (leaf (names (("_y" 1))) (exit 3))))
                       (next ())))))
                   (wildcard
                    (end (leaf (names (("_a" 2) ("_b" 3) ("_c" 4))) (exit 4))))
                   (check_cases ())))
                 (next ())))))
             (wildcard
              (switch
               (key 2)
               (ids (4))
               (cases
                (case
                 (data 42)
                 (if_match (end (leaf (names (("_y" 1))) (exit 3))))
                 (next ())))
               (wildcard
                (end (leaf (names (("_a" 2) ("_b" 3) ("_c" 4))) (exit 4))))
               (check_cases ())))
             (check_cases ())))
           (next ())))))
       (wildcard
        (switch
         (key 1)
         (ids (3))
         (cases
          (case
           (data 21)
           (if_match
            (switch
             (key 2)
             (ids (4))
             (cases
              (case
               (data 22)
               (if_match (end (leaf (names (("_x" 0))) (exit 1))))
               (next ())))
             (wildcard
              (end (leaf (names (("_a" 2) ("_b" 3) ("_c" 4))) (exit 4))))
             (check_cases ())))
           (next ())))
         (wildcard
          (wildcard
           (key 2)
           (ids (4))
           (child (end (leaf (names (("_a" 2) ("_b" 3) ("_c" 4))) (exit 4))))))
         (check_cases ())))
       (check_cases ())))
     (exits
      ((0 ((text "\n")))
       (1 ((text "\n")))
       (2 ((text "\n")))
       (3 ((text "\n")))
       (4 ((text "\n")))))))
   (text "\n"))

Nests merge correctly
  $ acutis merge_nests.acutis --printopt
  ((match
    ((var "a") (var "b") (var "c"))
    (matching
     (tree
      (wildcard
       (key 0)
       (ids ())
       (child
        (nest
         (key 1)
         (ids ())
         (child
          (int_keys
           (switch
            (key 0)
            (ids ())
            (cases
             (case
              (data 20)
              (if_match
               (switch
                (key 1)
                (ids ())
                (cases
                 (case
                  (data 21)
                  (if_match
                   (end
                    (switch
                     (key 2)
                     (ids ())
                     (cases
                      (case
                       (data 12)
                       (if_match (end (leaf (names ()) (exit 0))))
                       (next
                        (case
                         (data 22)
                         (if_match (end (leaf (names ()) (exit 1))))
                         (next
                          (case
                           (data 32)
                           (if_match (end (leaf (names ()) (exit 2))))
                           (next ())))))))
                     (wildcard (end (leaf (names ()) (exit 3))))
                     (check_cases ()))))
                  (next ())))
                (wildcard
                 (end
                  (switch
                   (key 2)
                   (ids ())
                   (cases
                    (case
                     (data 12)
                     (if_match (end (leaf (names ()) (exit 0))))
                     (next ())))
                   (wildcard (end (leaf (names ()) (exit 3))))
                   (check_cases ()))))
                (check_cases ())))
              (next ())))
            (wildcard
             (wildcard
              (key 1)
              (ids ())
              (child
               (end
                (switch
                 (key 2)
                 (ids ())
                 (cases
                  (case
                   (data 12)
                   (if_match (end (leaf (names ()) (exit 0))))
                   (next ())))
                 (wildcard (end (leaf (names ()) (exit 3))))
                 (check_cases ()))))))
            (check_cases ()))))
         (wildcard
          (switch
           (key 2)
           (ids ())
           (cases
            (case
             (data 12)
             (if_match (end (leaf (names ()) (exit 0))))
             (next ())))
           (wildcard ())
           (check_cases ())))))))
     (exits
      ((0 ((text "\n"))) (1 ((text "\n"))) (2 ((text "\n"))) (3 ((text "\n")))))))
   (text "\n"))

Wildcards merge after nests correctly
  $ acutis wildcard_merge_after_nest.acutis --printopt
  ((match
    ((var "a") (var "b"))
    (matching
     (tree
      (nest
       (key 0)
       (ids (0 1))
       (child
        (int_keys
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
               (data 10)
               (if_match
                (switch
                 (key 1)
                 (ids ())
                 (cases
                  (case
                   (data 20)
                   (if_match
                    (end
                     (switch
                      (key 1)
                      (ids ())
                      (cases
                       (case
                        (data 30)
                        (if_match
                         (end
                          (switch
                           (key 1)
                           (ids (2))
                           (cases
                            (case
                             (data 40)
                             (if_match (end (leaf (names ()) (exit 1))))
                             (next
                              (case
                               (data 41)
                               (if_match
                                (end (leaf (names (("_x" 0))) (exit 0))))
                               (next ())))))
                           (wildcard
                            (end (leaf (names (("_y" 1) ("_z" 2))) (exit 2))))
                           (check_cases ()))))
                        (next ())))
                      (wildcard ())
                      (check_cases ()))))
                   (next ())))
                 (wildcard ())
                 (check_cases ())))
               (next ())))
             (wildcard ())
             (check_cases ()))))
          (wildcard ()))))
       (wildcard
        (switch
         (key 1)
         (ids (2))
         (cases
          (case
           (data 41)
           (if_match (end (leaf (names (("_x" 0))) (exit 0))))
           (next ())))
         (wildcard (end (leaf (names (("_y" 1) ("_z" 2))) (exit 2))))
         (check_cases ())))))
     (exits ((0 ((text "\n"))) (1 ((text "\n"))) (2 ((text "\n")))))))
   (text "\n"))

Different-sized lists merge correctly
  $ acutis diff_size_lists.acutis --printopt
  ((match
    ((var "a"))
    (matching
     (tree
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
           (wildcard
            (key 0)
            (ids (0 1))
            (child
             (construct
              (key 1)
              (ids (2))
              (nil (end (end (leaf (names (("_x" 0))) (exit 1)))))
              (cons
               (wildcard
                (key 1)
                (ids (2))
                (child (end (end (leaf (names (("_x" 1) ("_y" 2))) (exit 2))))))))))))
         (wildcard ())))))
     (exits ((0 ((text "\n"))) (1 ((text "\n"))) (2 ((text "\n")))))))
   (text "\n"))

A big list pattern works
  $ acutis big_list.acutis --printopt
  ((match
    ((var "a") (var "b"))
    (matching
     (tree
      (construct
       (key 0)
       (ids (1))
       (nil
        (switch
         (key 1)
         (ids ())
         (cases
          (case
           (data 42)
           (if_match (end (leaf (names (("_y" 1))) (exit 3))))
           (next ())))
         (wildcard (end (leaf (names ()) (exit 4))))
         (check_cases ())))
       (cons
        (nest
         (key 0)
         (ids (1))
         (child
          (int_keys
           (switch
            (key 0)
            (ids ())
            (cases
             (case
              (data 10)
              (if_match
               (construct
                (key 1)
                (ids ())
                (nil ())
                (cons
                 (nest
                  (key 1)
                  (ids ())
                  (child
                   (int_keys
                    (switch
                     (key 0)
                     (ids ())
                     (cases
                      (case
                       (data 11)
                       (if_match
                        (construct
                         (key 1)
                         (ids (0))
                         (nil
                          (end
                           (end
                            (switch
                             (key 1)
                             (ids ())
                             (cases
                              (case
                               (data 12)
                               (if_match (end (leaf (names ()) (exit 0))))
                               (next
                                (case
                                 (data 22)
                                 (if_match
                                  (end (leaf (names (("_x" 0))) (exit 1))))
                                 (next
                                  (case
                                   (data 42)
                                   (if_match
                                    (end (leaf (names (("_y" 1))) (exit 3))))
                                   (next ())))))))
                             (wildcard (end (leaf (names ()) (exit 4))))
                             (check_cases ())))))
                         (cons
                          (wildcard
                           (key 1)
                           (ids (0))
                           (child
                            (end
                             (end
                              (switch
                               (key 1)
                               (ids ())
                               (cases
                                (case
                                 (data 22)
                                 (if_match
                                  (end (leaf (names (("_x" 0))) (exit 1))))
                                 (next
                                  (case
                                   (data 42)
                                   (if_match
                                    (end (leaf (names (("_y" 1))) (exit 3))))
                                   (next ())))))
                               (wildcard (end (leaf (names ()) (exit 4))))
                               (check_cases ())))))))))
                       (next ())))
                     (wildcard ())
                     (check_cases ()))))
                  (wildcard ())))))
              (next
               (case
                (data 30)
                (if_match
                 (construct
                  (key 1)
                  (ids ())
                  (nil
                   (end
                    (switch
                     (key 1)
                     (ids ())
                     (cases
                      (case
                       (data 32)
                       (if_match (end (leaf (names ()) (exit 2))))
                       (next
                        (case
                         (data 42)
                         (if_match (end (leaf (names (("_y" 1))) (exit 3))))
                         (next ())))))
                     (wildcard (end (leaf (names ()) (exit 4))))
                     (check_cases ()))))
                  (cons ())))
                (next ())))))
            (wildcard ())
            (check_cases ()))))
         (wildcard
          (switch
           (key 1)
           (ids ())
           (cases
            (case
             (data 42)
             (if_match (end (leaf (names (("_y" 1))) (exit 3))))
             (next ())))
           (wildcard (end (leaf (names ()) (exit 4))))
           (check_cases ())))))))
     (exits
      ((0 ((text "\n")))
       (1 ((text "\n")))
       (2 ((text "\n")))
       (3 ((text "\n")))
       (4 ((text "\n")))))))
   (text "\n"))

Record fields sort correctly
  $ acutis record_field_sort.acutis --printopt
  ((match
    ((var "a") (var "b"))
    (matching
     (tree
      (nest
       (key 0)
       (ids ())
       (child
        (string_keys
         (switch
          (key "a")
          (ids ())
          (cases
           (case
            (data 10)
            (if_match
             (switch
              (key "b")
              (ids ())
              (cases
               (case
                (data 11)
                (if_match
                 (end
                  (switch
                   (key 1)
                   (ids ())
                   (cases
                    (case
                     (data 12)
                     (if_match (end (leaf (names ()) (exit 0))))
                     (next ())))
                   (wildcard (end (leaf (names ()) (exit 2))))
                   (check_cases ()))))
                (next ())))
              (wildcard ())
              (check_cases ())))
            (next
             (case
              (data 20)
              (if_match
               (switch
                (key "b")
                (ids ())
                (cases
                 (case
                  (data 21)
                  (if_match
                   (end
                    (switch
                     (key 1)
                     (ids ())
                     (cases
                      (case
                       (data 22)
                       (if_match (end (leaf (names ()) (exit 1))))
                       (next ())))
                     (wildcard (end (leaf (names ()) (exit 2))))
                     (check_cases ()))))
                  (next ())))
                (wildcard ())
                (check_cases ())))
              (next ())))))
          (wildcard ())
          (check_cases ()))))
       (wildcard
        (wildcard (key 1) (ids ()) (child (end (leaf (names ()) (exit 2))))))))
     (exits ((0 ((text "\n"))) (1 ((text "\n"))) (2 ((text "\n")))))))
   (text "\n"))

New fields expand existing rows
  $ acutis record_fields_expand.acutis --printopt
  ((match
    ((var "a"))
    (matching
     (tree
      (nest
       (key 0)
       (ids (0))
       (child
        (string_keys
         (switch
          (key "a")
          (ids ())
          (cases
           (case
            (data 20)
            (if_match
             (switch
              (key "b")
              (ids ())
              (cases
               (case
                (data 10)
                (if_match
                 (wildcard
                  (key "c")
                  (ids ())
                  (child (end (end (leaf (names ()) (exit 0)))))))
                (next ())))
              (wildcard
               (wildcard
                (key "c")
                (ids ())
                (child (end (end (leaf (names ()) (exit 1)))))))
              (check_cases ())))
            (next ())))
          (wildcard
           (switch
            (key "b")
            (ids ())
            (cases
             (case
              (data 10)
              (if_match
               (wildcard
                (key "c")
                (ids ())
                (child (end (end (leaf (names ()) (exit 0)))))))
              (next ())))
            (wildcard
             (switch
              (key "c")
              (ids ())
              (cases
               (case
                (data 30)
                (if_match (end (end (leaf (names ()) (exit 2)))))
                (next ())))
              (wildcard ())
              (check_cases ())))
            (check_cases ())))
          (check_cases ()))))
       (wildcard (end (leaf (names (("_x" 0))) (exit 3))))))
     (exits
      ((0 ((text "\n"))) (1 ((text "\n"))) (2 ((text "\n"))) (3 ((text "\n")))))))
   (text "\n"))

Dictionaries merge correctly
  $ acutis dicts.acutis --printopt
  ((match
    ((var "a"))
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
                 (wildcard
                  (key "b")
                  (ids (0))
                  (child
                   (optional
                    (child ())
                    (next (end (end (leaf (names (("b" 0))) (exit 0)))))))))
                (next ())))
              (next ())))
            (wildcard ())
            (check_cases ())))
          (next
           (optional
            (child
             (switch
              (key "b")
              (ids ())
              (cases
               (case
                (data 2)
                (if_match
                 (optional
                  (child
                   (switch
                    (key "c")
                    (ids ())
                    (cases
                     (case
                      (data 3)
                      (if_match (end (end (leaf (names ()) (exit 1)))))
                      (next ())))
                    (wildcard ())
                    (check_cases ())))
                  (next ())))
                (next ())))
              (wildcard ())
              (check_cases ())))
            (next ()))))))
       (wildcard (end (leaf (names ()) (exit 2))))))
     (exits
      ((0 ((text " ") (echo () fmt_int (var "b") escape) (text "\n")))
       (1 ((text " bc\n")))
       (2 ((text " _\n")))))))
   (text "\n"))
