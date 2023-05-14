Basic tree
  $ acutis basic.acutis --printopt
  ((match
    ((var "a"))
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
           (wildcard
            (key 0)
            (ids (0))
            (child (end (end (leaf (names (("_a" 0))) (exit 0))))))))
         (wildcard ())
         (debug not_dict)))))
     (exits (0 ((text " "))) (1 ((text " "))))))
   (text "\n"))

Cases are sorted correctly
  $ acutis sorting.acutis --printopt
  ((match
    ((var "a"))
    ((tree
      (switch
       (key 0)
       (ids ())
       (cases
        ((data 0)
         (if_match (end (leaf (names ()) (exit 0))))
         (next
          ((data 10)
           (if_match (end (leaf (names ()) (exit 0))))
           (next
            ((data 15)
             (if_match (end (leaf (names ()) (exit 1))))
             (next
              ((data 20)
               (if_match (end (leaf (names ()) (exit 0))))
               (next
                ((data 30)
                 (if_match (end (leaf (names ()) (exit 0))))
                 (next ())))))))))))
       (wildcard (end (leaf (names ()) (exit 2))))
       (debug_row open)))
     (exits (0 ((text " "))) (1 ((text " "))) (2 ((text "\n"))))))
   (text "\n"))

A basic decision tree works
  $ acutis basic_decision_tree.acutis --printopt
  ((match
    ((var "a") (var "b") (var "c"))
    ((tree
      (switch
       (key 0)
       (ids (0 2))
       (cases
        ((data 10)
         (if_match
          (switch
           (key 1)
           (ids (3))
           (cases
            ((data 11)
             (if_match
              (switch
               (key 2)
               (ids (4))
               (cases
                ((data 12)
                 (if_match (end (leaf (names ()) (exit 0))))
                 (next ())))
               (wildcard
                (end (leaf (names (("_a" 2) ("_b" 3) ("_c" 4))) (exit 4))))
               (debug_row open)))
             (next
              ((data 21)
               (if_match
                (switch
                 (key 2)
                 (ids (4))
                 (cases
                  ((data 22)
                   (if_match (end (leaf (names (("_x" 0))) (exit 1))))
                   (next ())))
                 (wildcard
                  (end (leaf (names (("_a" 2) ("_b" 3) ("_c" 4))) (exit 4))))
                 (debug_row open)))
               (next ())))))
           (wildcard
            (wildcard
             (key 2)
             (ids (4))
             (child (end (leaf (names (("_a" 2) ("_b" 3) ("_c" 4))) (exit 4))))))
           (debug_row open)))
         (next
          ((data 30)
           (if_match
            (switch
             (key 1)
             (ids (1 3))
             (cases
              ((data 21)
               (if_match
                (switch
                 (key 2)
                 (ids (4))
                 (cases
                  ((data 22)
                   (if_match (end (leaf (names (("_x" 0))) (exit 1))))
                   (next
                    ((data 42)
                     (if_match (end (leaf (names (("_y" 1))) (exit 3))))
                     (next ())))))
                 (wildcard
                  (end (leaf (names (("_a" 2) ("_b" 3) ("_c" 4))) (exit 4))))
                 (debug_row open)))
               (next
                ((data 31)
                 (if_match
                  (switch
                   (key 2)
                   (ids (4))
                   (cases
                    ((data 32)
                     (if_match (end (leaf (names ()) (exit 2))))
                     (next
                      ((data 42)
                       (if_match (end (leaf (names (("_y" 1))) (exit 3))))
                       (next ())))))
                   (wildcard
                    (end (leaf (names (("_a" 2) ("_b" 3) ("_c" 4))) (exit 4))))
                   (debug_row open)))
                 (next ())))))
             (wildcard
              (switch
               (key 2)
               (ids (4))
               (cases
                ((data 42)
                 (if_match (end (leaf (names (("_y" 1))) (exit 3))))
                 (next ())))
               (wildcard
                (end (leaf (names (("_a" 2) ("_b" 3) ("_c" 4))) (exit 4))))
               (debug_row open)))
             (debug_row open)))
           (next ())))))
       (wildcard
        (switch
         (key 1)
         (ids (3))
         (cases
          ((data 21)
           (if_match
            (switch
             (key 2)
             (ids (4))
             (cases
              ((data 22)
               (if_match (end (leaf (names (("_x" 0))) (exit 1))))
               (next ())))
             (wildcard
              (end (leaf (names (("_a" 2) ("_b" 3) ("_c" 4))) (exit 4))))
             (debug_row open)))
           (next ())))
         (wildcard
          (wildcard
           (key 2)
           (ids (4))
           (child (end (leaf (names (("_a" 2) ("_b" 3) ("_c" 4))) (exit 4))))))
         (debug_row open)))
       (debug_row open)))
     (exits
      (0 ((text "\n")))
      (1 ((text "\n")))
      (2 ((text "\n")))
      (3 ((text "\n")))
      (4 ((text "\n"))))))
   (text "\n"))

Nests merge correctly
  $ acutis merge_nests.acutis --printopt
  ((match
    ((var "a") (var "b") (var "c"))
    ((tree
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
             ((data 20)
              (if_match
               (switch
                (key 1)
                (ids ())
                (cases
                 ((data 21)
                  (if_match
                   (end
                    (switch
                     (key 2)
                     (ids ())
                     (cases
                      ((data 12)
                       (if_match (end (leaf (names ()) (exit 0))))
                       (next
                        ((data 22)
                         (if_match (end (leaf (names ()) (exit 1))))
                         (next
                          ((data 32)
                           (if_match (end (leaf (names ()) (exit 2))))
                           (next ())))))))
                     (wildcard (end (leaf (names ()) (exit 3))))
                     (debug_row open))))
                  (next ())))
                (wildcard
                 (end
                  (switch
                   (key 2)
                   (ids ())
                   (cases
                    ((data 12)
                     (if_match (end (leaf (names ()) (exit 0))))
                     (next ())))
                   (wildcard (end (leaf (names ()) (exit 3))))
                   (debug_row open))))
                (debug_row open)))
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
                  ((data 12)
                   (if_match (end (leaf (names ()) (exit 0))))
                   (next ())))
                 (wildcard (end (leaf (names ()) (exit 3))))
                 (debug_row open))))))
            (debug_row open))))
         (wildcard
          (switch
           (key 2)
           (ids ())
           (cases
            ((data 12) (if_match (end (leaf (names ()) (exit 0)))) (next ())))
           (wildcard ())
           (debug_row open)))
         (debug not_dict)))))
     (exits
      (0 ((text "\n")))
      (1 ((text "\n")))
      (2 ((text "\n")))
      (3 ((text "\n"))))))
   (text "\n"))

Wildcards merge after nests correctly
  $ acutis wildcard_merge_after_nest.acutis --printopt
  ((match
    ((var "a") (var "b"))
    ((tree
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
              ((data 10)
               (if_match
                (switch
                 (key 1)
                 (ids ())
                 (cases
                  ((data 20)
                   (if_match
                    (end
                     (switch
                      (key 1)
                      (ids ())
                      (cases
                       ((data 30)
                        (if_match
                         (end
                          (switch
                           (key 1)
                           (ids (2))
                           (cases
                            ((data 40)
                             (if_match (end (leaf (names ()) (exit 1))))
                             (next
                              ((data 41)
                               (if_match
                                (end (leaf (names (("_x" 0))) (exit 0))))
                               (next ())))))
                           (wildcard
                            (end (leaf (names (("_y" 1) ("_z" 2))) (exit 2))))
                           (debug_row open))))
                        (next ())))
                      (wildcard ())
                      (debug_row open))))
                   (next ())))
                 (wildcard ())
                 (debug_row open)))
               (next ())))
             (wildcard ())
             (debug_row open))))
          (wildcard ())
          (debug not_dict))))
       (wildcard
        (switch
         (key 1)
         (ids (2))
         (cases
          ((data 41)
           (if_match (end (leaf (names (("_x" 0))) (exit 0))))
           (next ())))
         (wildcard (end (leaf (names (("_y" 1) ("_z" 2))) (exit 2))))
         (debug_row open)))
       (debug not_dict)))
     (exits (0 ((text "\n"))) (1 ((text "\n"))) (2 ((text "\n"))))))
   (text "\n"))

Different-sized lists merge correctly
  $ acutis diff_size_lists.acutis --printopt
  ((match
    ((var "a"))
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
         (wildcard ())
         (debug not_dict)))))
     (exits (0 ((text "\n"))) (1 ((text "\n"))) (2 ((text "\n"))))))
   (text "\n"))

A big list pattern works
  $ acutis big_list.acutis --printopt
  ((match
    ((var "a") (var "b"))
    ((tree
      (construct
       (key 0)
       (ids (1))
       (nil
        (switch
         (key 1)
         (ids ())
         (cases
          ((data 42)
           (if_match (end (leaf (names (("_y" 1))) (exit 3))))
           (next ())))
         (wildcard (end (leaf (names ()) (exit 4))))
         (debug_row open)))
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
             ((data 10)
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
                      ((data 11)
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
                              ((data 12)
                               (if_match (end (leaf (names ()) (exit 0))))
                               (next
                                ((data 22)
                                 (if_match
                                  (end (leaf (names (("_x" 0))) (exit 1))))
                                 (next
                                  ((data 42)
                                   (if_match
                                    (end (leaf (names (("_y" 1))) (exit 3))))
                                   (next ())))))))
                             (wildcard (end (leaf (names ()) (exit 4))))
                             (debug_row open)))))
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
                                ((data 22)
                                 (if_match
                                  (end (leaf (names (("_x" 0))) (exit 1))))
                                 (next
                                  ((data 42)
                                   (if_match
                                    (end (leaf (names (("_y" 1))) (exit 3))))
                                   (next ())))))
                               (wildcard (end (leaf (names ()) (exit 4))))
                               (debug_row open)))))))))
                       (next ())))
                     (wildcard ())
                     (debug_row open))))
                  (wildcard ())
                  (debug not_dict)))))
              (next
               ((data 30)
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
                      ((data 32)
                       (if_match (end (leaf (names ()) (exit 2))))
                       (next
                        ((data 42)
                         (if_match (end (leaf (names (("_y" 1))) (exit 3))))
                         (next ())))))
                     (wildcard (end (leaf (names ()) (exit 4))))
                     (debug_row open))))
                  (cons ())))
                (next ())))))
            (wildcard ())
            (debug_row open))))
         (wildcard
          (switch
           (key 1)
           (ids ())
           (cases
            ((data 42)
             (if_match (end (leaf (names (("_y" 1))) (exit 3))))
             (next ())))
           (wildcard (end (leaf (names ()) (exit 4))))
           (debug_row open)))
         (debug not_dict)))))
     (exits
      (0 ((text "\n")))
      (1 ((text "\n")))
      (2 ((text "\n")))
      (3 ((text "\n")))
      (4 ((text "\n"))))))
   (text "\n"))

Record fields sort correctly
  $ acutis record_field_sort.acutis --printopt
  ((match
    ((var "a") (var "b"))
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
           ((data 10)
            (if_match
             (switch
              (key "b")
              (ids ())
              (cases
               ((data 11)
                (if_match
                 (end
                  (switch
                   (key 1)
                   (ids ())
                   (cases
                    ((data 12)
                     (if_match (end (leaf (names ()) (exit 0))))
                     (next ())))
                   (wildcard (end (leaf (names ()) (exit 2))))
                   (debug_row open))))
                (next ())))
              (wildcard ())
              (debug_row open)))
            (next
             ((data 20)
              (if_match
               (switch
                (key "b")
                (ids ())
                (cases
                 ((data 21)
                  (if_match
                   (end
                    (switch
                     (key 1)
                     (ids ())
                     (cases
                      ((data 22)
                       (if_match (end (leaf (names ()) (exit 1))))
                       (next ())))
                     (wildcard (end (leaf (names ()) (exit 2))))
                     (debug_row open))))
                  (next ())))
                (wildcard ())
                (debug_row open)))
              (next ())))))
          (wildcard ())
          (debug_row open))))
       (wildcard
        (wildcard (key 1) (ids ()) (child (end (leaf (names ()) (exit 2))))))
       (debug not_dict)))
     (exits (0 ((text "\n"))) (1 ((text "\n"))) (2 ((text "\n"))))))
   (text "\n"))

New fields expand existing rows
  $ acutis record_fields_expand.acutis --printopt
  ((match
    ((var "a"))
    ((tree
      (nest
       (key 0)
       (ids (0))
       (child
        (string_keys
         (switch
          (key "a")
          (ids ())
          (cases
           ((data 20)
            (if_match
             (switch
              (key "b")
              (ids ())
              (cases
               ((data 10)
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
              (debug_row open)))
            (next ())))
          (wildcard
           (switch
            (key "b")
            (ids ())
            (cases
             ((data 10)
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
               ((data 30)
                (if_match (end (end (leaf (names ()) (exit 2)))))
                (next ())))
              (wildcard ())
              (debug_row open)))
            (debug_row open)))
          (debug_row open))))
       (wildcard (end (leaf (names (("_x" 0))) (exit 3))))
       (debug not_dict)))
     (exits
      (0 ((text "\n")))
      (1 ((text "\n")))
      (2 ((text "\n")))
      (3 ((text "\n"))))))
   (text "\n"))
