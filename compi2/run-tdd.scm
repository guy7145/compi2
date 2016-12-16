(load "cse.scm")
(load "/home/guy/Desktop/compi2_github_repo/testing-tools/tdd-tools.scm")

#|(display (apply-sub-on-itself (create-let*-body `(- (* 1 (* (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4)) (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4))))
                                                    (* 1 (* (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4)) (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4))))))))|#

(display-colored-BIG 'TDD:)
(display-colored "length of lists")
(ASSERT-EQUAL (length '()) 0)
(ASSERT-EQUAL (length '(a)) 1)
(ASSERT-EQUAL (length '(a b)) 2)

(let* ((e '(- 1 2 (+ 1 2) (+ 1 2)))
       (expr-name '(+ 1 2))
       (expr-val '1)
       (sub (cons expr-name expr-val))
       (subs `(,sub)))
  (display-colored "make-sub")(ASSERT-EQUAL (make-sub expr-name) (cons expr-name initial-instance-count))
  (display-colored "get-sub-name")(ASSERT-EQUAL (get-sub-name sub) expr-name)
  (display-colored "get-sub-value")(ASSERT-EQUAL (get-sub-value sub) expr-val)
  (display-colored "count-sub")(ASSERT-EQUAL (count-sub sub) (cons expr-name (+ 1 expr-val)))
  (display-colored "modify-subs")(ASSERT-EQUAL (modify-subs subs expr-name) `(,(cons expr-name (+ 1 expr-val))))
  (display-colored-title "modify-subs 2")(ASSERT-EQUAL (modify-subs subs `(+ ,expr-name ,expr-name)) `((,expr-name . ,expr-val) ((+ ,expr-name ,expr-name) . 1)))
  (display-colored-title "expand-subs")(ASSERT-EQUAL (expand-subs subs `(+ ,expr-name ,expr-name)) `((,expr-name . 3) ((+ ,expr-name ,expr-name) . 1)))

  (display-colored-title "clean-small-subs")
  (ASSERT-EQUAL (clean-small-subs
                 `(
                   ((* (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4))) . 1)
                   ((+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4)) . 1)
                   ((+ 1 2) . 2)
                   ((- 1 2) . 1)
                   ((+ 1 4) . 1))
                 )
   `(((+ 1 2) . 2)))

  (display-colored-title "create-optimizable-subs")
  (ASSERT-EQUAL
   (create-optimizable-subs `(* (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4)) (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4))))
   `(
     ((+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4)) . 2)
     ((+ 1 2) . 4)
     ((- 1 2) . 2)
     ((+ 1 4) . 2)
     ))
  )

(display-colored "get-expr")
(ASSERT-EQUAL (get-expr '(name (+ 1 2))) '(+ 1 2))
(display-colored "get-rename")
(ASSERT-EQUAL (get-rename '(name (+ 1 2))) 'name)
(display-colored "apply-sub-to-expr")
(ASSERT-EQUAL ((apply-sub-to-expr '(name (+ 1 2))) '(+ 1 2)) 'name)
(ASSERT-EQUAL ((apply-sub-to-expr '(name (+ 1 2))) '(- 1 2)) '(- 1 2))
(ASSERT-EQUAL ((apply-sub-to-expr '(name (+ 1 2))) '(* (+ 1 2) (+ 1 2))) '(* name name))

(ASSERT-EQUAL ((apply-sub-to-expr '(name (+ 1 2))) '(* (- 3 7) (+ 1 2))) '(* (- 3 7) name))

(ASSERT-EQUAL
 (apply-all-subs-to-expr
  '((e0 (* (+ 1 2) (- 3 4)))
    (e1 (+ 1 2))
    (e2 (- 3 4)))
  '(* (+ 1 2) (- 3 4)))
 'e0)

(ASSERT-EQUAL
 (apply-all-subs-to-expr
  '((e0 (* (* 1 2) (- 3 4)))
    (e1 (+ 1 2))
    (e2 (- 3 4)))
  '(* (+ 1 2) (- 3 4)))
 '(* e1 e2))

(let* ((expression `(* (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4)) (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4))))
       (let*-body (create-let*-body expression)))
  (ASSERT-MATCH (apply-all-subs-to-expr let*-body expression) '(* var1 var1)))



(display-colored-2 (cse `(- (* 1 (* (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4)) (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4))))
                            (* 1 (* (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4)) (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4)))))))

(display-colored-BIG 'ENDOF-TDD)
