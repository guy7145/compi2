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
       (expr-parents (list expr-root))
       (sub (list expr-name expr-val expr-parents))
       (subs `(,sub)))
  (display-colored "make-sub")(ASSERT-EQUAL (make-sub expr-name expr-root) (list expr-name initial-instance-count `(,expr-root)))
  (display-colored "get-sub-name")(ASSERT-EQUAL (get-sub-name sub) expr-name)
  (display-colored "get-sub-value")(ASSERT-EQUAL (get-sub-value sub) expr-val)
  (display-colored "count-sub")(ASSERT-EQUAL (count-sub sub expr-root) (list expr-name (+ 1 expr-val) `(,expr-root)))
  (display-colored "modify-subs")(ASSERT-EQUAL (modify-subs subs expr-name expr-root) `(,(list expr-name (+ 1 expr-val) expr-parents)))
  (display-colored-title "modify-subs 2")
  (ASSERT-EQUAL (modify-subs subs `(+ ,expr-name ,expr-name) expr-root) `((,expr-name ,expr-val ,expr-parents) ((+ ,expr-name ,expr-name) 1 ,expr-parents)))
  
  (display-colored-title "expand-subs")
  (ASSERT-EQUAL 
   (expand-subs  '() `(+ (+ 1 2) (+ 1 2))) 
   `(((+ 1 2) 2 ((+ (+ 1 2) (+ 1 2)))) ((+ (+ 1 2) (+ 1 2)) 1 (,expr-root))))
  
  (display-colored-title "<application-rule>")
  (ASSERT-EQUAL 
   ((<application-rule> expr-root)  '() `(+ (+ 1 2) (+ 1 2)))
   `(((+ 1 2) 2 ((+ (+ 1 2) (+ 1 2)))) ((+ (+ 1 2) (+ 1 2)) 1 (,expr-root))))

  (display-colored-title "clean-small-subs")
  (ASSERT-EQUAL (clean-small-subs
                 `(
                   ((* (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4))) 1 'some-parent)
                   ((+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4)) 1 'some-parent)
                   ((+ 1 2) 2 'some-parent)
                   ((- 1 2) 1 'some-parent)
                   ((+ 1 4) 1 'some-parent))
                 )
   `(((+ 1 2) 2 'some-parent)))
  
  (display-colored-title "find-sub-by-name")
  (ASSERT-EQUAL
   (find-sub-by-name (expand-subs '()  `(f (* (* (* (+ 5 6)))) (* (* (* (+ 5 6)))))) ; subs
                     '(+ 5 6)) ; sub name
   '((+ 5 6) 2 ((* (+ 5 6))))
  )
  
  (display-colored-title "^is-sub-necessary?")
  (let ((subs (expand-subs '()  `(f (* (* (* (+ 5 6)))) (* (* (* (+ 5 6))))))))
    (ASSERT-EQUAL ((^is-sub-necessary? subs) '((+ 5 6) 2 ((* (+ 5 6))))) #f)
    (ASSERT-EQUAL ((^is-sub-necessary? subs) '((* (* (* (+ 5 6)))) 2 ((f (* (* (* (+ 5 6)))) (* (* (* (+ 5 6)))))))) #t)
    )

  (display-colored-title "remove-unnecessary-subs")
  (ASSERT-EQUAL
   (remove-unnecessary-subs (expand-subs '() `(f (* (* (* (+ 5 6)))) (* (* (* (+ 5 6)))))))
   `(((* (* (* (+ 5 6)))) 2 ((f (* (* (* (+ 5 6)))) (* (* (* (+ 5 6))))))))
  )
   
   
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

(ASSERT-EQUAL (cse '(begin (define goo (a (b b) (b c) (b b) (b c) (b b) (b c))) (a b))) '())

#|
(display-colored-2 (cse `(- (* 1 (* (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4)) (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4))))
                            (* 1 (* (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4)) (+ (+ 1 2) (+ 1 2) (- 1 2) (+ 1 4)))))))
|#

(display-colored-BIG 'ENDOF-TDD)
