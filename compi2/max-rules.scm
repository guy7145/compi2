
















(define <cond-rule>
  (pattern-rule
   `(cond ,(? 'expr) . ,(? 'exprs))
   (lambda (head tail)
     (letrec ((cond->if (lambda (lst)
                          (if (null? lst)
                              (void)
                              (if (equal? 'else (caar lst))
                                  (beginify (cdar lst))
                                  `(if ,(caar lst)
                                       ,(beginify (cdar lst))
                                       ,(cond->if (cdr lst))))))))
       (parse (cond->if `(,head ,@tail)))))))



                                        ;(display (format "\033[1;34m lst: ~s ;\033[0m " lst))(newline)