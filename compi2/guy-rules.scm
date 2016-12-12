
#|(define id (lambda (x) x))
(define <begin-rule>
  (let ((unwrapper (pattern-rule `(seq ,(? 'body)) id)))
    (Y
     (lambda (arg-func)
       
       (display arg-func)(newline)
       
       (lambda (e)
         (display e)(newline)
         (let ((sexpr 
                (begin 
                  (display 'alive)(newline) 
                  (arg-func e)))
               (cont 
                (lambda () 
                  (display 'alive!)(newline)
                  (parse e))))
           (display sexpr)(newline)
           (unwrapper sexpr cont)))))))
|#
(define get-tag car)
(define get-data cdr)



(define beginify
  (lambda (s)
    (cond ((null? s) *void-object*)
          ((null? (cdr s)) (car s))
          (else `(begin ,@s)))))

(define <begin-rule-empty>
  (pattern-rule
   `(begin)
   (lambda () `(const ,*void-object*))))

(define <begin-rule-single-statement>
  (pattern-rule
   `(begin ,(? 'body))
   (lambda (body) (parse body))))

#|(define <begin-rule-hidden>
  (pattern-rule
   `(begin ,(? 'first-statement) . ,(? 'rest-statements))
   (lambda (first-statement . rest-statements)
     (let ((body (cons first-statement (car rest-statements))))
       (flatten-list (map (lambda (e) (<begin-rule-hidden> e (lambda () `(,(parse e))))) body))))))|#

(define flatten-list
  (lambda (s)
    (cond ((null? s) '())
          ((list? (car s)) (append (car s) (flatten-list (cdr s))))
          (else (cons (car s) (flatten-list (cdr s)))))))
                                        ;(fold-right (lambda (a b) (display (format "\033[1;34m a: ~s ; b: ~s ; ;\033[0m \n" a b)) (append a b)) s '())))

(define <begin-rule-several-statements>
  (let ((parse-unwrap
         (lambda (e)
           (let ((e-tagged (parse e)))
             (if (equal? 'seq (get-tag e-tagged)) (flatten-list (get-data e-tagged)) e-tagged)))))
    (pattern-rule
     `(begin ,(? 'first-statement) . ,(? 'rest-statements))
     (lambda (first-statement . rest-statements)
       (let ((body (cons first-statement (car rest-statements))))
         `(seq ,(map parse-unwrap body)))))))
; (lambda (e) (<begin-rule-hidden> e (lambda () (parse e))))
(define <seq-rule-explicit>
  (compose-patterns
   <begin-rule-empty>
   <begin-rule-single-statement>
   <begin-rule-several-statements>
   ))


#|
       (display "\033[1;34m first: \033[0m ")(display first-statement)(display "\033[1;34m ;\033[0m ")(newline)
       (display "\033[1;34m rest: \033[0m ")(display rest-statements)(display "\033[1;34m ;\033[0m ")(newline)
       (display "\033[1;34m body: \033[0m ")(display body)(display "\033[1;34m ;\033[0m ")(newline)|#




















































