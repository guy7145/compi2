

(define Y
  (lambda (X)
    ((lambda (f) (X (lambda (x) ((f f) x))))
     (lambda (f) (X (lambda (x) ((f f) x)))))))

(define fact
  (Y
   (lambda (arg-func)
     (lambda (num)
       (if (<= num 0)
           1
           (* num (arg-func (- num 1))))))))


(define id (lambda (x) x))
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

(define <begin-rule-several-statements>
  (pattern-rule
   `(begin ,(? 'first-statement) . ,(? 'rest-statements))
   (lambda (first-statement . rest-statements)
     (let ((body (cons first-statement (car rest-statements))))
       `(seq ,(map parse body))))))

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
























