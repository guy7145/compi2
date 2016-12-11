(define <if2-rule>
  (pattern-rule
   `(if ,(? 'test) ,(? 'dit))
   (lambda (test dit)
     `(if3 ,(parse test) ,(parse dit) (const ,*void-object*)))))

(define <if3-rule>
  (pattern-rule
   `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
   (lambda (test dit dif)
     `(if3 ,(parse test) ,(parse dit) ,(parse dif)))))

(define <disj-rule-no-args>
  (pattern-rule
   `(or)
   (lambda () `(const ,#f))))

(define <disj-rule-single-arg>
  (pattern-rule
   `(or ,(? 'expr))
   (lambda (expr) (parse expr) )))

(define <disj-rule-several-args>
  (pattern-rule
   `(or ,(? 'expr) . ,(? 'rest-exprs))
   (lambda (expr . rest-exprs)
     (let ((rest-exprs-unwrapped (car rest-exprs)))
       `(or (,(parse expr) ,@(map parse rest-exprs-unwrapped)))))))

(define <disj-rule>
  (compose-patterns
           <disj-rule-no-args>
           <disj-rule-single-arg>
           <disj-rule-several-args>
           ))











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


(define identify-lambda
  (lambda (args ret-simple ret-opt ret-var)
    (cond ((null? args) (ret-simple '()))
          ((symbol? args) (ret-var args))
          (else (identify-lambda
                 (cdr args)
                 (lambda (s) (ret-simple `(,(car args) ,@s))) ;simple
                 (lambda (s . opt) (ret-opt `(,(car args) ,@s) (car opt))) ; opt
                 (lambda (var) (ret-opt `(,(car args)) `(,var)))) ; var
           ))))

(define list-is-duplicative?
  (lambda (s)
    (cond ((null? s) #f)
          ((member (car s) (cdr s)) #t)
          (else (list-is-duplicative? (cdr s))))))

(define args-not-duplicative?
  (lambda (args)
    (not (and (list? args) (list-is-duplicative? args)))))

(define <lambda-rule>
  (pattern-rule
   `(lambda ,(? 'args args-not-duplicative?) ,(? 'body) . ,(? 'rest-body))
   (lambda (args body . rest-body)
     (let ((rest-body (car rest-body)))

       (let ((body (if (null? rest-body)
                       body
                       (beginify (cons body rest-body)))))

         (let ((parsed-body (parse body)))

           (identify-lambda
            args
            (lambda (s) `(lambda-simple ,s ,parsed-body)) ; simple
            (lambda (s opt) `(lambda-opt ,s ,@opt ,parsed-body)) ; opt
            (lambda (var) `(lambda-var ,var ,parsed-body)) ; var
            )))))))
























