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

(define <disj-rule>
  (pattern-rule
   `(or ,(? 'expr) . ,(? 'rest-exprs))
   (lambda (expr . rest-exprs)
     (let ((rest-exprs-unwrapped (car rest-exprs)))
       `(or (,(parse expr) ,@(map parse rest-exprs-unwrapped)))))))

(define beginify
  (lambda (s)
    (cond ((null? s) *void-object*)
          ((null? (cdr s)) (car s))
          (else `(begin ,@s)))))

(define <seq-rule-explicit>
  (pattern-rule
   `(begin ,(? 'statements))
   (lambda (statements) `(seq ,(map parse statements)))))

(define identify-lambda
  (lambda (args ret-simple ret-opt ret-var)
    (cond ((null? args) (ret-simple '()))
          ((symbol? args) (ret-var args))
          (else (identify-lambda
                 (cdr args)
                 (lambda (s) (ret-simple `(,(car args) ,@s))) ;simple
                 (lambda (s . opt) (ret-opt `(,(car args) ,@s) opt)) ; opt
                 (lambda (var) (ret-opt `(,(car args)) var))) ; var
                 ))))

(define <lambda-rule-single-body-line>
  (pattern-rule
   `(lambda ,(? 'args) ,(? 'body))
   (lambda (args body)
     (let ((parsed-body (parse body)))
       (identify-lambda
        args
        (lambda (s) `(lambda-simple ,s ,parsed-body)) ; simple
        (lambda (s opt) `(lambda-opt ,s ,@opt ,parsed-body)) ; opt
        (lambda (var) `(lambda-var ,var ,parsed-body)) ; var
        )))))

(define <lambda-rule>
  (pattern-rule
   `(lambda ,(? 'args) ,(? 'body) . ,(? 'rest-body))
   (lambda (args body . rest-body)
     (let* ((begin-body (beginify (cons body rest-body)))
            (parsed-body (parse begin-body)))
       (identify-lambda
        args
        (lambda (s) `(lambda-simple ,s ,parsed-body)) ; simple
        (lambda (s opt) `(lambda-opt ,s ,@opt ,parsed-body)) ; opt
        (lambda (var) `(lambda-var ,var ,parsed-body)) ; var
        )))))

#|
(let ((parsed-body (parse body))
           (parsed-rest-body (map parse (car rest-body))))|#
























