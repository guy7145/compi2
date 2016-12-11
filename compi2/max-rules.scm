(define <define-rule>
  (pattern-rule
   `(define ,(? 'var) ,(? 'val) . ,(? 'val-rest))
   (lambda (var val . val-rest)
     (if (null? val-rest)
         `(def ,(parse var) ,(parse val))
         `(def ,(parse var) ,(parse (beginify (cons val (car val-rest)))))))))

(define proper-list?
  (lambda (s)
    (cond ((equal? s '()) #t)
          ((list? s) (proper-list? (cdr s)))
          (else #f))))

(define merge-bodies
  (lambda (body rest-body)
    (if (null? rest-body)
        body
        (beginify (cons body (car rest-body))))))

(define <define-mit-rule-var>
  (pattern-rule 
   `(define (,(? 'object) . ,(? 'var-arg)) ,(? 'body) . ,(? 'rest-body))
   (lambda (object var-arg body . rest-body)
     (let ((body (merge-bodies body rest-body)))
       `(def ,(parse object) ,(parse `(lambda ,var-arg ,body)))
   ))))

(define <define-mit-rule-simple-opt>
  (pattern-rule
   `(define (,(? 'object) ,(? 'args)) ,(? 'body))
   (lambda (object args body . rest-body) 
       `(def ,(parse object) ,(parse `(lambda ,args ,body))))))

(define <define-mit-rule>
  (compose-patterns
   <define-mit-rule-var>
   <define-mit-rule-simple-opt>
   ))

(define <assignment-rule>
  (pattern-rule
   `(set! ,(? 'var var?) ,(? 'val))
   (lambda (var val)
     `(set ,(parse var) ,(parse val)))))

(define <application-rule>
  (pattern-rule
   `(,(? 'foo not-reserved-word?) . ,(? 'args))
   (lambda (foo . args)
     `(applic ,(parse foo) (,@(map parse (car args)))))))


(define <and-rule-no-args>
  (pattern-rule
   `(and)
   (lambda () `(const ,#t))))

(define <and-rule>
  (pattern-rule
   `(and ,(? 'expr) . ,(? 'rest-exprs))
   (lambda (expr . rest-exprs)
     (letrec ((rest-exprs-unwrapped (car rest-exprs))
              (and->if (lambda (lst)
                         (if (null? (cdr lst))
                             (car lst)
                             (list 'if
                                   (car lst)
                                   (and->if (cdr lst))
                                   #f)))))
       (parse (and->if `(,expr ,@(car rest-exprs))))))))

(define <cond-rule>
  (pattern-rule
   `(cond ,(? 'expr) . ,(? 'exprs))
   (lambda (head tail)
     (letrec ((cond->if (lambda (lst)
                          (begin ;(display lst)
                            (if (null? lst)
                                (void)
                                (if (equal? "else" (caar lst))
                                    (cdr lst)
                                    `(if ,(caar lst)
                                         ,@(cdar lst)
                                         ,(cond->if (cdr lst)))))))))
       (parse (cond->if `(,head ,@tail)))))))